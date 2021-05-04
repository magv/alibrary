#!/usr/bin/env python3

import sys
sys.path = [p for p in sys.path if ".local" in p] + [p for p in sys.path if ".local" not in p]

# Docommentation.

import mistletoe
import mistletoe.span_token
import pygments
import pygments.formatters
import pygments.lexers
import pygments.token
import glob
import os.path
import os
import io
import mathematica # pygments-mathematica
import re

# Markdown (i.e. comment) parsing and rendering

class CrossReferenceToken(mistletoe.span_token.SpanToken):
    pattern = re.compile(r"\[\[ *(.+?) *\]\]")
    def __init__(self, match):
        self.target = match.group(1)

class TocRenderer(mistletoe.HTMLRenderer):
    def __init__(self, *extras):
        super().__init__(CrossReferenceToken, *extras)
        self._toc = []
    def render_heading(self, token):
        rendered = super().render_heading(token)
        content = re.sub(r'<.+?>', '', rendered)
        self._toc.append((token.level, content))
        return rendered

class DokRenderer(mistletoe.HTMLRenderer):
    def __init__(self, xref={}):
        self._xref = xref
        super().__init__(CrossReferenceToken)
    def render_cross_reference_token(self, token):
        template = '<a href="{target}">{inner}</a>'
        #target = mistletoe.html_renderer.quote(token.target)
        print("XREF", token.target)
        target = self._xref.get(token.target, "??")
        inner = self.render_inner(token)
        return template.format(target=target, inner=inner)

def markdown_headers(text):
    with TocRenderer() as renderer:
        renderer.render(mistletoe.Document(text))
        return renderer._toc

def markdown_render(text, xref={}):
    with DokRenderer(xref=xref) as renderer:
        return renderer.render(mistletoe.Document(text))

# Parsing/formatting: Mathematica

Token_Doc = 1

def lexer_concat(lexer):
    prevtok = None
    values = []
    for tok, value in lexer:
        if tok == prevtok:
            values.append(value)
        else:
            if values:
                yield prevtok, "".join(values)
            prevtok = tok
            values = [value]
    if values:
        yield prevtok, "".join(values)

def lexer_with_lineinfo(lexer):
    line = 1
    column = 1
    for tok, value in lexer:
        yield tok, value, line, column
        nnl = value.count("\n")
        if nnl:
            line += nnl
            column = len(value) - value.rindex("\n")
        else:
            column += len(value)

def preparse_mma(data, xref, url):
    tokens = list(lexer_with_lineinfo(lexer_concat(pygments.lex(data, mathematica.MathematicaLexer()))))
    #tokens = list(lexer_concat(pygments.lex(data, mathematica.MathematicaLexer())))
    tok_warn = (pygments.token.Token.Error,)
    tok_func = (pygments.token.Token.Name.Variable,)
    tok_comm = (pygments.token.Token.Comment,)
    for i in range(len(tokens)):
        tok, value, lin, col = tokens[i]
        if col == 1 and tok in tok_comm:
            if i+2 < len(tokens):
                tok2, value2, _, _ = tokens[i+1]
                tok3, value3, _, _ = tokens[i+2]
                if value2 == "\n" and tok3 in tok_func:
                    if tok3 not in xref:
                        xref[value3] = (url, value3)
                        yield Token_Doc, f"<h4 id=\"{value3}\">{value3}[]</h4>\n"
                    else:
                        print(f"Already defined: {value3}")
        if col == 1 and tok in tok_comm:
            yield Token_Doc, value
        #if tok in tok_warn:
        #    print(f"Warning: syntax error in {filename!r} at {lin}:{col}: {value!r}")
        else:
            yield tok, value

def strip_comment(value):
    if value.startswith("(*"): value = value[2:]
    if value.endswith("*)"): value = value[:-2]
    value = value.strip("\n")
    return "\n".join([
        line[3:] if line.startswith(" * ") else \
        line[2:] if line.startswith(" *") else \
        line
        for line in value.splitlines()
    ])

def relurl(url, baseurl):
    url = url.split("/")
    baseurl = baseurl.split("/")
    n = 0
    while n < len(url) and n < len(baseurl) and url[n] == baseurl[n]: n += 1
    if url[-1] == "index.html": url = url[:-1] + [""]
    return "/".join([".."] * (len(baseurl) - n - 1) + url[n:])

def format_mma(f, xref, url, tokens):
    f.write(HTML_HEAD.format(baseprefix=relurl("", url)))
    towrap = {
        pygments.token.Token.Comment: "tc",
        pygments.token.Token.Literal.Number: "tl",
        pygments.token.Token.Literal.Number.Float: "tc",
        pygments.token.Token.Literal.Number.Integer: "tc",
        pygments.token.Token.Literal.String: "ts",
        pygments.token.Token.Name: "tn",
        pygments.token.Token.Name.Builtin: "tnb",
        pygments.token.Token.Name.Exception: "tne",
        pygments.token.Token.Name.Tag: "tnt",
        pygments.token.Token.Name.Variable: "tnv",
        pygments.token.Token.Name.Variable.Class: "tnvc"
    }
    lastmode = None
    for tok, value in tokens:
        mode = tok is Token_Doc#pygments.token.Token.Comment
        if lastmode is not mode:
            if lastmode is True: f.write("</div>")
            elif lastmode is False: f.write(f"</pre>")
            if mode is True: f.write("<div class=\"text\">\n")
            elif mode is False: f.write(f"<pre>")
            lastmode = mode
        if mode is True:
            f.write(markdown_render(strip_comment(value), xref=xref))
        else:
            cls = towrap.get(tok, None)
            if tok == pygments.token.Token.Name.Variable and value in xref:
                refurl, hash = xref[value]
                refurl = relurl(refurl, url)
                f.write(f"<a class=\"{cls}\" href=\"{refurl}#{hash}\">{value}</a>")
            #if tok == pygments.token.Token.Name:
            elif cls is not None:
                f.write(f"<span class=\"{cls}\">{value}</span>")
            else:
                f.write(value)
    f.write(HTML_FOOT)

MMA = (preparse_mma, format_mma)

# Parsing/formatting: Markdown

def preparse_md(data, xref, url):
    return [data]

def format_md(f, xref, url, data):
    f.write(HTML_HEAD.format(baseprefix=relurl("", url)))
    f.write(markdown_render(data[0], xref=xref))
    f.write(HTML_FOOT)

MD = (preparse_md, format_md)

# Main

HTML_HEAD = """\
<!DOCTYPE html>
<html lang="en">
 <head>
  <meta charset="utf-8">
  <link rel="stylesheet" href="{baseprefix}style.css">
  <link rel="icon" type="image/svg+xml" href="{baseprefix}favicon.svg">
 </head>
 <body>
"""

HTML_FOOT = """\
  </body>
</html>
"""

STYLE_CSS = """\
html { font-family: "Charter",serif; font-size: 18px; hyphens: auto; text-align: justify; line-height: 1.2; }
a { text-decoration: none; }
a:hover, a:focus { text-decoration: underline; }
pre, code { font-family: "Fira Mono",monospace; font-size: 90%; }
html{ background: white; max-width: 780px; margin: auto; }
h4 { font-family: "Fira Mono",mononspace; margin-bottom: 0; margin-top: 2em; }
.tc { color: #969896; }
.tl { color: #005cc5; }
.ts { color: #032f62; }
.tn, .tnv, .tnvc { }
.tnb { color: #d73a49; }
.tne { color: red; }
.tnt { color: #0c9a9a; }
pre { overflow-x: auto; }
pre { padding-left: 1.5em; background: #efeef0; border-left: 0.5em solid #e0e0e8; }
"""

FAVICON_SVG = """\
<svg
  xmlns="http://www.w3.org/2000/svg"
  viewBox="0 0 10 10"
  height="10mm"
  width="10mm">
 <g style="fill:none;stroke-width:1;stroke-linecap:butt;stroke-opacity:1">
  <path d="m 1,9 c 0,-2 2,0 2,-2" style="stroke:#666" />
  <path d="m 9,1 c 0,2 -2,0 -2,2" style="stroke:#666" />
  <path d="m 5,8 c -1.5,0 -3,-1.5 -3,-3 0,-1.5 1.5,-3 3,-3 1.5,0 3,1.5 3,3 0,1.5 -1.5,3 -3,3 z" style="stroke:#b02427;" />
 </g>
</svg>
"""

if __name__ == "__main__":

    import getopt

    def usage(file, code):
        print(f"usage: {sys.argv[0]} src-dir [dst-dir]", file=file)
        exit(0)

    opts, args = getopt.gnu_getopt(sys.argv[1:], "h")
    for opt, val in opts:
        if opt == "-h": usage(sys.stdout, 0)
    if len(args) == 1:
        srcdir = args[0]
        dstdir = "."
    elif len(args) == 2:
        srcdir = args[0]
        dstdir = args[1]
    else:
        usage(sys.stderr, 1)

    def pattern(pat):
        return re.compile("(.*)".join(re.escape(p) for p in pat.split("*")), re.DOTALL)

    def sub_pattern(pat, tmpl, string):
        m = pat.fullmatch(string)
        return None if m is None else tmpl.format(string, *m.groups())

    config = [
        (pattern(r"*.m"), "{1}.html", MMA),
        (pattern(r"README.md"), "index.html", MD),
        (pattern(r"*/README.md"), "{1}/index.html", MD),
        (pattern(r"*.md"), "{1}.html", MD)
    ]

    xref = {}
    files = []
    for root, dirnames, filenames in os.walk(srcdir):
        for filename in filenames:
            fullname = os.path.join(root, filename)
            relpath = os.path.relpath(fullname, srcdir)
            for pat, tmpl, (preparse, postparse) in config:
                reldstpath = sub_pattern(pat, tmpl, relpath)
                if reldstpath is not None:
                    url = reldstpath if not reldstpath.endswith("index.html") else \
                          os.path.dirname(reldstpath)
                    print(f"read {relpath} -> {reldstpath}")
                    with open(fullname, "r") as f:
                        text = f.read()
                    data = list(preparse(text, xref, url))
                    files.append((reldstpath, postparse, url, data))
                    break

    for reldstpath, postparse, url, data in files:
        dstpath = os.path.join(dstdir, reldstpath)
        dirname = os.path.dirname(dstpath)
        if dirname and not os.path.exists(dirname):
            print("mkdir", dirname)
            os.mkdir(dirname)
        print(f"format {dstpath} ({url})")
        with open(dstpath, "w") as f:
            postparse(f, xref, url, data)

    def create(relpath, content):
        dstpath = os.path.join(dstdir, relpath)
        dirname = os.path.dirname(dstpath)
        if not os.path.exists(dirname):
            print("mkdir", dirname)
            os.mkdir(dirname)
        print(f"create {dstpath}")
        with open(dstpath, "w") as f:
            f.write(content)

    create("style.css", STYLE_CSS)
    create("favicon.svg", FAVICON_SVG)
