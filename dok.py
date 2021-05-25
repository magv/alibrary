#!/usr/bin/env python3

# Needed packages:
#   pip3 install mistletoe pygments pygments-mathematica

# Docommentation.

import glob
import io
import mistletoe
import mistletoe.span_token
import os
import os.path
import pygments
import pygments.formatters
import pygments.lexers
import pygments.token
import re
import sys

# Markdown (i.e. comment) parsing and rendering

# The way this was supposed to work is that in markdown_headers
# we would preparse Markdown, figure out the complete section
# structure, and fill in the xref. Then in markdown_render we
# would render the AST that was parsed here. Unfortunately no
# Python library actually provides usable AST for Markdown, which
# is why we will be parsing the text twice, and which is also why
# there is a duplication of code between DokPreRenderer and DokRenderer.

class CrossReferenceToken(mistletoe.span_token.SpanToken):
    pattern = re.compile(r"\[\[ *(.+?) *\]\]")
    def __init__(self, match):
        self.target = match.group(1)

def name_to_id(text):
    return "".join(word.capitalize() for word in re.split("\\W+", text))

class DokPreRenderer(mistletoe.HTMLRenderer):
    def __init__(self, url, xref):
        super().__init__(CrossReferenceToken)
        self._toc = []
        self._url = url
        self._xref = xref
    def render_heading(self, token):
        inner = self.render_inner(token)
        title = re.sub(r'<.+?>', '', inner)
        self._toc.append((token.level, title))
        self._xref[title] = (self._url, "#" + name_to_id(title))
        return ""

class DokRenderer(mistletoe.HTMLRenderer):
    def __init__(self, url, xref, toc, code_language="wl"):
        super().__init__(CrossReferenceToken)
        self._url = url
        self._xref = xref
        self._toc = toc
        self._code_language = code_language
        self._headers = []
    def render_heading(self, token):
        inner = self.render_inner(token)
        title = re.sub(r'<.+?>', '', inner)
        self._headers.append((token.level, title))
        return f'<h{token.level} id=\"{name_to_id(title)}\">{inner}</h{token.level}>'
    def render_cross_reference_token(self, token):
        xrefvalue = self._xref.get(token.target)
        if xrefvalue is not None:
            target, hash = xrefvalue
            target = relurl(target, self._url)
            inner = self.render_inner(token)
            return f"<a href=\"{target}{hash}\">{inner}</a>"
        elif token.target == "table of contents":
            with io.StringIO() as f:
                format_toc(f, [item for item in self._toc if item not in self._headers], self._xref, self._url)
                return f.getvalue()
        else:
            print(f"WARNING: missing x-ref: {token.target!r}")
            return "[[" + self.render_inner(token) + "]]"
    def render_block_code(self, token):
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
        inner = token.children[0].content
        lexer = pygments.lexers.get_lexer_by_name(token.language or self._code_language)
        tokens = pygments.lex(inner, lexer)
        with io.StringIO() as f:
            f.write("<pre class=\"doc\">")
            for tok, value in tokens:
                cls = towrap.get(tok, None)
                if tok == pygments.token.Token.Name.Variable and value in self._xref:
                    refurl, hash = self._xref[value]
                    refurl = relurl(refurl, self._url)
                    f.write(f"<a class=\"{cls}\" href=\"{refurl}{hash}\">{value}</a>")
                elif cls is not None:
                    f.write(f"<span class=\"{cls}\">{value}</span>")
                else:
                    f.write(value)
            f.write("</pre>\n")
            return f.getvalue()

def markdown_headers(text, url, xref):
    with DokPreRenderer(url, xref) as renderer:
        renderer.render(mistletoe.Document(text))
        return renderer._toc

def markdown_render(text, url, xref, toc):
    with DokRenderer(url, xref, toc) as renderer:
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

def preparse_mma(data, xref, url, toc):
    tokens = list(lexer_with_lineinfo(lexer_concat(pygments.lex(data, pygments.lexers.get_lexer_by_name("wl")))))
    #tokens = list(lexer_concat(pygments.lex(data, mathematica.MathematicaLexer())))
    tok_warn = (pygments.token.Token.Error,)
    tok_func = (pygments.token.Token.Name.Variable,)
    tok_comm = (pygments.token.Token.Comment,)
    for i in range(len(tokens)):
        tok, value, lin, col = tokens[i]
        if tok in tok_warn:
            print(f"Warning: syntax error at {url}:{lin}:{col}, token {value!r}")
        if col == 1 and tok in tok_comm:
            if i+2 < len(tokens):
                tok2, value2, _, _ = tokens[i+1]
                tok3, value3, _, _ = tokens[i+2]
                if value2 == "\n" and tok3 in tok_func:
                    if value3 not in xref:
                        xref[value3] = (url, "#" + value3)
                        toc.append((3, value3))
                        yield Token_Doc, f"<h3 id=\"{value3}\">{value3}[]</h3>\n"
                    else:
                        xurl, xhash = xref[value3]
                        print(f"Warning: name {value3!r} at {url}:{lin} was already defined in {xurl}")
        if col == 1 and tok in tok_comm:
            value = strip_comment(value)
            toc.extend(markdown_headers(value, url, xref))
            yield Token_Doc, value
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

def format_toc(f, toc, xref, url):
    level0 = min(lvl for lvl, title in toc) - 1
    f.write("<nav>\n")
    level = level0
    for lvl, title in toc:
        if level < lvl:
            while level < lvl:
                f.write("<ul><li>\n")
                level += 1
        elif level > lvl:
            while level > lvl:
                f.write("</li></ul>\n")
                level -= 1
            f.write("</li><li>\n")
        else:
            f.write("</li><li>\n")
        refurl, hash = xref[title]
        refurl = relurl(refurl, url)
        f.write(f" <a href=\"{refurl}{hash}\">{title}</a>\n")
    while level > level0:
        f.write("</li></ul>\n")
        level -= 1
    f.write("</nav>\n")

def format_mma(f, xref, url, tokens, toc):
    title = toc[0][1] if toc else None
    f.write(HTML_HEAD.format(baseprefix=relurl("", url), title=title))
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
            if lastmode is False: f.write(f"</pre>")
            if mode is False: f.write(f"<pre>")
            lastmode = mode
        if mode is True:
            f.write(markdown_render(value, url, xref, toc))
        else:
            cls = towrap.get(tok, None)
            if tok == pygments.token.Token.Name.Variable:
                if value in xref:
                    refurl, hash = xref[value]
                    refurl = relurl(refurl, url)
                    f.write(f"<a class=\"{cls}\" href=\"{refurl}{hash}\">{value}</a>")
                    continue
            if tok == pygments.token.Token.Name.Builtin:
                f.write(f"<a class=\"{cls}\" href=\"https://reference.wolfram.com/language/ref/{value}.html\" rel=\"nofollow\">{value}</a>")
                continue
            if cls is not None:
                f.write(f"<span class=\"{cls}\">{value}</span>")
            else:
                f.write(value)
    f.write(HTML_FOOT)

MMA = (preparse_mma, format_mma)

# Parsing/formatting: Markdown

def preparse_md(data, xref, url, toc):
    toc.extend(markdown_headers(data, url, xref))
    return [data]

def format_md(f, xref, url, data, toc):
    title = toc[0][1] if toc else None
    f.write(HTML_HEAD.format(baseprefix=relurl("", url), title=title))
    f.write(markdown_render(data[0], url, xref, toc))
    f.write(HTML_FOOT)

MD = (preparse_md, format_md)

# Main

HTML_HEAD = """\
<!DOCTYPE html>
<html lang="en">
 <head>
  <meta charset="UTF-8">
  <title>{title}</title>
  <meta name="viewport" content="width=device-width">
  <link rel="stylesheet" href="{baseprefix}style.css">
  <link rel="icon" href="{baseprefix}favicon.svg" type="image/svg+xml">
 </head>
 <body>
"""

HTML_FOOT = """\
  </body>
</html>
"""

STYLE_CSS = """\
html { background: white; max-width: 800px; margin: auto; }
html { font-family: "Charter",serif; font-size: 18px; hyphens: auto; text-align: justify; line-height: 1.2; }
body { margin: 0 10px; }
h1:first-child { margin-top: 0px; }
h1,h2,h3 { margin-top: 36px; margin-bottom: 12px; }
pre,p,hr { margin-top: 0px; margin-bottom: 18px; }
h3 { font-family: "Fira Mono",monospace; }
a { text-decoration: none; }
a:hover, a:focus { text-decoration: underline; }
pre, code { font-family: "Fira Mono",monospace; }
code { font-size: 90%; }
pre, pre code { font-size: 14px; }
.tc { color: #969896; }
.tl { color: #005cc5; }
.ts { color: #032f62; }
.tn, .tnv, .tnvc { }
.tnb { color: #d73a49; }
.tne { color: red; }
.tnt { color: #0c9a9a; }
pre { overflow-x: auto; }
pre { padding-left: 0.5em; background: #efeef0; border-left: 0.5em solid #e0e0e8; }
pre.doc { margin-left: 2em; border-left-color: #d0e0e0; }
ul ul { text-align: left; }
ul ul li { display: inline; }
ul ul li:after { content: " * "; color: #557; }
ul ul li:last-child:after { content: ""; }
hr { border: 2px dashed #efeef0; }
@media screen and (prefers-color-scheme: dark) {
 html { background: black; color: white; }
 pre { background: #111; border-left-color: #334; }
 pre.doc { border-left-color: #433; }
.ts { color: #234f82; }
}
"""

FAVICON_SVG = """\
<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 10 10" height="10mm" width="10mm">
 <path style="fill:none;stroke:#ef6234;stroke-width:1.5;stroke-linejoin:bevel" d="M 1,9 5,1 9,9" />
 <path style="fill:none;stroke:#6b75ca;stroke-width:1.0" d="m 2.5,6 c 4,0 3.5,2 2.5,2 -1,0 -1.5,-2 2.5,-2" />
 <circle style="fill:#222;stroke:none" cx="2.5" cy="6" r="1" />
 <circle style="fill:#222;stroke:none" cx="7.5" cy="6" r="1" />
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
    fulltoc = []
    for root, dirnames, filenames in os.walk(srcdir):
        for filename in filenames:
            fullname = os.path.join(root, filename)
            relpath = os.path.relpath(fullname, srcdir)
            for pat, tmpl, (preparse, format) in config:
                reldstpath = sub_pattern(pat, tmpl, relpath)
                if reldstpath is not None:
                    url = reldstpath if not reldstpath.endswith("index.html") else \
                          os.path.dirname(reldstpath)
                    print(f"read {relpath} -> {reldstpath}")
                    xref[relpath] = (url, "")
                    with open(fullname, "r") as f:
                        text = f.read()
                    toc = []
                    data = list(preparse(text, xref, url, toc))
                    fulltoc.extend(toc)
                    files.append((reldstpath, format, url, data, toc))
                    break

    for reldstpath, format, url, data, toc in files:
        dstpath = os.path.join(dstdir, reldstpath)
        dirname = os.path.dirname(dstpath)
        if dirname and not os.path.exists(dirname):
            print("mkdir", dirname)
            os.mkdir(dirname)
        print(f"format {dstpath} ({url})")
        with open(dstpath, "w") as f:
            format(f, xref, url, data, toc)

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
    create("toc.html", "".join([
        f"<h{lvl}>{name}</h{lvl}>\n" if lvl < 4 else \
        f"<span>{name}</span> * "
        for lvl, name in fulltoc
    ]))
