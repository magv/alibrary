(* # Displaying diagrams
 * 
 * Lets say we've generated some diagrams with e.g. [[mkdia.py]],
 * like so:
 *)

SetOptions[$Output, PageWidth -> 65];
$HistoryLength = 2;

Get["alibrary.m"];
Get[$Apath <> "/amodel-qcd.m"];

SafeRun[$Apath, "/mkdia.py dia-A-A-2.m"];
diagrams = SafeGet[MkString["dia-A-A-2.m"]];

(* Normally the diagrams would now be passed into [[Amplitude]]
 * to apply Feyman rules to them; there are however two reasons
 * to additionally convert them into a viewable form: to debug
 * the Feynman rules and to prepare publications.
 *
 * # Quick & dirty diagram viewing
 *
 * There are two ways to view the obtained diagrams. One way is by
 * producing `.dot` files for them, using `graphviz` to convert
 * those into PDFs, and importing the PDFs into Mathematica.
 * This achieves a quick and dirty view of the diagram, suitable
 * for debugging:
 *)

diagrams // Map[DiagramViz]

(* Note that the result of [[DiagramViz]] is a `Graphics[]`
 * object, and the Mathematica GUI frontend is needed to see
 * it.
 *
 * # Exporting diagrams to LaTeX
 *
 * The other objective is to prepare publication-quality LaTeX
 * renderings of the diagrams. To that end, we have found it best
 * to use the [TikZ] library in combination with the [TikZiT]
 * editor. To use this combination, first export any diagram to
 * a `.tikz` (LaTeX) file:
 *
 * [tikz]: https://github.com/pgf-tikz/pgf
 * [tikzit]: https://tikzit.github.io
 *)

MkFile["diagram.tikz", DiagramToTikZ[diagrams[[1]]]];

(* Then edit it with TikZiT by running `tikzit diagram.tikz`.
 * Make sure to load the `all.tiksztyles` style file that comes
 * with alibrary to be able to preview the diagrams insize TikZiT.
 *
 * Once you're done, you can include `diagram.tikz` into any
 * LaTeX source (in text or in a formula) to get a picture of
 * the diagram. A minimal example would be:
 *
 *     \documentclass{article}
 *     \input{all.tikzdefs}
 *     \input{all.tikzstyles}
 *     \begin{document}
 *       \input{diagram.tikz}
 *     \end{document}
 *
 * Note that both `all.tikzdefs` and `all.tikzstyles` are required
 * for this to work.
 *
 * The result might looks like this:
 *
 * $$
 *  \begin{tikzpicture}
 *    \begin{pgfonlayer}{nodelayer}
 *      \node [style=none] (-1) at (-1.5, 0) {};
 *      \node [style=none] (-2) at (1.5, 0) {};
 *      \node [style=dot] (1) at (-1, 0) {};
 *      \node [style=dot] (2) at (1, 0) {};
 *      \node [style=dot] (3) at (0, 1) {};
 *      \node [style=dot] (4) at (0, -1) {};
 *    \end{pgfonlayer}
 *    \begin{pgfonlayer}{edgelayer}
 *      \draw [style=incoming vector] (-1.center) to (1);
 *      \draw [style=outgoing vector] (2) to (-2.center);
 *      \draw [style=fermion] (3) to (1);
 *      \draw [style=fermion] (1) to (4);
 *      \draw [style=fermion] (2) to (3);
 *      \draw [style=fermion] (4) to (2);
 *      \draw [style=gluon] (4) to (3);
 *    \end{pgfonlayer}
 *  \end{tikzpicture}
 * $$
 *)
