(* Misc utils *)

(* Remove terms from a series that are not free of a given pattern. *)
SeriesDropTerms[HoldPattern[s : SeriesData[x_, x0_, items_List, n1_, n2_, d_]], pattern_] :=
Module[{idx, nterms},
  idx = items // MapIndexed[If[FreeQ[#1, pattern], Nothing, #2[[1]]] &];
  If[idx === {},
    s,
    nterms = Min[idx] - 1;
    SeriesData[x, x0, items[[;; nterms]], n1, n1 + nterms, d]
  ]
]
SeriesDropTerms[l_List, pattern_] := Map[SeriesDropTerms[#, pattern] &, l]
SeriesDropTerms[pattern_] := SeriesDropTerms[#, pattern] &

(* Cached["fib123.m", Fibonacci[123]] will load from "fib123.m",
 * or evaluate the expression, and save it.
 *)
SetAttributes[Cached, {HoldAll}];
Cached[key_, body_] := Module[{filename = MkString[key], result},
  If[FileExistsQ[filename],
   Print["Loading cached results from ", filename];
   SafeGet[filename],
   result = body; Put[result, filename]; result
   ]
  ]

(* Run a (shell) command, print its output into the GUI.
 *)
Runx[command__] := Module[{file, s},
  file = OpenRead[MkString["!", Riffle[{command}, " "]]];
  While[True,
    s = ReadString[file];
    If[s === EndOfFile, Break[]];
    Print[s];
  ];
  Close[file];
]

(* Factor out an overall prefactor; faster than the full Factor.
 *)
FasterFactor[ex_List] := Map[FasterFactor, ex]
FasterFactor[ex_Times] := Map[FasterFactor, ex]
FasterFactor[ex_] := Module[{gcd, terms},
  terms = ex // Expand // Terms;
  If[Length[terms] === 0,
   0
   ,
   gcd = terms[[1]];
   terms[[2 ;;]] // Map[(gcd = PolynomialGCD[#, gcd];) &];
   terms // Map[#/gcd &] // Apply[Plus] // #*gcd &
   ]
  ]

FastMatrixRank[mx_] := Module[{vars, mnx},
  vars = mx // CaseUnion[_Symbol];
  Table[
    mxn = mx /. Association[MapThread[Rule, {vars, RandomInteger[{2^20, 2^30}, Length[vars]]}]];
    MatrixRank[mxn, Modulus->RandomPrime[{2^30, 2^31-1}]]
    ,
    10
  ] // Max
];

(* Given a list of vectors, select a linearly independent subset,
 * return the indices of the vectors in it. *)
SelectAnyBasis[vectors_List] := Module[{dim, basisidx, basis, i, newbasis, rank},
  dim = Length[vectors[[1]]];
  basis = {};
  basisidx = {};
  Do[
    newbasis = Append[basis, vectors[[i]]];
    rank = newbasis // FastMatrixRank;
    If[rank =!= Length[newbasis], Continue[]];
    basis = newbasis;
    basisidx = Append[basisidx, i];
    If[rank === dim, Break[]];
    ,
    {i, Length[vectors]}];
  basisidx
]

(* XGraphs *)

Needs["GraphUtilities`"];
XGraph[edgelist_List] :=
 Module[{Edge, edges = {}, styles = {}, e, x, edge, label, opt},
  Edge[Rule[a_, b_]] := a \[DirectedEdge] b;
  Edge[e : DirectedEdge[a_, b_]] := e;
  Edge[e : UndirectedEdge[a_, b_]] := e;
  Edge[e_] := Throw[{"Not an edge:", e}];
  Do[Replace[e, {{x_, style___} :> (edge = Edge[x];
        AppendTo[edges, edge];
        AppendTo[styles, edge -> {style}];), x_ :> (edge = Edge[x];
        AppendTo[edges, edge];
        AppendTo[styles, edge -> {}];)}];, {e, edgelist}];
  XGraph[edges,
   GatherBy[styles, First] // Map[#[[1, 1]] -> Map[#[[2]] &, #] &]]]
MakeBoxes[expr : XGraph[edges_List, opt_List],
  StandardForm | TraditionalForm] :=
 Block[{counter, e}, Do[counter[e] = 1, {e, edges}];
  ToBoxes[
   Interpretation[
    Graph[edges, ImageSize -> 300, GraphLayout -> "SpringEmbedding",
     EdgeShapeFunction ->(Module[{style, c, s, x, i},
         c = ++counter[#2];
         If[c > Length[#2 /. opt], counter[#2] = c = 1];
         style = (#2 /. opt)[[c]];
         Append[Table[Replace[s, {
             Text[x_] :> Text[x, LineScaledCoordinate[#1, 0.5]],
             Dots[x_] :>
              Table[{Black, Opacity[1.0],
                Disk[LineScaledCoordinate[#1, i/(x + 1) // N],
                 0.05]}, {i, x}]
             }], {s, style}], Arrow@#1]] &)], expr], StandardForm]]

(* Diagrams *)

DiagramId[Diagram[id_, f_, i_List, o_List, p_List, v_List]] := id

(* Diagram -> Graph *)

DiagramGraphEdges[Diagram[_, _, i_List, o_List, p_List, _]] := Join[
  p /. P[f_, fi1_, fi2_, vi1_, vi2_, mom_] :> II[vi1] <-> II[vi2],
  i /. F[f_, fi_, vi_, mom_] :> SS[fi] <-> II[vi],
  o /. F[f_, fi_, vi_, mom_] :> II[vi] <-> EE[fi]
]
DiagramGraph[d_Diagram] := DiagramGraphEdges[d] // Graph
DiagramXGraph[Diagram[_, _, i_List, o_List, p_List, _]] := Join[
   i /. F[f_, fi_, vi_, mom_] :> {IN[fi] -> vi,
      Text[MkString[f, "(", mom, ")"] // StringReplace[" " -> ""]],
      Gray, Thin},
   o /. F[f_, fi_, vi_, mom_] :> {vi -> OO[fi],
      Text[MkString[f, "(", mom, ")"] // StringReplace[" " -> ""]],
      Gray, Thin},
   p /. P[f_, fi1_, fi2_, vi1_, vi2_, mom_] :> {vi1 -> vi2,
      Text[MkString[f, "(", mom, ")"] // StringReplace[" " -> ""]],
      Switch[f, q, Thickness[0.01], _, {}]}
   ] // XGraph
DiagramToGraphviz[Diagram[id_, _, i_List, o_List, p_List, _]] := Module[{c, defc},
  c = <|
    "q" -> 6, "Q" -> 6,
    "t" -> 6, "T" -> 6,
    "g" -> 4,
    "c" -> 8, "C" -> 8,
    "O" -> 10, "H" -> 10, "A" -> 10, "Z" -> 10, "s" -> 10, "S" -> 10
  |>;
  defc = 12;
  MkString[
   "digraph {\n",
   " fontsize=12; margin=0;\n",
   " node [shape=circle width=0.1 color=black];\n",
   " edge [fontsize=8; colorscheme=paired12];\n",
   i /. F[f_, fi_, vi_, mom_] :> fi // Union // Map[{" ", #, " [width=0.05 color=gray];\n"} &],
   o /. F[f_, fi_, vi_, mom_] :> fi // Union // Map[{" ", #, " [width=0.05 color=gray];\n"} &],
   i /. F[f_, fi_, vi_, mom_] :> {
     " ", fi, " -> ", vi,
     " [label=\"", f, "(", mom // ToString // StringReplace[" " -> ""], ")\",color=", Lookup[c, f, defc] - 1, "];\n"},
   p /. P[f_, fi1_, fi2_, vi1_, vi2_, mom_] :> {
     " ", vi1, " -> ", vi2,
     " [label=\"", f, "(", mom // ToString // StringReplace[" " -> ""], ")\",color=", Lookup[c, f, defc], ",style=bold];\n"},
   o /. F[f_, fi_, vi_, mom_] :> {
     " ", vi, " -> ", fi,
     " [label=\"", f, "(", mom // ToString // StringReplace[" " -> ""], ")\",color=", Lookup[c, f, defc] - 1, "];\n"},
   "}\n"
   ]
]
DiagramToGraphviz[CutDiagram[
  Diagram[id1_, _, i1_List, o1_List, p1_List, v1_List], Diagram[id2_, _, i2_List, o2_List, p2_List, v2_List]
]] := Module[{c, defc},
  c = <|
    "q" -> 6, "Q" -> 6,
    "t" -> 6, "T" -> 6,
    "g" -> 4,
    "c" -> 8, "C" -> 8,
    "O" -> 10, "H" -> 10, "A" -> 10, "Z" -> 10, "s" -> 10, "S" -> 10
  |>;
  defc = 12;
  MkString[
   "digraph {\n",
   " fontsize=12; margin=0; label_scheme=2;\n",
   " node [shape=circle width=0.1 color=black];\n",
   " edge [fontsize=8; colorscheme=paired12];\n",
   i1 /. F[f_, fi_, vi_, mom_] :> fi // Union // Map[{" ", #, "01 [fontsize=10 width=0.05 color=gray label=\"", #, "\"];\n"} &],
   i2 /. F[f_, fi_, vi_, mom_] :> fi // Union // Map[{" ", #, "02 [fontsize=10 width=0.05 color=gray label=\"", #, "'\"];\n"} &],
   v1 /. V[id_, ___] :> id // Union // Map[{" ", #, "01 [label=\"", #, "\"];\n"} &],
   v2 /. V[id_, ___] :> id // Union // Map[{" ", #, "02 [label=\"", #, "'\"];\n"} &],
   o1 /. F[f_, fi_, vi_, mom_] :> fi // Union // Map[{" \"|edgelabel|", -#, "00\" [fontsize=10 width=0.05 shape=square color=gray label=\"", #, "\"];\n"} &],
   i1 /. F[f_, fi_, vi_, mom_] :> {
     " ", fi, "01 -> ", vi, "01",
     "[label=\"", f, "(", mom // ToString // StringReplace[" " -> ""], ")\",color=", Lookup[c, f, defc] - 1, "];\n"
   },
   i2 /. F[f_, fi_, vi_, mom_] :> {
     " ", fi, "02 -> ", vi, "02",
     "[label=\"", f, "(", mom // ToString // StringReplace[" " -> ""], ")\",color=", Lookup[c, f, defc] - 1, "];\n"
   },
   p1 /. P[f_, fi1_, fi2_, vi1_, vi2_, mom_] :> {
     " ", vi1, "01 -> ", vi2, "01",
     " [label=\"", f, "(", mom // ToString // StringReplace[" " -> ""], ")\",color=", Lookup[c, f, defc], ",style=bold];\n"
   },
   p2 /. P[f_, fi1_, fi2_, vi1_, vi2_, mom_] :> {
     " ", vi1, "02 -> ", vi2, "02",
     " [label=\"", f, "(", mom // ToString // StringReplace[" " -> ""], ")\",color=", Lookup[c, f, defc], ",style=bold];\n"
   },
   o1 /. F[f_, fi_, vi_, mom_] :> {
     " ", vi, "01 -> \"|edgelabel|", -fi, "00\"",
     " [label=\"", f, "(", mom // ToString // StringReplace[" " -> ""], ")\",color=", Lookup[c, f, defc] - 1, "];\n"
   },
   o2 /. F[f_, fi_, vi_, mom_] :> {
     " ", vi, "02 -> \"|edgelabel|", -fi, "00\"",
     " [label=\"", f, "(", mom // ToString // StringReplace[" " -> ""], ")\",color=", Lookup[c, f, defc] - 1, "];\n"
   },
   "}\n"
   ]
]
CutDiagramGraph[CutDiagram[d1_Diagram, d2_Diagram]] := Module[{e1, e2, x},
  e1 = DiagramGraphEdges[d1];
  e2 = DiagramGraphEdges[d2];
  Join[e1 // DeleteCases[_ <-> _EE],
   e2 /. SS -> SS2 /. II -> II2 /. Cases[e1, (i_ <-> e_EE) :> ((x_ <-> e) :> Style[x <-> i, Dashed])]]
]
DiagramViz[d:(_Diagram|_CutDiagram)] := Module[{tmp, pdf, result},
  tmp = MkTemp["diavis", ".gv"];
  pdf = tmp <> ".pdf";
  Export[tmp, DiagramToGraphviz[d], "String"];
  Run["neato -Tpdf -o", pdf, tmp];
  result = Import[pdf];
  DeleteFile[{tmp, pdf}];
  result // First
]

DiagramToTikz[Diagram[id_, _, i_List, o_List, p_List, v_List]] := Module[{es, ni, no, scale},
  es = {
    "q"|"u"|"d"|"c"|"s"|"t"|"b"|"x"|"y" -> "fermion",
    "W"|"A" -> "photon",
    "g" -> "gluon",
    "h" -> "scalar",
    "c" -> "ghost",
    _ -> "edge"
  };
  ni = Length[i];
  no = Length[o];
  scale = Max[ni,no,4];
  MkString[
    "\\begin{tikzpicture}\n",
    "\t\\begin{pgfonlayer}{nodelayer}\n",
    i /. F[f_, fi_, vi_, mom_] :> {"\t\t\\node [style=blank] (", fi, ") at (0, ", scale(-1-fi)/(2 ni)//Round, ") {$", f, "(", mom, ")$};\n"},
    o /. F[f_, fi_, vi_, mom_] :> {"\t\t\\node [style=blank] (", fi, ") at (", scale, ", ", scale(-2-fi)/(2 no)//Round, ") {$", f, "(", mom, ")$};\n"},
    v /. V[vi_, __] :> {"\t\t\\node [style=dot] (", vi, ") at (", 1+Random[]*(scale-2)//Round, ", ", 1+Random[]*(scale-2)//Round, ") {};\n"},
    "\t\\end{pgfonlayer}\n",
    "\t\\begin{pgfonlayer}{edgelayer}\n",
    i /. F[f_, fi_, vi_, mom_] :> {"\t\t\\draw [style=incoming edge] (", fi, ") to (", vi, ");\n"},
    o /. F[f_, fi_, vi_, mom_] :> {"\t\t\\draw [style=outgoing edge] (", vi, ") to (", fi, ");\n"},
    p /. P[f_, fi1_, fi2_, vi1_, vi2_, mom_] :> {"\t\t\\draw [style=", f /. es, "] (", vi1, ") to (", vi2, ");\n"},
    "\t\\end{pgfonlayer}\n",
    "\\end{tikzpicture}\n"
  ]
]

(* Amplitudes & Feynman Rules *)

AmpConjugateMomenta[ex_] := ex \
  /. l1 -> r1 /. l2 -> r2 /. l3 -> r3 /. l4 -> r4 /. l5 -> r5 \
  /. l6 -> r6 /. l7 -> r7 /. l8 -> r8 /. l9 -> r9 /. l10 -> r10
AmpConjugate[ex_] := (ex
  (*
  (* Spin indices *must* all be internal, because there is a
   * gammachain[spinor, spn[-1], X[-1]] at each external line of
   * fermions: here spn[-1] must be renamed, and X[-1] mustn't.
   *)
  /. (idx : spn)[i_] :> idx[2000 + i]
  (* The rest of the indices are external if negative, and
   * internal otherwise.
   *)
  /. (idx : flv | lor | fun | adj)[i_] :> idx[If[TrueQ[i >= 0], 5000 + i, i]]
  *)
  /. (idx : flv | lor | fun | adj | spn)[i_] :> idx[If[TrueQ[i < 0], -5000 + i, 5000 + i]]
  /. Complex[re_, im_] :> Complex[re, -im]
  /. (chain : gammachain | colorT | colorf)[m___, i_, j_] :> chain[Sequence @@ Reverse[{m}], j, i]
  // AmpConjugateMomenta)
FormatAmp[ex_] := ReplaceRepeated[ex, {
   delta[a_, b_] :> Subscript["\[Delta]", Row[{a, b}]],
   deltaf[a_, b_] :> Subscript["\[Delta]", Row[{a, b}]],
   lor[n__] :> Subscript["\[Mu]", n],
   adj[n__] :> Subscript["a", n],
   fun[n__] :> Subscript["i", n],
   flv[n__] :> Subscript["f", n],
   spn[n__] :> Subscript["s", n],
   momentum[p_, idx_] :> Superscript[OverVector[p], idx],
   dot[p1_, p2_] :>
    Row[{"(", OverVector[p1], "\[CenterDot]", OverVector[p2], ")"}],
   gammachain[g___, a_, b_] :>
    Subscript[Row[{"(", g, ")"}], Row[{a, b}]],
   gammatrace[g___] :> Row[{"Tr(", g, ")"}],
   gamma[mu_] :> Superscript["\[Gamma]", mu],
   slash[mom_] :> OverVector[mom],
   polarization[p_, idx_, I] :>
    Row[{Superscript["\[Epsilon]", idx], "(", p, ")"}],
   polarization[p_, idx_, -I] :>
    Row[{Superscript["\[Epsilon]", Row[{"*", idx}]], "(", p, ")"}],
   colorf[a_, b_, c_] :> Superscript["f", Row[{a, b, c}]],
   colorT[a_, b_, c_] :> Subsuperscript["T", Row[{b, c}], a],
   spinor[p_, I] :> Style[u, Italic][p],
   spinor[p_, -I] :> OverBar[Style[u, Italic]][p],
   gs -> Subscript["g", "s"],
   p1 -> Subscript["p", "1"], p2 -> Subscript["p", "2"],
   p3 -> Subscript["p", "3"], p4 -> Subscript["p", "4"],
   q1 -> Subscript["q", "1"], q2 -> Subscript["q", "2"],
   q3 -> Subscript["q", "3"], q4 -> Subscript["q", "4"]
}]
PRAmp[ex_] := (Print[ex // FormatAmp]; ex)
deltaflv[a_, b_] := deltaf[flv[a], flv[b]]
deltaflvt[a_, b_] := deltaft[flv[a], flv[b]]
deltafun[a_, b_] := delta[fun[a], fun[b]]
deltaadj[a_, b_] := delta[adj[a], adj[b]]
deltalor[a_, b_] := delta[lor[a], lor[b]]

ClearAll[Amplitude];
(* Propagators *)
Amplitude[P["q", fi1_, fi2_, _, _, p_]] :=
  I deltaflv[fi1, fi2] deltafun[fi2, fi1] \
  gammachain[slash[p], spn[fi2], spn[fi1]] den[p]
Amplitude[P["t", fi1_, fi2_, _, _, p_]] :=
  I deltaflvt[fi1, fi2] deltafun[fi2, fi1] \
  (gammachain[slash[p], spn[fi2], spn[fi1]] + mt1 gammachain[spn[fi2], spn[fi1]]) den[p, mt2]
Amplitude[P["g", fi1_, fi2_, _, _, p_]] :=
  -I deltaadj[fi1, fi2] (deltalor[fi1, fi2] den[p] -
    Xi momentum[p, lor[fi1]] momentum[p, lor[fi2]] den[p]^2)
Amplitude[P["H", fi1_, fi2_, _, _, p_]] := I den[p]
Amplitude[P["s"|"S", fi1_, fi2_, _, _, p_]] := I den[p]
Amplitude[P["c", fi1_, fi2_, _, _, p_]] := I deltaadj[fi1, fi2] den[p]
(* Vertices *)
Amplitude[V[_, "gqQ", fi1_, p1_, fi2_, p2_, fi3_, p3_]] :=
  I gs deltaflv[fi2, fi3] gammachain[gamma[lor[fi1]], spn[fi3],
  spn[fi2]] colorT[adj[fi1], fun[fi3], fun[fi2]]
Amplitude[V[_, "gtT", fi1_, p1_, fi2_, p2_, fi3_, p3_]] :=
  I gs deltaflvt[fi2, fi3] gammachain[gamma[lor[fi1]], spn[fi3],
  spn[fi2]] colorT[adj[fi1], fun[fi3], fun[fi2]]
Amplitude[V[_, "gcC", fi1_, p1_, fi2_, p2_, fi3_, p3_]] :=
  -gs colorf[adj[fi1], adj[fi2], adj[fi3]] momentum[-p3, lor[fi1]]
Amplitude[V[_, "AqQ", fi1_, p1_, fi2_, p2_, fi3_, p3_]] :=
  (* ge *) I deltaflv[fi2, fi3] deltafun[fi3, fi2] chargeQ[flv[fi2]] \
  gammachain[gamma[lor[fi1]], spn[fi3], spn[fi2]]
Amplitude[V[_, "ZqQ", fi1_, p1_, fi2_, p2_, fi3_, p3_]] :=
  (* gz *) I deltaflv[fi2, fi3] deltafun[fi3, fi2] (
    chargeV[flv[fi2]] gammachain[gamma[lor[fi1]], spn[fi3], spn[fi2]] -
    (* gamma5[mu] == gamma[mu] gamma5 *)
    chargeA[flv[fi2]] gammachain[gamma5[lor[fi1]], spn[fi3], spn[fi2]]
  )
Amplitude[V[_, "ggg", fi1_, p1_, fi2_, p2_, fi3_, p3_]] :=
  gs colorf[adj[fi1], adj[fi2], adj[fi3]] (
    deltalor[fi1, fi2] momentum[p1 - p2, lor[fi3]] +
    deltalor[fi2, fi3] momentum[p2 - p3, lor[fi1]] +
    deltalor[fi3, fi1] momentum[p3 - p1, lor[fi2]]
  )
Amplitude[V[vi_, "gggg", fi1_, p1_, fi2_, p2_, fi3_, p3_, fi4_, p4_]] :=
  Module[{adjX = adj[1000 + vi]},
    -I gs^2 (
      colorf[adjX, adj[fi1], adj[fi2]] colorf[adjX, adj[fi3], adj[fi4]]
        (deltalor[fi1, fi3] deltalor[fi2, fi4] -
          deltalor[fi1, fi4] deltalor[fi2, fi3]) +
      colorf[adjX, adj[fi1], adj[fi3]] colorf[adjX, adj[fi4], adj[fi2]]
        (deltalor[fi1, fi4] deltalor[fi2, fi3] -
          deltalor[fi1, fi2] deltalor[fi3, fi4]) +
      colorf[adjX, adj[fi1], adj[fi4]] colorf[adjX, adj[fi2], adj[fi3]]
        (deltalor[fi1, fi2] deltalor[fi3, fi4] -
          deltalor[fi1, fi3] deltalor[fi2, fi4])
    )
  ]
Amplitude[V[_, "HtT", fi1_, p1_, fi2_, p2_, fi3_, p3_]] :=
  gH deltaflvt[fi2, fi3] deltafun[fi2, fi3] gammachain[spn[fi3], spn[fi2]]
Amplitude[V[_, "Hgg", fi1_, p1_, fi2_, p2_, fi3_, p3_]] :=
  (* gh *) I deltaadj[fi2, fi3] (
    deltalor[fi2, fi3] dot[p2, p3] - momentum[p2, lor[fi3]] momentum[p3, lor[fi2]]
  )
Amplitude[V[vi_, "Hggg", fi1_, p1_, fi2_, p2_, fi3_, p3_, fi4_, p4_]] :=
  (* gh *) Amplitude[V[vi, "ggg", fi2, p2, fi3, p3, fi4, p4]]
Amplitude[V[vi_, "Hgggg", fi1_, p1_, fi2_, p2_, fi3_, p3_, fi4_, p4_, fi5_, p5_]] :=
  (* gh *) Amplitude[V[vi, "gggg", fi2, p2, fi3, p3, fi4, p4, fi5, p5]]
Amplitude[V[vi_, "OqQ", fi1_, p1_, fi2_, p2_, fi3_, p3_]] := (
  FailUnless[IntegerQ[OPN]];
  If[OPN < 1, 0,
    deltaflv[fi2, fi3]
      deltafun[fi3, fi2]
      gammachain[slash[pDELTA], spn[fi3], spn[fi2]]
      dot[pDELTA, p3]^(OPN-1)
  ]
)
Amplitude[V[vi_, "OqQg", fi1_, p1_, fi2_, p2_, fi3_, p3_, fi4_, p4_]] := (
  FailUnless[IntegerQ[OPN]];
  If[OPN < 2, 0,
    gs deltaflv[fi2, fi3]
      momentum[pDELTA, lor[fi4]]
      colorT[adj[fi4], fun[fi3], fun[fi2]]
      gammachain[slash[pDELTA], spn[fi3], spn[fi2]]
      Sum[dot[pDELTA, p3]^(OPN-2-j) dot[pDELTA, (p2+p3+p4)-p2]^j, {j, 0, OPN-2}]
  ]
)
Amplitude[V[_, "sss", fi1_, p1_, fi2_, p2_, fi3_, p3_]] := I
Amplitude[V[_, "ssss", fi1_, p1_, fi2_, p2_, fi3_, p3_, fi4_, p4_]] := I
Amplitude[V[_, "sssss", fi1_, p1_, fi2_, p2_, fi3_, p3_, fi4_, p4_, fi5_, p5_]] := I
Amplitude[V[_, "SSS", fi1_, p1_, fi2_, p2_, fi3_, p3_]] := I
Amplitude[V[_, "SSSS", fi1_, p1_, fi2_, p2_, fi3_, p3_, fi4_, p4_]] := I
Amplitude[V[_, "SSSSS", fi1_, p1_, fi2_, p2_, fi3_, p3_, fi4_, p4_, fi5_, p5_]] := I
(*
(* Incoming Fields *)
AmplitudeIF[F["A"|"Z"|"g"|"q"|"Q", fi_, _, mom_]] := 1 (* Use a projector here instead *)
AmplitudeIF[F["H" | "c" | "C", fi_, _, mom_]] := 1
(*
AmplitudeIF[F["A"|"Z"|"g", fi_, _, mom_]] := polarization[mom, lor[fi], I]
AmplitudeIF[F["q", fi_, _, mom_]] := gammachain[spinor(*u*)[mom, I], spn[fi], X[fi]]
AmplitudeIF[F["Q", fi_, _, mom_]] := gammachain[spinor(*vbar*)[mom, -I], X[fi], spn[fi]]
*)
AmplitudeIF[F["O", fi_, _, mom_]] := 1
AmplitudeIF[x__] := Error["Don't know an incoming field amplitude for: ", x]
(* Outgoing Fields *)
AmplitudeOF[F["A"|"Z", fi_, _, mom_]] := 1 (* polarization[mom, lor[fi], -I] *)
AmplitudeOF[F["g", fi_, _, mom_]] := 1 (* polarization[mom, lor[fi], -I]*)
AmplitudeOF[F["H" | "c" | "C", fi_, _, mom_]] := 1
AmplitudeOF[F["q", fi_, _, mom_]] := gammachain[spinor(*ubar*)[mom, -I], X[fi], spn[fi]]
AmplitudeOF[F["Q", fi_, _, mom_]] := gammachain[spinor(*v*)[mom, I], spn[fi], X[fi]]
AmplitudeOF[F["O", fi_, _, mom_]] := 1
AmplitudeOF[x__] := Error["Don't know an outgoing field amplitude for: ", x]
*)
(* Diagrams *)
Amplitude[
  dia:Diagram[id_, factor_, ifields_List, ofields_List, propagators_List, vertices_List]] := Flatten[{
    DiagramSign[dia, "q"|"Q"|"c"|"C"|"t"|"T"],
    Abs[factor],
    propagators // Map[Amplitude],
    vertices // Map[Amplitude]
    (*ifields // Map[AmplitudeIF],
    ofields // Map[AmplitudeOF]*)
  }] // Apply[Times]
(* Default Case *)
Amplitude[x__] := Error["Don't know an amplitude for: ", x]
(* Cut Line Joins Between Outgoing Fields *)
ClearAll[CutAmplitudeGlue]
CutAmplitudeGlue[F[f:"H", fi_, _, mom_], F[f_, fi_, _, mom_]] := 1
CutAmplitudeGlue[F[f:"s"|"S", fi_, _, mom_], F[f_, fi_, _, mom_]] := 1
(*
CutAmplitudeGlue[F[f:"g", fi_, _, mom_], F[f_, fi_, _, mom_]] := (* -g_mn d_ab *)
  -delta[lor[fi], lor[fi] // AmpConjugate] delta[adj[fi], adj[fi] // AmpConjugate]
*)
(*
CutAmplitudeGlue[F[f:"g", fi_, _, mom_], F[f_, fi_, _, mom_]] := (*  d_ab (-g_mn + (pm pn)/p^2) *)
Module[{mu = lor[fi], nu = lor[fi] // AmpConjugate},
  delta[adj[fi], adj[fi] // AmpConjugate] (
    -delta[mu, nu] + Xi2 momentum[mom, mu] momentum[mom, nu] den[mom,0,cut]
  )
]
*)
CutAmplitudeGlue[F[f:"g", fi_, _, mom_], F[f_, fi_, _, mom_]] := (*  d_ab (-g_mn + (pm kn + pn km)/p.k; where k.k = 0 *)
Module[{mu = lor[fi], nu = lor[fi] // AmpConjugate, k = placeholder[mom]},
  delta[adj[fi], adj[fi] // AmpConjugate] (
    - delta[mu, nu]
    (*+ Xi2 momentum[mom, mu] momentum[mom, nu] den[mom,0,cut]*)
    (*+ Xi3 (momentum[mom, mu] momentum[k, nu] + momentum[mom, nu] momentum[k, mu]) 2 denplaceholder[mom]*)
    (* - Xi4 dot[k, k] momentum[mom, mu] momentum[mom, nu] 4 denplaceholder[mom]^2 *)
  )
]
(* The -1 here makes it so that the external ghost pair contributions
 * are subtracted. *)
CutAmplitudeGlue[F[f:"c", fi_, _, mom_], F[f_, fi_, _, mom_]] := (* -1 * d_ab *)
  -delta[adj[fi], adj[fi] // AmpConjugate]
CutAmplitudeGlue[F[f:"C", fi_, _, mom_], F[f_, fi_, _, mom_]] := (* d_ab *)
  delta[adj[fi], adj[fi] // AmpConjugate]
CutAmplitudeGlue[F[f:"Q", fi_, _, mom_], F[f_, fi_, _, mom_]] := (* p^slash d_ij d_f1f2 *)
  gammachain[slash[mom], spn[fi], spn[fi] // AmpConjugate] delta[fun[fi], fun[fi] // AmpConjugate] deltaf[flv[fi], flv[fi] // AmpConjugate]
CutAmplitudeGlue[F[f:"q", fi_, _, mom_], F[f_, fi_, _, mom_]] := (* p^slash d_ij d_f1f2 *)
  gammachain[slash[mom], spn[fi] // AmpConjugate, spn[fi]] delta[fun[fi], fun[fi] // AmpConjugate] deltaf[flv[fi], flv[fi] // AmpConjugate]
CutAmplitudeGlue[F[f:"T", fi_, _, mom_], F[f_, fi_, _, mom_]] := (* (p^slash - mt1) d_ij d_f1f2 *)
  (gammachain[slash[mom], spn[fi], spn[fi] // AmpConjugate] - mt1 gammachain[spn[fi], spn[fi] // AmpConjugate]) delta[fun[fi], fun[fi] // AmpConjugate] deltaft[flv[fi], flv[fi] // AmpConjugate]
CutAmplitudeGlue[F[f:"t", fi_, _, mom_], F[f_, fi_, _, mom_]] := (* (p^slash + mt1) d_ij d_f1f2 *)
  (gammachain[slash[mom], spn[fi] // AmpConjugate, spn[fi]] + mt1 gammachain[spn[fi] // AmpConjugate, spn[fi]]) delta[fun[fi], fun[fi] // AmpConjugate] deltaft[flv[fi], flv[fi] // AmpConjugate]
CutAmplitudeGlue[f1_, f2_] := Error["Can't connect these cut lines: ", f1, " and ", f2]
CutAmplitudeGlue[
  dia1:Diagram[id1_, factor1_, ifields1_List, ofields1_List, propagators1_List, vertices1_List],
  dia2:Diagram[id2_, factor2_, ifields2_List, ofields2_List, propagators2_List, vertices2_List]
] := (
  FailUnless[ofields1[[;;,1]] === ofields2[[;;,1]]];
  Times[
    MapThread[CutAmplitudeGlue, {ofields1, ofields2}] // Apply[Times],
    ofields1[[;;,1]] // PositionIndex // Values // Map[Length /* Factorial] // 1/Times@@# &
  ]
)
AmpToForm[expression_] :=
 ToString[expression, InputForm] //
  StringReplace[{"\"" -> "", " " -> "", "[" -> "(", "]" -> ")"}]
AmpFromForm[expression_] := expression
AmpFormIndexMaps[ex_] := Module[{original, renamed, i, mu},
  original = CaseUnion[ex, _lor | _adj | _fun | _spn | _X | _flv];
  renamed = original // Map[Replace[{
       i : (idx_)[mu_?Negative] :>
        ToExpression[ToString[idx] <> "e" <> ToString[-mu]],
       i : (idx_)[mu_] :>
        ToExpression[ToString[idx] <> "i" <> ToString[mu]]
       }]];
  {Association[MapThread[Rule, {original, renamed}]],
   Association[MapThread[Rule, {renamed, original}]]}
  ]

AmpSanityCheck[ex_List] := Map[AmpSanityCheck, ex]
AmpSanityCheck[ex_] := Module[{idx, termidx},
  idx = ex // CaseUnion[_flv|_lor|_fun|_adj|_spn];
  ex // Expand // Map[(
    termidx = Cases[#, _flv|_lor|_fun|_adj|_spn, -1] // Sort // GroupBy[#&] // Map[Length];
    If[idx =!= Keys[termidx],
      Print["Expected this set of indices: ", idx];
      Print["Got this set: ", Keys[termidx]];
      Error["Mismatch in index set"];
    ];
    If[Union[Values[termidx]] =!= {2},
      Print["* Summation notation fail:"];
      termidx // Normal // DeleteCases[_->2] // PrintIndexed;
      Error["Summation notation fail"];
    ];
  )&];
]

AmpFormRun[{}, _] := {}
AmpFormRun[exprs_List, code_] :=
 Module[{tmpsrc, tmpdst, tmplog, result, toform, fromform, i, expridxs},
  tmpsrc = MkTemp["amp", ".frm"];
  tmpdst = tmpsrc <> ".m";
  tmplog = tmpsrc // StringReplace[".frm" ~~ EndOfString -> ".log"];
  {toform, fromform} = AmpFormIndexMaps[exprs];
  (*CopyToClipboard[tmpsrc//PR];*)
  MkFile[tmpsrc,
    "#include library.frm\n",
    (* This whole dance is needed to distribute expressions
     * across workers; tform would keep everything in a single
     * process if we where to just do assign our expression to
     * EXPR directly.
     *)
    "Table EXTBL(1:", Length[exprs], ");\n",
    Table[
      {"Fill EXTBL(", i, ") = (",
        (* Replacing delta() with d_() doesn't work because
         * d_()^2 is broken. Instead use (d_()), which works.
         * See: github.com/vermaseren/form/issues/341.
         *)
        exprs[[i]] // ReplaceAll[toform] // AmpToForm // (*StringReplace["delta("->"d_("]*)
          StringReplace["delta(" ~~ a:(Except[")"] ...) ~~ ")" -> "(d_(" ~~ a ~~ "))"],
        ");\n"}
      ,
      {i, Length[exprs]}
    ],
    (*"L EXPR = <EX(1)>+...+<EX(", Length[exprs], ")>;\n",*)
    "L EXPR = sum_(xidx,1,", Length[exprs], ",EX(xidx));\n",
    ".sort:init;\n",
    (* Now that EX(n) are in different workers, we can insert
     * their values, and start the computations.
     *)
    "id EX(x?) = EX(x)*EXTBL(x);\n",
    (*"cleartable EXTBL;\n"*)
    "#call input\n",
    code,
    "#call output(", tmpdst, ")\n",
    ".end\n"
  ];
  Print["AmpFormRun: calling tform ", tmpsrc];
  (*Run["tform", "-q", "-Z", "-M", "-l", "-w4", tmpsrc]//TM;*)
  Run["form", "-q", "-Z", "-M", "-l", tmpsrc]//TM;
  Print["AmpFormRun: reading result (", FileByteCount[tmpdst]//FormatBytes, ")"];
  result = SafeGet[tmpdst]//TM;
  Print["AmpFormRun: transforming it back"];
  result /. fromform // AmpFromForm // Terms //
      Map[Replace[EX[i_]*ex_. :> {i, ex}]] //
     GroupBy[#, First -> (#[[2]] &)] & // Map[Apply[Plus]] //
   Lookup[#, Range[Length[exprs]], 0] &
  ]
AmpFormRun[code_] := AmpFormRun[#, code]&
AmpFormRun[exprs_, code_] := AmpFormRun[{exprs}, code] // First
FormFn[procname_String] := {"#call ", procname, "\n"}
FormFn[procnames__] := Map[FormFn, {procnames}]
FormFnToB[ibpbasis_List, extobid_List] := Module[{expridxs},
  {
    "#procedure toBid\n",
    "  ",
    ibpbasis // Map[Function[{basis},
      expridxs = Range[Length[extobid]] // Select[(extobid[[#]] === basis[[1]])&];
      (*If[basis[[1]] === 1, "  ", "  else"],*)
      If[expridxs === {},
        Nothing,
        {
          "if (match(EX(x?{,", expridxs // Riffle[#, ","]& ,"})));\n",
          "    multiply Bid^", basis[[1]], ";\n"
        }
      ]
    ]] // Riffle[#, "  else"]&,
    "  else;\n",
    "    exit \"ERROR: toBid: got a term without a good EX(n) factor.\";\n",
    "  endif;\n",
    "#endprocedure\n",
    "#procedure toBden\n",
    "  ",
    ibpbasis // Map[Function[{basis},
      expridxs = Range[Length[extobid]] // Select[(extobid[[#]] === basis[[1]])&];
      (*If[basis[[1]] === 1, "  ", "  else"],*)
      If[expridxs === {},
        Nothing,
        {
          "if (match(only, Bid^", basis[[1]], "));\n",
          basis[[{5,6}]] // Apply[Join] // Normal //
            Map[{"    id ", #[[1]]//AmpToForm, " = ",
              #[[2]]/.bden[n_]:>MkString["Bden",n]//AmpToForm, ";\n"}&]
        }
      ]
    ]] // Riffle[#, "  else"]&,
    "  else;\n",
    "    exit \"ERROR: toBden: got a term without a proper Bid^n factor.\";\n",
    "  endif;\n",
    "#endprocedure\n",
    "#call toB(", Length[ibpbasis[[1, 4]]], ", toBid, toBden)\n"
  }
]
ClearAll[InsertToBFactors];
InsertToBFactors[ibpbasis_, bidlist_List] := InsertToBFactors[#, ibpbasis, bidlist]&
InsertToBFactors[ex_List, ibpbasis_, bidlist_List] :=
  MapThread[ReplaceAll[#1, #2]*Bid^#3&, {
    ex,
    ibpbasis[[;;, 5]]//ReplaceAll[#, CaseUnion[#, bden[n_] :> (bden[n]->MkExpression["Bden", n])]]&//#[[bidlist]]&,
    bidlist
  }]
FormFnToB[ibpbasis_List] := {
    "#procedure toBid\n",
    "* Assume Bid^n factor are already supplied.\n",
    "#endprocedure\n",
    "#procedure toBden\n",
    "  ",
    ibpbasis // Map[Function[{basis},
      {
        "if (match(only, Bid^", basis[[1]], "));\n",
        basis[[6]] // Normal //
          Map[{"    id ", #[[1]]//AmpToForm, " = ",
            #[[2]]/.bden[n_]:>MkString["Bden",n]//AmpToForm, ";\n"}&]
      }
    ]] // Riffle[#, "  else"]&,
    "  else;\n",
    "    exit \"ERROR: toBden: got a term without a proper Bid^n factor.\";\n",
    "  endif;\n",
    "#endprocedure\n",
    "#call toB(", Length[ibpbasis[[1, 4]]], ", toBid, toBden)\n"
  }
FormFnZeroSectors[zerobs_List] := zerobs //
  MapReplace[B[bid_, idx__] :> {
    "id B(", bid, MapIndexed[ReplaceAll[#1, {
      0 -> {", x", #2, "?neg0_"},
      1 -> {", x", #2, "?"}
    }]&, {idx}], ") = 0;\n"
  }]
FormFnReplace[rules__] := {rules} // MapReplace[
  (sp[p_] -> v_) :> {
    "id sp(", p // InputForm, ") = ", v // InputForm, ";\n",
    "id dot(", p // InputForm, ",", p // InputForm, ") = ", v // InputForm, ";\n"
  },
  (sp[p1_,p2_] -> v_) :> {
    "id sp(", p1 // InputForm, ",", p2 // InputForm, ") = ", v // InputForm, ";\n",
    "id sp(", p2 // InputForm, ",", p1 // InputForm, ") = ", v // InputForm, ";\n",
    "id dot(", p1 // InputForm, ",", p2 // InputForm, ") = ", v // InputForm, ";\n"
  }
]

(*
{
  deltaflv[1,2] deltaflv[2,3] deltaflv[3,1],
  deltaflv[1,2] deltaflv[2,3] deltaflv[3,1] chargeQ[flv[2]],
  deltaflv[1,2] deltaflv[2,3] deltaflv[3,1] chargeV[flv[2]],
  deltaflv[1,2] deltaflv[2,3] deltaflv[3,1] chargeA[flv[2]],
  deltaflv[1,2] deltaflv[2,3] deltaflv[3,1] chargeQ[flv[2]] chargeQ[flv[1]],
  deltaflv[1,2] deltaflv[2,3] deltaflv[3,1] chargeV[flv[2]] chargeV[flv[1]],
  deltaflv[1,2] deltaflv[2,3] deltaflv[3,1] chargeA[flv[2]] chargeA[flv[1]],
  deltaflv[1,2] deltaflv[2,3] deltaflv[3,1] chargeV[flv[2]] chargeA[flv[1]],
  deltaflv[1,2] deltaflv[2,3] deltaflv[3,1] /. flv[3] -> 0,
  deltaflv[1,2] deltaflv[2,3] deltaflv[3,1] chargeQ[flv[2]] /. flv[3] -> 0,
  deltaflv[1,2] deltaflv[2,3] deltaflv[3,1] chargeV[flv[2]] /. flv[3] -> 0,
  deltaflv[1,2] deltaflv[2,3] deltaflv[3,1] chargeA[flv[2]] /. flv[3] -> 0,
  deltaflv[1,2] deltaflv[2,3] deltaflv[3,1] chargeQ[flv[2]] chargeQ[flv[1]] /. flv[3] -> 0,
  deltaflv[1,2] deltaflv[2,3] deltaflv[3,1] chargeV[flv[2]] chargeV[flv[1]] /. flv[3] -> 0,
  deltaflv[1,2] deltaflv[2,3] deltaflv[3,1] chargeA[flv[2]] chargeA[flv[1]] /. flv[3] -> 0,
  deltaflv[1,2] deltaflv[2,3] deltaflv[3,1] chargeA[flv[2]] chargeV[flv[1]] /. flv[3] -> 0
} // AmpFormRun[FormFn["flavorsum"]]//InputForm
*)

(* Remove factors that don't have indices inside from each
 * expression in a list, apply f to the resulting list, put the
 * factors back in. *)
AmpHideFactorsFromMap[ex_List, f_] := Module[{wheat, chaff},
  {wheat, chaff} =
    ex // Map[(# // Factors //
        GroupBy[FreeQ[#, Xi | _den | _dot | _sp | _lor | _adj | _fun | _spn | _X | _flv] &] //
        Lookup[#, {False, True}, {}] & //
        Map[Apply[Times]]) &
      ] // {#[[;;,1]], #[[;;,2]]}&;
  Print["AmpHideFactorsFromMap: wheat/chaff is ", ByteCount[wheat], "/", ByteCount[chaff]];
  MapThread[Times, {f[wheat], chaff}]
]
AmpHideFactorsFromMap[f_] := AmpHideFactorsFromMap[#, f] &
CutDiagramFlavorFactor[CutDiagram[d1_Diagram, d2_Diagram]] := Module[{a1, a2},
  a1 = d1 // Amplitude;
  a2 = d2 // Amplitude // AmpConjugate;
  a1 a2 /. flv[-2] -> 0 // FasterFactor // Factors //
    Select[NotFreeQ[_deltaf | _chargeQ | _chargeV | _chargeA]] // Apply[Times] //
    List // AmpFormRun[FormFn["flavorsum"]] // First
]

(* Graph canonization *)

StartDreadnaut[] :=
  If[(Head[$DREADNAUT] === Symbol || Not[ProcessStatus[$DREADNAUT, "Running"]]),
    $DREADNAUT = StartProcess["dreadnaut"];
    WriteLine[$DREADNAUT, "As l=0 $=1 c"];
  ]
KillDreadnaut[] :=
  If[Head[$DREADNAUT] =!= Symbol,
    KillProcess[$DREADNAUT];
    ClearAll[$DREADNAUT];
  ]
(*"
No self-loops.
No double lines.
Vertex colors are integers that start at 0.
Edge colors are integers that start at 1.
"*)
ColoredSimpleGraphToDreadnaut[vertices : {{_, _} ...}, edges : {{_, _, _} ...}] :=
 Module[{nlayers, nvertices, vertex2idx, i, l},
  nvertices = Length[vertices];
  nlayers = BitLength[Max[edges[[;; , 3]], vertices[[;; , 2]], 1]];
  vertex2idx = MapIndexed[First[#1] -> First[#2] &, vertices] // Association;
  MkString[
   "As c n=", nlayers*nvertices + 1, " $=0 g",
   Table[(*inter-layer threads*)
    {" ", l + i, ":", l + i + nvertices},
    {i, Length[vertices]}, {l, 0, (nlayers - 2) nvertices, nvertices}],
   Table[(*vertex colors*)
    If[BitGet[vertices[[i, 2]], l] =!= 1, Nothing, {" 0:", i + l nvertices}],
    {i, Length[vertices]}, {l, 0, nlayers - 1}],
   Table[(*edge colors*)
    If[BitGet[edges[[i, 3]], l] =!= 1, Nothing,
     {" ", vertex2idx[edges[[i, 1]]] + l nvertices, ":",
      vertex2idx[edges[[i, 2]]] + l nvertices}],
    {i, Length[edges]}, {l, 0, nlayers - 1}],
   " .\nf=[0",
   Table[{"|", l + 1, ":", l + nvertices}, {l,
     0, (nlayers - 1) nvertices, nvertices}],
   "]\nx \"--{\\n\" b \"}--\\n--cut--\\n\" ->>"
   ]
  ]
ColoredSimpleGraphCanonicalLabel[vertices : {{_, _} ...}, edges : {{_, _, _} ...}] :=
  Module[{nvertices = Length[vertices]},
    StartDreadnaut[];
    WriteLine[$DREADNAUT, ColoredSimpleGraphToDreadnaut[vertices, edges]];
    ReadString[$DREADNAUT, "--cut--\n"] //
      StringCases[#, "--{\n" ~~ Shortest[x__] ~~ "\n" ~~ Shortest[__] ~~ "}--" :> x ] & //
      Last // StringSplit // Map[ToExpression] //
      Select[0 < # <= nvertices &] //
      (*(Print["CAN ORD: ", #, " -> ",vertices[[#]]];#)&//*)
      (*PositionIndex // Map[First] // Normal // Map[Apply[vertices[[#2,1]] -> vertices[[#1,1]]&]] // PR*)
      MapIndexed[vertices[[#1, 1]] -> First[#2] &, #] &
  ]
(*"
Canonical vertex relabeling (a list of rules) for an undirected graph.
Self-edges and doubled lines are allowed.
Example:
  GraphCanonicalLabel[{{a,b},{b,c},{c,b},{c,d},{d,d}}]
  => {b -> 1, c -> 2, a -> 3, d -> 4}
"*)
GraphCanonicalLabel[g : {{_, _} ...}] :=
  Module[{edges, vertices, v},
    edges = g // Map[Sort];
    vertices = Union[edges[[;; , 1]], edges[[;; , 2]]];
    ColoredSimpleGraphCanonicalLabel[
      Table[{v, Count[edges, {v, v}]}, {v, vertices}],
      Counts[edges] // Normal // DeleteCases[{v_, v_} -> _] // Map[Apply[Append]]
    ]
  ]
(*
Example:
  GraphCanonicalForm[{{a,b},{b,c},{c,b},{c,d},{d,d}}]
  => {{1, 2}, {1, 2}, {1, 3}, {2, 4}, {4, 4}}
*)
GraphCanonicalForm[g_] :=
  g /. GraphCanonicalLabel[g] // Map[Sort] // Sort
GraphCanonicalString[g_] :=
  g // GraphCanonicalForm // Map[Riffle[#, "-"] &] // Riffle[#, ";"] & // MkString
(*"
Canonical vertex relabeling (a list of rules) for an undirected graph \
with colored edges.
Self-edges and doubled lines are allowed.
Colors can be any objects.
"*)
ColoredGraphCanonicalLabel[g : {{_, _, _} ...}] :=
 Module[{edgecolors, color2idx, vertices, v},
  edgecolors = g // GroupBy[(Sort[#[[;; 2]]] &) -> (#[[3]] &)];
  (*//Normal//Apply[List]//Transpose;*)
  color2idx = edgecolors // Values // Map[Sort] // Union // PositionIndex // Map[First];
  edgecolors = edgecolors // Map[color2idx];
  vertices = Union[g[[;; , 1]], g[[;; , 2]]];
  ColoredSimpleGraphCanonicalLabel[
    Table[{v, edgecolors[{v, v}] /. _Missing -> 0}, {v, vertices}],
    edgecolors // Normal // DeleteCases[{v_, v_} -> _] // Map[Apply[Append]]
  ]
]
ColoredGraphCanonicalForm[g_] :=
  g /. ColoredGraphCanonicalLabel[g] // Map[Append[Sort[{#[[1]], #[[2]]}], #[[3]]] &] // Sort
ColoredGraphCanonicalString[g_] :=
  g // ColoredGraphCanonicalForm // Map[Riffle[#, "-"] &] // Riffle[#, ";"] & // MkString

(* IBP Topologies *)

(* Get the list of momenta in the denominators of a Diagram or
 * a CutDiagram.
 *)
ClearAll[Denominators];
Denominators[P["t"|"T", _, _, _, _, mom_]] := {den[mom, mt2]}
Denominators[P["q"|"g"|"s"|"c", _, _, _, _, mom_]] := {den[mom]}
Denominators[d_Diagram] := d // CaseUnion[_P] // Map[Denominators] // Apply[Join] // NormalizeDens
Denominators[CutDiagram[
    Diagram[_, _, _, o1_List, p1_List, _List],
    Diagram[_, _, _, o2_List, p2_List, _List]
  ]] := Join[
    Join[p1, p2] // Map[Denominators] // Apply[Join],
    o1 /. F[__, mom_] :> den[DropLeadingSign[mom], 0, cut]
  ]
Denominators[x_] := Error["Don't know the denominators of ", x]

CutDiagramGraph[CutDiagram[d1_Diagram, d2_Diagram]] := Module[{e1, e2},
  e1 = DiagramGraphEdges[d1];
  e2 = DiagramGraphEdges[d2];
  Join[e1 // DeleteCases[_ <-> _EE],
   e2 /. SS -> SS2 /. II -> II2 /. Cases[e1, (i_ <-> e_EE) :> ((x_ <-> e) :> Style[x <-> i, Dashed])]]
]
CanonicalSector[edges_: {{_, _, _, _} ...}] :=
  Module[{canonicmap, tag, canonicedges},
    canonicmap = ColoredGraphCanonicalLabel[edges[[;; , ;; 3]]] // Association;
    canonicedges =
      MapAt[canonicmap, edges, {;; , ;; 2}] //
        Map[If[OrderedQ[#[[;; 2]]], #, #[[{2, 1, 3, 4}]]*{1, 1, 1, -1}] &] // Sort;
    tag = canonicedges[[;; , ;; 3]] // Map[Riffle[#, " "] &] // Riffle[#, ";"] & // MkString;
    {tag, canonicedges}
  ]
ClearAll[SectorsAdd];
SectorsAdd[sectors_,
    CutDiagram[Diagram[_, _, i1_List, o1_List, p1_List, _],
    Diagram[_, _, i2_List, o2_List, p2_List, _]]] :=
  Module[{outfi2vi, edges, tag},
  (*
  Construct an edge-colored graph with distinct colors for each
  input and each output, one color for propagators, one color for
  the cut lines, and a separate color for the first cut line.
  *)
  outfi2vi =
   o1 /. F[_, fi_, vi_, mom_] :> Rule[fi, vi] // Association;
  edges = Join[
    i1 /. F[_, fi_, vi_, mom_] :> {SS[fi], I1[vi], fi, mom},
    i2 /. F[_, fi_, vi_, mom_] :> {I2[vi], EE[fi], fi, mom},
    o2 /.
     F[_, fi_, vi_, mom_] :> {I1[outfi2vi[fi]], I2[vi],
       If[fi === -2, 0, 0], mom},
    p1 /. P[_, _, _, vi1_, vi2_, mom_] :> {I1[vi1], I1[vi2], 0, mom},
    p2 /.
     P[_, _, _, vi1_, vi2_, mom_] :> {I2[vi2], I2[vi1], 0,
       mom /. l1 -> r1 /. l2 -> r2 /. l3 -> r3 /. l4 -> r4}
    ];
  SectorsAdd[sectors, edges]
  ]
SectorsAdd[sectors_, sector_List] := Module[{tag, edges},
  {tag, edges} = CanonicalSector[sector];
  If[KeyExistsQ[sectors, tag],
   sectors
   ,
   Append[sectors, tag -> edges]
   ]
  ]
SectorUniquePropagators[sector_] :=
 sector // Cases[{_, _, _?(Negative /* Not), _}] // #[[;; , 4]] & //
   Map[DropLeadingSign] // Union
SubSectors[subsectors_, {}] := subsectors
SubSectors[subsectors_, sector_List] :=
 Module[{ss = subsectors, line, i, tag, edges},
  Do[
   line = sector[[i]];
   If[Not[MatchQ[line, {a_, a_, _, _} | {_, _, Except[0], _}]],
    edges =
     Drop[sector, {i}] //
      MapAt[Replace[line[[1]] -> line[[2]]], #, {;; , ;; 2}] &;
    If[Not[MatchQ[edges, {___, {a_, a_, ___}, ___}]],
     {tag, edges} = CanonicalSector[edges];
     If[Not[KeyExistsQ[ss, tag]],
      (*Print["ADD ",1+Length[ss]," (",tag,"):",edges//Map[
      Apply[{DirectedEdge[#1,#2](*,Text[#4]*)}&]]//XGraph];*)
      ss = Append[ss, tag -> edges] // SubSectors[#, edges] &;
      ]
     ]
    ],
   {i, Length[sector]}];
  ss
  ]
(* Return this sector, and all its subsectors with the same set of propagators. *)
EqualSectorSet[sector_] := Module[{tag, edges},
  {tag, edges} = CanonicalSector[sector];
  EqualSectorSet[<|tag -> edges|>, edges, SectorUniquePropagators[edges]]
]
EqualSectorSet[subsectors_, sector_, uniqprops_List] :=
  Module[{ss = subsectors, line, i, tag, edges},
    Do[
      line = sector[[i]];
      If[Not[MatchQ[line, {a_, a_, _, _} | {_, _, Except[0], _}]],
        {tag, edges} = CanonicalSector[
          Drop[sector, {i}] // MapAt[Replace[line[[1]] -> line[[2]]], #, {;; , ;; 2}] &];
        If[Not[KeyExistsQ[ss, tag]] && (SectorUniquePropagators[edges] === uniqprops),
          ss = Append[ss, tag -> edges] // EqualSectorSet[#, edges, uniqprops] &;
        ]
      ],
    {i, Length[sector]}];
    ss
  ]
SuperSectors[sectors_] :=
  Module[{supersectors, subsectors, tag, sector, eqsubsectors, sector2subsectors},
    subsectors = <||>;
    supersectors = <||>;
    sector2subsectors = <||>;
    Do[
     eqsubsectors = EqualSectorSet[sectors[tag]](*//PR*);
     If[AllTrue[eqsubsectors // Keys, Not[KeyExistsQ[subsectors, #]] &],
      (*{tag,sector}=eqsubsectors//Normal//SortBy[Length[#[[2]]]&];
      If[Length[eqsubsectors]>
      0,*)
      {tag, sector} = eqsubsectors // Normal // #[[-1]] & // Apply[List](*//PR*)(*]*);
      subsectors = Append[subsectors, tag -> sector] // SubSectors[#, sector] &;
      supersectors = Append[supersectors, tag -> sector];
      ];
     ,
     {tag, Keys[sectors] // SortBy[sectors /* SectorUniquePropagators /* Length /* Minus]}];
    supersectors
  ]
SectorXGraph[sector_] := sector // Map[Apply[{DirectedEdge[#1, #2], Text[#4]} &]] // XGraph
SectorXGraph[sector_] := sector // Map[Replace[{
      {v1_, v2_, 2, mom_} :> {DirectedEdge[v1, v2], Text[mom], Thick,
        Dashed, Red},
      {v1_, v2_, 1, mom_} :> {DirectedEdge[v1, v2], Text[mom], Thick,
        Dashed},
      {v1_, v2_, _?Negative, mom_} :> {DirectedEdge[v1, v2],
        Text[mom], Lighter[Gray]},
      {v1_, v2_, _, mom_} :> {DirectedEdge[v1, v2], Text[mom], Thick}
      }]] // XGraph
SectorGraph[sector_] :=
 sector // Map[Apply[UndirectedEdge[#1, #2] &]] // Graph
(*
 * Check if a list of denominators are linearly dependent, in
 * the sense that there is a non-trivial linear combination of
 * them that is a constant.
 *
 * The denominators can be: den[p] for 1/p^2, and den[p,m2]
 * for 1/(p^2-m2). Monomials of the form l*k are considered
 * independent variables for this test, with l being any of the
 * loop momenta, and k any of the loop or external momenta.
 *
 * It is OK to supply a superset of the loop momenta in loopmom,
 * and a superset of external momenta in extmom.
 *)
DenominatorsLinearlyDependentQ[dens : {_den ...}, loopmom_List, extmom_List] :=
Module[{L, K, ex, c},
  L = Alternatives @@ loopmom;
  K = Alternatives @@ Join[loopmom, extmom];
  ex = dens /. den[p_] :> p^2 /. den[p_, m2_, ___] :> p^2 - m2 // Expand;
  ex = ex /. (l : L)^2 :> Dot[l, l] /. (l : L)*(k : K) :> Sort[Dot[l, k]];
  PolynomialsLinearlyDependentQ[ex - (ex /. _Dot -> 0), ex // CaseUnion[_Dot]]
]

PartialFractionFactor[dens_List, loopmom_List, extmom_List] :=
Module[{L, E, p, q, m, nums, vars, c, mx, nullspace, ns0, cutk, num, i, ex},
  L = Alternatives @@ loopmom;
  E = Alternatives @@ extmom;
  (* nums = 1/dens *)
  nums = dens /. den[p_] :> p^2 /. den[p_, m_, ___] :> p^2 - m // Expand;
  nums = nums /.
    (p:L)^2 :> Dot[p, p] /.
    (p:L) (q:L|E) :> Sort[Dot[p, q]] /.
    (p:E) (q:E) :> Sort[sp[p, q]] /.
    (p:E)^2 :> sp[p];
  vars = CaseUnion[nums, _Dot];
  {c, mx} = CoefficientArrays[nums, vars];
  (* nums == c + mx.vars *)
  nullspace = mx // Transpose // NullSpace // Normal;
  (* nullspace[[i]].mx == {0, ...} => ns[[i]].nums == c *)
  If[Length[nullspace] < 1,
    1
    ,
    nullspace = nullspace // RowReduce;
    ns0 = Select[nullspace, Expand[#.c] =!= 0 &];
    (* Assume that all cut demoninators have index 1, and drop
     * indices lower than that. *)
    cutk = Map[If[MatchQ[#, den[_, _, cut]], 0, 1]&, dens];
    If[ns0 =!= {},
      ns0 // MaximalBy[Count[#*cutk, 0] &] // Sort // First // (#*cutk).(1/dens)/Factor[#.c] &
      ,
      Print["WARNING: PartialFractionFactor -- second case"];
      nullspace // MaximalBy[Count[#*cutk, 0] &] // Sort // First //
        #.MapIndexed[num[#2//First]&, dens]& //
        Solve[#==0, #//CaseUnion[_num]//First]& // Only // Only //
        Replace[(num[i_] -> ex_) :> (dens[[i]] * ex)] //
        ReplaceAll[num[i_] :> cutk[[i]]/dens[[i]]]
    ]
  ]
]
(*
FailUnless[
  PartialFractionFactor[{den[p1], den[p1+p2], den[p2], den[p1-p2]}, {p1,p2}, {q}] /.
    den[p_]:>1/p^2 /. den[p_,m_]:>1/(p^2-m) // Together // #===1&
];
FailUnless[
  PartialFractionFactor[{den[p1], den[p1,m]}, {p1,p2}, {q}] /.
    den[p_]:>1/p^2 /. den[p_,m_]:>1/(p^2-m) // Together // #===1&
];
*)
(*
 * Perform partial fraction decomposition on terms consisting of
 * den[p] and den[p,m2,___] objects via the Leinartas' algorithm.
 * Example:
 *   > PartialFraction[den[p]*den[p,m2],{p},{}]
 *   den[p,m2]/m2 - den[p]/m2
 *
 * Cut denominators are assumed to all have power 1, and zero
 * terms are dropped automatically:
 *   > PartialFraction[den[p]*den[p,m2,cut],{p},{}]
 *   den[p,m2]/m2
 *)
PartialFraction[ex_, loopmom_List, extmom_List] :=
 FixedPoint[
  Bracket[#, _den, Together, # * PartialFractionFactor[
      CaseUnion[#, _den]//SortBy[{#,MatchQ[#,den[_,_,cut]]}&], loopmom, extmom
    ]&
  ]&,
  ex]
(* > CompleteIBPBasis[{den[l1], den[q-l1]}, {l1, l2}, {q}, 9]
 * IBPBasis[9, {l1, l2}, {q}, {den[l1], den[-l1 + q], den[l2, 0, irr], den[l1 + l2, 0, irr], ...
 *)
CompleteIBPBasis[denominators_List, loopmom_List, extmom_List, bid_] :=
  Module[{L, p, k, k1, k2, E, nums, vars, c, mx, candidatemoms, denadd, numadd, cadd, mxadd, dens},
    L = loopmom // Map[DropLeadingSign] // Apply[Alternatives];
    E = Join[loopmom, extmom] // Map[DropLeadingSign] // Apply[Alternatives];
    dens = denominators;
    nums = dens /. den[p_] :> p^2 /. den[p_, m2_, ___] :> p^2 - m2 // Expand;
    nums = nums /. (l : L)^2 :> Dot[l, l] /. (l : L) (k : L | E) :>
         Dot @@ Sort[{l, k}] /. (k1 : E) (k2 : E) :>
        sp @@ Sort[{k1, k2}] /. (k : E)^2 :> sp[k];
    vars = Tuples[{loopmom, Join[loopmom, extmom]}] //
       Map[DropLeadingSign /* Sort /* Apply[Dot]] // Union;
    (*Print["Independent scalar products: ",Length[vars],", ", vars];*)
    If[nums =!= {},
      {c, mx} = CoefficientArrays[nums, vars] // Normal;
      (* nums == c + mx.vars *)
      If[MatrixRank[mx] =!= Length[mx],
        Print["nums=",nums];
        Print["vars=",vars];
        Print[c];
        Print[mx];
        Print[MatrixRank[mx]];
       Error["CompleteIBPBasis: denominators ", denominators, " are already linearly dependent"]
      ];
      ,
      {c, mx} = {{}, {}};
    ];
    (*Print["Need ",Length[vars]-Length[mx]," more denominators"];*)
    candidatemoms =
     Subsets[Join[loopmom, extmom], {1, Infinity}] //
       Map[Apply[Plus] /* DropLeadingSign] // Select[NotFreeQ[L]];
    (*Print["Candidate irr denominators: ", candidatemoms];*)
    While[Length[vars] > Length[mx] && Length[candidatemoms] > 0,
     numadd =
      Expand[candidatemoms[[1]]^2] /. (l : L)^2 :>
           Dot[l, l] /. (l : L) (k : L | E) :>
          Dot @@ Sort[{l, k}] /. (k1 : E) (k2 : E) :>
         sp @@ Sort[{k1, k2}] /. (k : E)^2 :> sp[k];
     {cadd, mxadd} = CoefficientArrays[numadd, vars] // Normal;
     If[MatrixRank[Append[mx, mxadd]] === Length[mx] + 1,
      (*Print["Add: ",candidatemoms[[1]]];*)
      AppendTo[mx, mxadd];
      AppendTo[dens, den[candidatemoms[[1]], 0, irr]];
      AppendTo[nums, numadd];
      AppendTo[c, cadd];
      (*c+mx.vars==nums*)
      ,
      (*Print["Can't add: ",candidatemoms[[1]]];*)
      candidatemoms = candidatemoms[[2 ;;]];
      ];
     ];
    IBPBasis[
     (*ID*)bid,
     (*Loom momenta*)loopmom // DropLeadingSign,
     (*Ext momenta*)extmom // DropLeadingSign,
     (*Den list*)dens,
     (*Den map*)
     MapIndexed[#1 -> bden @@ #2 &, dens] // DeleteCases[den[_, _, irr] -> _] //
        ReplaceAll[(den[p_, x___] -> y_) :> {den[p, x] -> y, den[-p, x] -> y}] // Flatten // Association,
     (*Dot map*)
     Inverse[mx].(Map[1/bden[#] &, Range[Length[mx]]] - c) //
        Bracket[#, _bden, Factor] & // MapThread[Rule, {vars, #}] & // Join[#, # /. Dot[a_, b_] :> Dot[b,a]]& //
      Association
    ]
  ]
IBPBasisSanityCheck[ibpbasis_List] := (Map[IBPBasisSanityCheck, ibpbasis];)
IBPBasisSanityCheck[IBPBasis[bid_, loopmom_List, extmom_List, dens_List, denmap_, dotmap_]] :=
Module[{normaldens, i, dots, p1, p2, p, m},
  normaldens = dens // DeleteCases[den[_, _, irr]];
  FailUnless[(normaldens /. denmap) === Table[bden[i], {i, Length[normaldens]}]];
  FailUnless[(MapAt[Minus, normaldens, {;;,1}] /. denmap) === Table[bden[i], {i, Length[normaldens]}]];
  dots = Table[{p1.p2, p2.p1}, {p1, loopmom}, {p2, Join[loopmom, extmom]}] // Flatten // Union;
  FailUnless[Map[Sort, dots] === Together[dots /.
    dotmap /. bden[i_] :> dens[[i]] /.
    den[p_] :> 1/p.p /. den[p_, m_, ___] :> 1/(p.p-m) /.
    Dot->ExpandDot /. sp[p1_] :> p1.p1 /. sp[p1_, p2_] :> p1.p2
  ]];
]
IBPBasisSanityCheck[ex_] := Error["Not an IBPBasis: ", ex]

(*sectors[[1,;;,4]]//DeleteCases[q|-q]//Map[DropLeadingSign/*den]// CompleteIBPBasis[#,{l1,l2,l3,p1},{-q},99]&*)
DiagramToSector[Diagram[_, _, i_List, o_List, p_List, _]] :=
 Module[{outfi2vi, edges, tag},
  Join[
   i /. F[_, fi_, vi_, mom_] :> {fi, vi, fi, mom},
   o /. F[_, fi_, vi_, mom_] :> {vi, fi, fi, mom},
   p /. P[_, _, _, vi1_, vi2_, mom_] :> {vi1, vi2, 0, mom}
   ]
  ]
DiagramToSector[CutDiagram[
  Diagram[_, _, i1_List, o1_List, p1_List, _],
  Diagram[_, _, i2_List, o2_List, p2_List, _]
 ]] :=
 Module[{outfi2vi, edges, tag},
  outfi2vi = o1 /. F[_, fi_, vi_, mom_] :> Rule[fi, vi] // Association;
  edges = Join[
    i1 /. F[_, fi_, vi_, mom_] :> {SS[fi], I1[vi], fi, mom},
    i2 /. F[_, fi_, vi_, mom_] :> {I2[vi], EE[fi], fi, mom},
    o2 /.
     F[_, fi_, vi_, mom_] :> {I1[outfi2vi[fi]], I2[vi],
       If[fi === -2, 2, 1], mom},
    p1 /. P[_, _, _, vi1_, vi2_, mom_] :> {I1[vi1], I1[vi2], 0, mom},
    p2 /.
     P[_, _, _, vi1_, vi2_, mom_] :> {I2[vi2], I2[vi1], 0,
       mom // AmpConjugateMomenta}
    ]
  ]
SectorDeletePropagators[sector_List, {momentum_}] := Module[{rules},
  rules = sector // Cases[{v1_, v2_, 0, momentum | -momentum} :> Rule[v1, v2]];
  sector // DeleteCases[{_, _, 0, momentum | -momentum}] //
    MapAt[Replace[rules], #, {;; , ;; 2}] &
]
SectorDeletePropagators[sector_List, momenta_List] :=
  Fold[SectorDeletePropagators[#1, {#2}] &, sector, momenta]
SectorDeletePropagators[momenta_List] :=
  SectorDeletePropagators[#, momenta] &

IBPBasisToInclusive[IBPBasis[bid_, loopmom_List, extmom_List, dens_List, denmap_, dotmap_]] :=
  CompleteIBPBasis[Join[
      dens // DeleteCases[den[_, (1-x)sp[q], cut]],
      dens // Cases[den[p_, (1-x)sp[q], cut] :> den[p, 0, irr]]
    ],
    loopmom, extmom, bid]

SectorToSubTopologies[sector_, mastersubsectors_] :=
 Module[{eqsectors, tag, edges, idx, eqs, result},
  result = None;
  eqsectors = EqualSectorSet[sector];
  (*{tag,edges}=CanonicalSector[sector];*)
  Do[
   edges = eqsectors[tag];
   (*Print["EQ SECTOR:",edges//SectorXGraph];*)
   idx = FirstPosition[
      mastersubsectors, _?(KeyExistsQ[#, tag] &), {-1}, {1},
      Heads -> False] // First;
   If[idx >= 0,
    eqs = (edges[[;; , 4]] /.
        p : Except[List | Times | Plus | Power, _Symbol] :>
         OLD[p]) - (mastersubsectors[[idx]][tag][[;; , 4]] /.
        p : Except[List | Times | Plus | Power, _Symbol] :> NEW[p]);
    (*Print["EQ:",eqs];*)
    (*Print[mastersubsectors[[idx,1]]//
    SectorXGraph];*)
    result = {idx,
      Solve[eqs // Map[# == 0 &], CaseUnion[eqs, _OLD]] // First //
       ReplaceAll[(OLD | NEW)[p_] :> p]};
    Break[];
    ]
   ,
   {tag, eqsectors // Keys}];
  If[result === None, Error["Failed to match a sector to topologies"]];
  result
  ]

ClearAll[ExpandDot, LinearizeDot]
ExpandDot[a_, b_] := Distribute[Dot[Expand[a], Expand[b]]] /. Dot -> LinearizeDot
LinearizeDot[k1_.*a_Symbol, k2_. b_Symbol] := k1 k2 Dot @@ Sort[{a, b}]
LinearizeDot[a_, b_] := Error["ExpandDot: don't know how to expand ", a.b]

BToDen[ex_, ibpbasis_List] := (ex
  /. B[bid_, idx__] :> (Times @@ (ibpbasis[[bid, 4]]^{idx}))
  /. den[p_]^n_?Negative :> dot[p, p]^(-n)
  /. den[p_, m_]^n_?Negative :> (dot[p, p] - m)^(-n)
  /. den[p_, m_, irr]^n_?Negative :> (dot[p, p] - m)^(-n)
  /. dot -> ExpandDot
  /. den[p_, m2_, irr] :> (dot[p, p] - m2)
)
BToDen[ibpbasis_List] := BToDen[#, ibpbasis] &

SemiInclusiveCuts[in_String, out1_String, order_Integer, projector_String] :=
Module[{particles, priority, n1, n2, proj},
  particles = Join[{out1}, DeleteCases[{"g", "q", "Q", "c", "C", "-"}, out1]];
  priority = PositionIndex[particles] // Map[First];
  Tuples[particles, order + 1] //
    Map[DeleteCases["-"] /* Map[priority] /* Sort] //
    Union //
    Map[Map[particles[[#]]&]] //
    Select[FreeQ[out1] /* Not] //
    Select[(Length[#] >= 2)&] //
    Select[(Count[#, "c"] === Count[#, "C"])&] //
    Select[(Count[#, "q"] === Count[#, "Q"])&] //
    Map[Table[
      n1 = order - (n2 + Length[#] - 1);
      If[n1 >= n2, Cut[in, StringJoin@@#, n1, n2, projector], Nothing]
      ,
      {n2, 0, order}]&
    ] // Flatten
]

ListAllCuts[in_String, out_, order_] := Module[{n1, n2},
  (out /. proc_String :> (Plus@@Flatten@Table[
    n1 = order - (n2 + StringLength[proc] - 1);
    If[n1 < 0, 0,
      If[n1 >= n2,
        Cut[in, proc, n1, n2, "I"],
        Cut[in, proc, n2, n1, "I"] (REAL-1)
      ]
    ]
    ,
    {n2, 0, order}])) // Bracket[#, _Cut]&
]

ProcessCutList[{in_String, out_, order_Integer}] := ListAllCuts[in, out, order] // CaseUnion[_Cut]
ProcessCutList[{Rule[in_String, out1_String], order_Integer, projector_String}] := SemiInclusiveCuts[in, out1, order, projector]
ProcessCutList[l_List] := Map[ProcessCutList, l] // Apply[Join]
ProcessCutList[ex_] := Error["ProcessCutList: bad process format ", ex]

(*
 * Mass dimension
 *)
ClearAll[MassDim];
MassDim[ex_List] := Map[MassDim, ex]
MassDim[ex_Series] := MapAt[MassDim, ex, {3}]
MassDim[x_ /; FreeQ[x, _sp|_dot|_B|_den]] := 0
MassDim[HoldPattern[Times[a__]]] := Plus @@ MassDim /@ {a}
MassDim[HoldPattern[Plus[a__]]] := SameMassDim @@ MassDim /@ {a}
MassDim[HoldPattern[a_^b_]] := MassDim[a]*b
MassDim[B[_, idx__]] := -Plus[idx]
MassDim[sp[__]] := 1
MassDim[dot[__]] := 1
MassDim[den[__]] := -1
MassDim[ex_] := Error["Don't know the mass dimension of a ", Head[ex]]
ClearAll[SameMassDim];
SameMassDim[a_ ..] := a

(*
 * Master integral fixing
 *)
RedistributeDots[B[bid_, idx__], ibpbasis_List] := Module[{iscut, badd, freeslots},
  iscut = ibpbasis[[bid, 4]] // MapReplace[den[_, _, cut] -> True, _den -> False];
  newidx = MapThread[If[#1, 1, #2]&, {iscut, {idx}}];
  badd = IndicesToR[{idx}] - IndicesToR[newidx];
  freeslots = MapThread[If[#1 || #2 <= 0, Nothing, #3]&, {iscut, {idx}, Range[Length[{idx}]]}];
  IntegerPartitions[badd + Length[freeslots], {Length[freeslots]}] - 1 //
    Map[Permutations] //
    Apply[Join] //
    Map[MapThread[Rule, {freeslots, #}]& /* (SparseArray[#, Length[iscut]]&) /* Normal] //
    Union //
    Reverse //
    Map[B[bid, Sequence @@ (newidx + #)]&]
]
AddNumerators[B[bid_, idx__], ibpbasis_List, nadd_] := Module[{isok, freeslots},
  isok = MapThread[And, {
    ibpbasis[[bid, 4]] // MapReplace[den[_, _, irr|cut] -> False, _den -> True],
    {idx} // MapReplace[_?NonPositive -> True, _ -> False]
  }];
  freeslots = MapIndexed[If[#1, #2//First, Nothing]&, isok];
  IntegerPartitions[nadd + Length[freeslots], {Length[freeslots]}] - 1 //
    Map[Permutations] //
    Apply[Join] //
    Map[MapThread[Rule, {freeslots, #}]& /* (SparseArray[#, Length[isok]]&) /* Normal] //
    Union //
    Reverse //
    Map[B[bid, Sequence @@ ({idx} - #)]&]
]
AddDenominators[B[bid_, idx__], ibpbasis_List, nadd_] := Module[{isok, freeslots},
  isok = MapThread[And, {
    ibpbasis[[bid, 4]] // MapReplace[den[_, _, irr|cut] -> False, _den -> True],
    {idx} // MapReplace[_?Positive -> True, _ -> False]
  }];
  freeslots = MapIndexed[If[#1, #2//First, Nothing]&, isok];
  IntegerPartitions[nadd + Length[freeslots], {Length[freeslots]}] - 1 //
    Map[Permutations] //
    Apply[Join] //
    Map[MapThread[Rule, {freeslots, #}]& /* (SparseArray[#, Length[isok]]&) /* Normal] //
    Union //
    Reverse //
    Map[B[bid, Sequence @@ ({idx} + #)]&]
]

(* Epsilon form solutions
 *)

(* Return a list of expressions, each representing an order of
 * expansion in epsilon of the solution to D[J,x]=epsilon*S.J.
 * Each order is a list of {Hlog[...], matrix} entries.
 *)
EpsilonFormFundamentalSolutionSeries[S_, x_, orders_Integer] := Module[{Sa, result},
  Sa = MxApart[S, x] // MapReplace[{1/(k_.*x+c_.), mx_} :> {-c/k, SparseArray[mx/k]}];
  result = {
    (* Order 0 result is just the identity matrix: *)
    {List[{}, SparseArray[IdentityMatrix[Length[S]]]]}
  };
  Do[
    result = AppendTo[result,
      Table[
        result[[-1]] //
          Map[{Prepend[#[[1]], i], Sa[[i, 2]].#[[2]]//Normal//SparseArray}&] //
          DeleteCases[{_, mx_?ZeroMatrixQ}]
        ,
        {i, Length[Sa]}
      ] // Apply[Join]
    ];
    ,
    orders-1];
  result // Map[MapReplace[
    {{}, mx_} :> {1, mx},
    {idx_List, mx_} :> {G[Sequence@@Sa[[idx,1]],x], mx}
  ]]
]

SeriesOfMatrices[mxlist_List, var_] := Table[
  SeriesData[
    var,
    0,
    Table[If[mxk === 0, 0, mxk[[i,j]]], {mxk, mxlist}],
    0,
    Length[mxlist],
    1]
  ,
  {i, Length[mxlist[[1]]]},
  {j, Length[mxlist[[1, 1]]]}
]

SeriesOfVectors[mxlist_List, var_] := Table[
  SeriesData[
    var,
    0,
    Table[If[mxk === 0, 0, mxk[[i]]], {mxk, mxlist}],
    0,
    Length[mxlist],
    1]
  ,
  {i, Length[mxlist[[1]]]}
]

KnownBasisMap[knownibp:{{(*file*)_String, (*idx*)_Integer, (*exb*)_} ...}, masters_List] := Module[{kmx},
  kmx = knownibp[[;;,3]] // CoefficientMatrix[masters];
  idx = kmx // SelectAnyBasis;
  FailUnless[Length[idx] === Length[masters]];
  (* k = kmx . masters => masters = kmx^-1 . k *)
  Inverse[kmx[[idx, ;;]]].Map[Known[#[[1]], #[[2]]]&, knownibp[[idx]]] // MapThread[Rule, {masters, #}]&
]

(*** LIB IBP ***)

(* R = denominator power sum
 * Dots = denominator dot count
 * T = denominator count
 * S = numerator power sum
 *)
IndicesToSectorId[idx_List] := Plus @@ Table[If[idx[[i]] > 0, 2^(i-1), 0], {i, Length[idx]}]
SectorIdToIndices[sector_Integer, ndens_Integer] := IntegerDigits[sector, 2, ndens] // Reverse
IndicesToR[idx_List] := idx // Cases[n_ /; n > 0 :> n] // Apply[Plus]
IndicesToDots[idx_List] := idx // Cases[n_ /; n>1 :> n-1] // Apply[Plus]
IndicesToT[idx_List] := idx // Count[n_ /; n > 0]
IndicesToS[idx_List] := idx // Cases[n_ /; n < 0 :> -n] // Apply[Plus]

NormalizeDens[ex_] := ex /. den[p_, x___] :> den[DropLeadingSign[p], x] /. den[p_, 0] :> den[p]

(* Kira sorts bases by name, in stead of adhering to the order
 * of definition. We shall make sure that both the numerical
 * and the lexicographic orders match, which will prevent Kira
 * from messing it up.
 *)
KiraBasisName[bid_] := MkString["b", IntegerDigits[bid, 10, 5]]

MkKiraKinematicsYaml[filename_, extmom_List] := MkKiraKinematicsYaml[filename, extmom, {sp[q]->qq}]
MkKiraKinematicsYaml[filename_, extmom_List, sprules_List] :=
  MkFile[filename,
    "kinematics:\n",
    " incoming_momenta: [", extmom // Riffle[#, ", "]&, "]\n",
    " kinematic_invariants:\n",
    "  - [qq, 2]\n",
    "  - [x, 0]\n",
    " scalarproduct_rules:\n",
    (*"  - [[q,q], qq]\n",*)
    sprules // MapReplace[
      (sp[p_] -> v_) :> {"  - [[", p//InputForm, ",", p//InputForm, "], ", v//InputForm, "]\n"},
      (sp[p1_, p2_] -> v_) :> {"  - [[", p1//InputForm, ",", p2//InputForm, "], ", v//InputForm, "]\n"}
    ],
    " symbol_to_replace_by_one: qq"
  ];

TopSectors[idxlist_List] := Module[{tops, sector2r, sector2s, sector2d, s2sectors, int, sector, r, s, d, sectors, done, i, ss},
  tops = idxlist // Map[IndicesToS] // Max[#, 1]&;
  sector2r = <||>;
  sector2s = <||>;
  sector2d = <||>;
  s2sectors = Association @@ Table[s -> {}, {s, 0, tops}];
  Do[
      sector = IndicesToSectorId[int];
      r = IndicesToR[int];
      s = IndicesToS[int];
      d = IndicesToDots[int];
      AppendTo[s2sectors[s], sector];
      sector2r[sector] = Max[r, sector2r[sector] /. _Missing -> 0];
      sector2d[sector] = Max[d, sector2d[sector] /. _Missing -> 0];
      (* Note: s=0 makes Kira produce false masters. It's not
       * clear, if we should only fix s=0 case, or if we need
       * to add +1 to all s. Currently we're doing the former.
       *)
      sector2s[sector] = Max[s, sector2s[sector] /. _Missing -> 1];
      ,
      {int, idxlist}
  ];
  Print["* Sectors by numerator power sum (s)"];
  sectors = {};
  done = {};
  For[s = tops, s >= 0, s--,
      s2sectors[s] = s2sectors[s] // Union // Reverse;
      Do[
          If[MemberQ[done, sector], Continue[]];
          i = FirstPosition[done, ss_ /; (BitAnd[ss, sector] === sector)];
          If[MatchQ[i, _Missing],
              AppendTo[done, sector];
              AppendTo[sectors, sector];
              Print["Unique sector: ", sector, ", nprops=", DigitCount[sector, 2, 1], ", r=", sector2r[sector], ", s=", sector2s[sector], ", d=", sector2d[sector]];
              ,
              i = i[[1]];
              Print["Subsector of ", done[[i]], ": ", sector, ", nprops=", DigitCount[sector, 2, 1], ", r=", sector2r[sector], ", s=", sector2s[sector], ", d=", sector2d[sector]];
              sector2r[done[[i]]] = Max[sector2r[sector], sector2r[done[[i]]]];
              sector2d[done[[i]]] = Max[sector2d[sector], sector2d[done[[i]]]];
              sector2s[done[[i]]] = Max[sector2s[sector], sector2s[done[[i]]]];
              ];
          ,
          {sector, s2sectors[s]}
      ];
  ];
  (* We need to make sure each sector has more integrals than
   * masters, otherwise Kira will have nothing to work with, and
   * we'll miss masters in the IBP table.
   *)
  Do[
      sector2s[sector] = Max[sector2s[sector], 1];
      sector2d[sector] = Max[sector2d[sector], 1];
      sector2r[sector] = Max[sector2r[sector], DigitCount[sector, 2, 1]];
      ,
      {sector, sectors}];
  Print["Final sectors:"];
  Do[
    Print["- ", sector, " ", IntegerDigits[sector, 2, Length[First[idxlist]]]//Reverse, ", nprops=", DigitCount[sector, 2, 1], ", r=", sector2r[sector], ", s=", sector2s[sector], ", d=", sector2d[sector]];
    ,
    {sector, sectors}];
  Table[
    <|"id" -> sector, "r" -> sector2r[sector], "s" -> sector2s[sector], "d" -> sector2d[sector]|>
    ,
    {sector, sectors}]
]

MkKiraIntegralFamiliesYaml[filename_, ibpbasis_List, topsectors_] := Module[{loopmom, extmom, dens, basis},
  MkFile[filename,
    "integralfamilies:\n",
    Table[
      loopmom = basis[[2]];
      extmom = basis[[3]];
      dens = basis[[4]];
      {
        "  - name: \"", KiraBasisName[basis[[1]]], "\"\n",
        (*"    # ", dens // Riffle[#, " "]&, "\n",*)
        "    loop_momenta: [", Riffle[loopmom, ", "], "]\n",
        "    top_level_sectors: [", dens // MapReplace[den[_, _, irr] -> 0, _den -> 1] // IndicesToSectorId, "]\n",
        "#    magic_relations: true\n",
        "    propagators:\n",
        dens // Map[Replace[{
          den[p_] | den[p_, 0, ___] :> {"      - [\"", CForm[p], "\", 0]\n"},
          den[p_, m_, ___] :> {"      - [\"", CForm[p], "\", \"", CForm[m /. sp[q] -> qq], "\"]\n"},
          d_ :> Error["MkKiraConfig: bad denominator form: ", d]
        }]],
        "    cut_propagators: [",
        Riffle[Range[Length[dens]] // Select[MatchQ[dens[[#]], den[_, _, cut]]&], ", "],
        "]\n"
      }
      ,
      {basis, ibpbasis}]
  ];
]

MkKiraJobsYaml[filename_, bids_List, topsectors_] := Module[{bid, sector},
  MkFile[filename,
    "jobs:\n",
    " - reduce_sectors:\n",
    "    reduce:\n",
    Table[
        {"     - {topologies: [", KiraBasisName[bid], "], sectors: [", sector["id"], "], r: ", sector["r"], ", s: ", sector["s"], "}\n"}
        ,
        {bid, bids},
        {sector, topsectors[bid]}],
    "    select_integrals:\n",
    "     select_mandatory_list:\n",
    Table[
        {"      - [", KiraBasisName[bid], ", \"", KiraBasisName[bid], ".integrals\"]\n"}
        ,
        {bid, bids}],
    "#     select_mandatory_recursively:\n",
    Table[
        {
        "#      - {topologies: [", KiraBasisName[bid],
            "], sectors: [", sector["id"],
            "], r: ", sector["r"],
            ", s: ", sector["s"],
            ", d: ", sector["d"], "}\n"},
        {bid, bids},
        {sector, topsectors[bid]}],
    "    integral_ordering: 8\n",
    "    run_symmetries: true\n",
    "    run_initiate: true\n",
    "    run_triangular: true\n",
    "    run_back_substitution: true\n",
    " - kira2math:\n",
    "    target:\n",
    Table[
      {"     - [", KiraBasisName[bid], ", \"", KiraBasisName[bid], ".integrals\"]\n"},
      {bid, bids}],
    Table[
        {"#     - {topologies: [", KiraBasisName[bid], "], sectors: [", sector["id"], "], r: ", sector["r"], ", s: ", sector["s"], ", d: ", sector["d"], "}\n"},
        {bid, bids},
        {sector, topsectors[bid]}],
    "    reconstruct_mass: false\n",
    "    integral_ordering: 8\n"
  ];
]

KiraIBP[ampb_, ibpbasis_List, spmap_List] := Module[{blist, confdir, result},
  confdir = FileNameJoin[{$TemporaryDirectory, MkString["kira.", Environment["USER"], ".", $ProcessID]}];
  EnsureNoDirectory[confdir];
  EnsureDirectory[confdir <> "/config"];
  extmom = ibpbasis[[1, 3]];
  blist = CaseUnion[ampb, _B];
  topsectors = Table[
    idxlist = blist // CaseUnion[B[bid, idx__] :> {idx}];
    MkFile[confdir <> "/" <> KiraBasisName[bid] <> ".integrals",
      idxlist // Map[{"- [", Riffle[#, ","], "]\n"}&]
    ];
    bid -> (idxlist // TopSectors // Sort)
    ,
    {bid, ibpbasis[[;;,1]]}] // Association;
  MkKiraKinematicsYaml[confdir <> "/config/kinematics.yaml", extmom, spmap];
  MkKiraIntegralFamiliesYaml[confdir <> "/config/integralfamilies.yaml", ibpbasis, topsectors];
  MkKiraJobsYaml[confdir <> "/jobs.yaml", ibpbasis[[;;,1]], topsectors];
  If[Run[MkString["./kira.sh ", confdir, "/jobs.yaml"]]//TM//# =!= 0&,
    Error["Failed to run kira"];
  ];
  result = KiraApplyResults[ampb, confdir, ibpbasis];
  EnsureNoDirectory[confdir];
  result
]

MkKiraConfig[exb_, ibpbasis_List, spmap_List, confdir_String] := Module[{extmom, blist, topsectors, idxlist, bid},
  EnsureNoDirectory[confdir];
  EnsureDirectory[confdir <> "/config"];
  extmom = ibpbasis[[1, 3]];
  blist = CaseUnion[exb, _B];
  topsectors = Table[
    idxlist = blist // CaseUnion[B[bid, idx__] :> {idx}];
    MkFile[confdir <> "/" <> KiraBasisName[bid] <> ".integrals",
      idxlist // Map[{"- [", Riffle[#, ","], "]\n"}&]
    ];
    bid -> (idxlist // TopSectors // Sort)
    ,
    {bid, ibpbasis[[;;,1]]}] // Association;
  MkKiraKinematicsYaml[confdir <> "/config/kinematics.yaml", extmom, spmap];
  MkKiraIntegralFamiliesYaml[confdir <> "/config/integralfamilies.yaml", ibpbasis, topsectors];
  MkKiraJobsYaml[confdir <> "/jobs.yaml", ibpbasis[[;;,1]], topsectors];
]

MkKiraMultiConfig[exb_, ibpbasis_List, spmap_List, confdir_String] := Module[{basisgroups, group, basis},
  EnsureNoDirectory[confdir];
  basisgroups = ibpbasis // GroupBy[IBPBasisStructureId];
  Do[
    bases = basisgroups[group];
    MkKiraConfig[exb // CaseUnion[B[Alternatives @@ bases[[;;,1]], ___]], bases, spmap, confdir <> "/" <> group];
    ,
    {group, basisgroups // Keys}];
]

KiraApplyResults[ex_, confdir_String, ibpbasis_List] :=
Module[{exx = ex, bids, bvarmap, bid, ibpmapfiles, bvar, table, bmap},
  bids = ex // CaseUnion[B[bid_, __] :> bid];
  bvarmap = Table[
    bvar = KiraBasisName[bid] // ToExpression;
    (bvar[idx__] :> B[$BID, idx]) /. $BID -> bid
    ,
    {bid, Length[ibpbasis]}
  ];
  Do[
    Print["* Loading IBP tables for basis ", bid];
    ibpmapfiles = MkString[confdir, "/results/", KiraBasisName[bid], "/kira_", KiraBasisName[bid], ".integrals.m"] // FileNames;
    FailUnless[Length[ibpmapfiles] === 1];
    table = ibpmapfiles // First // SafeGet;
    table = table /. bvarmap // TM;
    BMapLoad[bmap, table] // TM;
    (*
    Print["Supposed Kira masters:"];
    Print[MkString[confdir, "/results/", KiraBasisName[bid], "/masters.final"] // ReadString // StringTrim];
    *)
    Print["Masters: "];
    BMapMasters[bmap] // Map[Print["- ", #]&];
    Print["* Applying the IBP tables for basis ", bid];
    table = {};
    exx = exx // BMapApply[bmap];
    BMapClear[bmap];
    ,
    {bid, bids}
  ];
  exx
]

LoadKiraSectorMappings[nmomenta_Integer, filename_String] :=
Module[{text, sector1, mommap, jacobian, sector2, bid2},
  (* Example:
   *   14 l1 l1 l2 l2+l1 1 14 3 0 {place==holder} {place==holder} -1 1 2 3 -1 0 0
   * Decoding:
   * - 14: i=0..2^jule ???
   * - l1 l1 l2 l2+l1: change of variables {l1 = l1, l2 = l2+l1};
   * - 1: the jacobian
   * - 14: sector
   * - 3: n of props
   * - 0: ext symm???
   * - {place==holder} ext sym???
   * - {place==holder} ext sym???
   * - -1 1 2 3 -1: ing[g], g = 0..jule-1
   * - 0: symDOTS
   * - 0: topology
   *)
  text = Import[filename, "Text"];
  If[text === $Failed, Error["Failed to read the sectorRelations file"]];
  StringSplit[text, "\n"] // Map[StringSplit] // Map[(
    sector1 = #[[1]]//ToExpression;
    mommap = Table[#[[2+2*i]]->#[[2+2*i+1]], {i, 0, nmomenta-1}] // Map[ToExpression, #, {2}]&;
    jacobian = #[[2 + nmomenta*2]]//ToExpression;
    sector2 = #[[3 + nmomenta*2]]//ToExpression;
    bid2 = (#[[-1]]//ToExpression) + 1;
    {sector1, bid2, sector2, jacobian, mommap}
  )&] \
    // SortBy[{-#[[1]], -#[[4]], #[[5, ;;, 2]] // Map[TermCount] // Apply[Plus]}&]
]

(* Return a list of momenta maps for each amplitude that removes
 * symmetric duplicates among the denominator sets.
 *
 * Amplitudes are defined by the sets of den[__] objects inside,
 * otherwise their structure does not matter.
 *
 * This is slow; use SymmetryMaps[] in stead.
 *)
KiraSymmetryMaps[amplitudes_List, loopmom_List, extmom_List] :=
Module[{densets, uniqdensets, densetindices, ibpbasis, confdir, dens, topsector, text, sector1, bid2, sector2, jacobian, mommap},
  densets = amplitudes // Map[
    CaseUnion[_den] /* NormalizeDens /* Union /* Select[NotFreeQ[Alternatives@@loopmom]]
  ];
  {uniqdensets, densetindices} = UniqueSupertopologyMapping[densets];
  Print["Got a total of ", uniqdensets//Length, " denominator sets"];
  ibpbasis = Table[CompleteIBPBasis[uniqdensets[[i]], loopmom, extmom, i], {i, Length[uniqdensets]}];
  confdir = "kira-sym";
  Quiet[DeleteDirectory[confdir, DeleteContents->True], {DeleteDirectory::nodir}];
  Quiet[CreateDirectory[confdir], {CreateDirectory::filex}];
  Quiet[CreateDirectory[confdir <> "/config"], {CreateDirectory::filex}];
  MkFile[confdir <> "/jobs.yaml",
    "jobs:\n",
    " - reduce_sectors:\n",
    "    integral_ordering: 6\n",
    "    run_symmetries: true\n",
    "    run_initiate: false\n",
    "    run_pyred: false\n",
    "    run_triangular: false\n",
    "    run_back_substitution: false\n"
  ];
  MkFile[confdir <> "/config/kinematics.yaml",
    "kinematics:\n",
    " incoming_momenta: [", extmom // Riffle[#, ", "]&, "]\n",
    " kinematic_invariants:\n",
    "  - [qq,  2]\n",
    "  - [mt2,  2]\n",
    "  - [x, 0]\n",
    " scalarproduct_rules:\n",
    "  - [[q,q], qq]\n",
    " symbol_to_replace_by_one: qq"
  ];
  MkFile[confdir <> "/config/integralfamilies.yaml",
    "integralfamilies:\n",
    Table[
      dens = ibpbasis[[bid, 4]];
      topsector = Table[If[MatchQ[dens[[i]], den[_, _, irr]], 0, 2^(i-1)], {i, Length[dens]}]
        // Apply[Plus];
      {
        "  - name: \"", KiraBasisName[bid], "\"\n",
        "    loop_momenta: [", Riffle[loopmom, ", "], "]\n",
        "    top_level_sectors: [", topsector, "]\n",
        "    propagators:\n",
        dens // Map[Replace[{
          den[p_] | den[p_, 0, ___] :> {"      - [\"", CForm[p], "\", 0]\n"},
          den[p_, m_, ___] :> {"      - [\"", CForm[p], "\", \"", CForm[m /. sp[q] -> qq], "\"]\n"},
          d_ :> Error["MkKiraConfig: bad denominator form: ", d]
        }]],
        "    cut_propagators: [",
        Riffle[Range[Length[dens]] // Select[MatchQ[dens[[#]], den[_, _, cut]]&], ", "],
        "]\n"
      }
      ,
      {bid, Length[ibpbasis]}]
  ];
  If[Run[MkString["./kira.sh ", confdir, "/jobs.yaml"]]//TM//# =!= 0&,
    Error["Failed to run kira"];
  ];
  uniqdensetmaps = Table[
    dens = ibpbasis[[bid, 4]];
    topsector = Table[If[MatchQ[dens[[i]], den[_, _, irr]], 0, 2^(i-1)], {i, Length[dens]}]
      // Apply[Plus];
    (* {{sector1, bid2, sector2, jacobian, mommap}} *)
    topsymmetries = LoadKiraSectorMappings[
      Length[loopmom],
      MkString[confdir, "/sectormappings/", KiraBasisName[bid], "/sectorRelations"]
    ] // Cases[{topsector, _, _, _, _}];
    If[Length[topsymmetries] > 0,
      {sector1, bid2, sector2, jacobian, mommap} = First[topsymmetries];
      Print["Map: ", KiraBasisName[bid], "[", sector1, "] = ", KiraBasisName[bid2], "[", sector2, "]: ", mommap];
      If[jacobian =!= 1,
        Print["WARNING: Jacobian for that map is ", jacobian, "; does this matter?"];
      ];
      If[AllTrue[mommap, MatchQ[a_ -> a_]],
        {}
        ,
        mommap
      ]
      ,
      {}
    ]
    ,
    {bid, Length[ibpbasis]}];
  (uniqdensetmaps // Map[DeleteCases[a_->a_]])[[densetindices]]
]

(* Return a list of momenta maps, such that applying them to
 * the list of feynman integral families makes symmetries and
 * subtopology relations explicit. So, families that are symmetric
 * will have identical sets of denominators after the maps are
 * applied. Families that are symmetric to a subtopology of a
 * bigger family will have a subsets of the denominators.
 *
 * The families are defined by their set of den[]s.
 *
 * You can use UniqueSupertopologyMapping[] to figure out the
 * topmost supertopologies after this.
 *)
SymmetryMaps[families_List, loopmom_List, extmom_List] :=
Module[{densets, uniqdensets, densetindices, uniqdensetmaps},
  densets = families // NormalizeDens // MapWithCliProgress[
    CaseUnion[_den] /* Union /* Select[NotFreeQ[Alternatives@@loopmom]]
  ];
  {uniqdensets, densetindices} = UniqueSupertopologyMapping[densets];
  Print["Got a total of ", uniqdensets//Length, " unique denominator sets"];
  uniqdensetmaps = RunThrough["./feynson symmetrize -j4 -q -", {
    uniqdensets // Map[MapReplace[den[p_] :> p^2, den[p_, m_] :> p^2-m, den[p_, m_, cut] :> p^2-m-CUTM]],
    loopmom,
    {{q^2,1}}
  }] // Map[Map[Apply[Rule]]];
  (*
  FailUnless[
    Count[uniqdensetmaps, {}]
    <=
    MapThread[ReplaceAll, {uniqdensets, uniqdensetmaps}] // NormalizeDens // ExpandAll // UniqueSupertopologyMapping // #[[1]]&//Length
    ==
    MapThread[ReplaceAll, {densets, uniqdensetmaps[[densetindices]]}] // NormalizeDens // ExpandAll // UniqueSupertopologyMapping // #[[1]]&//Length
  ];
  *)
  uniqdensetmaps[[densetindices]]
];
(* Compute SymmetryMaps[], apply them, and return the result. *)
SymmetrizeAmplitudes[families_List, loopmom_List, extmom_List] :=
  MapThread[ReplaceAll, {families, SymmetryMaps[families, loopmom, extmom]}]

(* Same, but guarantee order. *)
SymmetryMapsX[families_List, loopmom_List, extmom_List, sprules_] :=
Module[{densets, uniqdensets, densetindices, uniqdensetmaps},
  densets = families // NormalizeDens // Map[
    CaseUnion[_den] /* Union /* Select[NotFreeQ[Alternatives@@loopmom]]
  ];
  densets = densets /. den[p_] :> p^2 /. den[p_, m_] :> p^2-m /. den[p_, m_, cut] :> p^2-m-CUT;
  RunThrough["tee feynson.in", {densets, loopmom, sprules // Map[Apply[List]]}];
  RunThrough["./feynson symmetrize -j4 -q -", {densets, loopmom, sprules // Map[Apply[List]]}] // Map[Map[Apply[Rule]]]
];
SymmetryMapsX[families_List, loopmom_List, extmom_List] := SymmetryMapsX[families, loopmom, extmom, {q^2->1}]

(* Out of a list of expressions choose the ones that can be
 * mappend into a given IBP basis by a symmetry transformation,
 * and return them transformed.
 *
 * The expressions are recognized by the set of den[...] inside.
 *)
MapToBasis[IBPBasis[bid_, loopmom_List, extmom_List, dens_List, denmap_, dotmap_], exprs_List] :=
Module[{densets, symmaps},
  densets = exprs // Map[CaseUnion[_den] /* Select[NotFreeQ[Alternatives @@ loopmom]]];
  symmaps = Join[{dens // DeleteCases[den[_,_,irr]]}, densets] // SymmetryMapsX[#, loopmom, extmom]&;
  FailUnless[symmaps[[1]] === {}];
  symmaps = symmaps[[2;;]];
  MapThread[If[SubtopologyQ[dens, #1/.#2//NormalizeDens], #3/.#2//NormalizeDens, Nothing]&, {densets, symmaps, exprs}]
]


(* Slow; use this only for double-checking. *)
KiraZeroSectors[IBPBasis[bid_, loopmom_List, extmom_List, dens_List, denmap_, dotmap_]] :=
Module[{confdir, topsector, zerosectors},
  confdir = "kira-intra-symmetries";
  Quiet[DeleteDirectory[confdir, DeleteContents->True], {DeleteDirectory::nodir}];
  Quiet[CreateDirectory[confdir], {CreateDirectory::filex}];
  Quiet[CreateDirectory[confdir <> "/config"], {CreateDirectory::filex}];
  MkFile[confdir <> "/jobs.yaml",
    "jobs:\n",
    " - reduce_sectors:\n",
    "    integral_ordering: 6\n",
    "    run_symmetries: true\n",
    "    run_initiate: false\n",
    "    run_pyred: false\n",
    "    run_triangular: false\n",
    "    run_back_substitution: false\n"
  ];
  MkFile[confdir <> "/config/kinematics.yaml",
    "kinematics:\n",
    " incoming_momenta: [", Riffle[extmom, ", "], "]\n",
    " kinematic_invariants: []\n",
    " scalarproduct_rules:\n",
    "  - [[q,q], 1]\n"
  ];
  topsector = Table[If[MatchQ[dens[[i]], den[_, _, irr]], 0, 2^(i-1)], {i, Length[dens]}]
    // Apply[Plus];
  MkFile[confdir <> "/config/integralfamilies.yaml",
    "integralfamilies:\n",
      {
        "  - name: \"", KiraBasisName[bid], "\"\n",
        "    loop_momenta: [", Riffle[loopmom, ", "], "]\n",
        "    top_level_sectors: [", topsector, "]\n",
        "    propagators:\n",
        dens // Map[Replace[{
          den[p_] | den[p_, 0, ___] :> {"      - [\"", CForm[p], "\", 0]\n"},
          den[p_, m_, ___] :> {"      - [\"", CForm[p], "\", \"", CForm[m], "\"]\n"},
          d_ :> Error["MkKiraConfig: bad denominator form: ", d]
        }]],
        "    cut_propagators: [",
        Riffle[Range[Length[dens]] // Select[MatchQ[dens[[#]], den[_, _, cut]]&], ", "],
        "]\n"
      }
  ];
  If[Run[MkString["./kira.sh ", confdir, "/jobs.yaml"]]//TM//# =!= 0&,
    Error["Failed to run kira"];
  ];
  zerosectors = \
    Import[MkString[confdir, "/sectormappings/", KiraBasisName[bid], "/trivialsector"], "Text"]
      // MkExpression["{", #, "}"]&
      // SortBy[DigitCount[#, 2]& /* Minus]
      // Map[SectorIdToIndices[#, Length[dens]]&]
      // Map[B[bid, Sequence@@#]&];
  zerosectors
]

GetKiraIBPMap[basedir_String, ibpbasis_List, bid_Integer] := Module[{bvarmap, filename, b},
  bvarmap = Table[
    bvar = KiraBasisName[b] // ToExpression;
    (bvar[idx__] :> B[$BID, idx]) /. $BID -> b
    ,
    {b, ibpbasis[[;;,1]]}
  ];
  filename = MkString[basedir, "/*/results/",
    KiraBasisName[bid], "/kira_", KiraBasisName[bid], ".integrals.m"] // FileNames // First;
  (*Print["* Loading Kira IBP tables for basis ", bid, " from ", filename];*)
  filename // SafeGet // ReplaceAll[bvarmap]
]

LoadFullKiraBMap[bmap_Symbol, basedir_String, ibpbasis_List] := Module[{bvarmap, filename, b},
  bvarmap = Table[
    bvar = KiraBasisName[b] // ToExpression;
    (bvar[idx__] :> B[$BID, idx]) /. $BID -> b
    ,
    {b, ibpbasis[[;;,1]]}
  ];
  MkString[basedir, "/*/results/*/kira_*.m"] // FileNames // Map[SafeGet /* ReplaceAll[bvarmap] /* (BMapLoad[bmap, #]&)];
]

ZeroSectors[IBPBasis[bid_, loopmom_List, extmom_List, dens_List, denmap_, dotmap_], spmap_] :=
  RunThrough["./feynson zero-sectors -sqj3 -", {
      dens /. den[p_] :> p^2 /. den[p_, m_] :> p^2-m /. den[p_, m_, cut] :> p^2-m-CUTM /. den[p_, m_, irr] :> p^2-m /. sp[q]->q^2,
      dens /. den[_, _, cut] -> 1 /. den[___] -> 0,
      loopmom,
      spmap /. Dot->Times /. Rule->List
    }] // Map[B[bid, Sequence@@SectorIdToIndices[#, Length[dens]]]&]

ZeroSectors[b_IBPBasis] := ZeroSectors[b, {q^2->1}]

ZeroSectorPattern[b_IBPBasis, spmap_] := ZeroSectors[b, spmap] //
  MapAt[Replace[{0->_?NonPositive,1->_}], #, {;;, 2;;}]& // Apply[Alternatives]

SubtopologyQ[superdenset_List, denset_List] := And[
    Count[superdenset, den[_, _, cut]] === Count[denset, den[_, _, cut]],
    SubsetQ[superdenset, denset]
  ]

UniqueSupertopologyMapping[densets_List] := UniqueSupersetMapping[densets, SubtopologyQ]

DenMassTerm[den[p_]] := 0
DenMassTerm[den[p_, m_, ___]] := m
DenMassTerm[ex_] := Error["What's the mass term here: ", ex]

(* Differentiate a B[...] by var; only works for mass terms. *)
DiffB[var_] := DiffB[#, var]&
DiffB[B[bid_, indices__], var_] := Module[{idx, i, j},
  idx = {indices};
  Sum[
    Times[
      idx[[i]],
      D[DenMassTerm[ibpbasis[[bid, 4, i]]], var],
      B[bid, Sequence @@ (idx + Table[If[j === i, 1, 0], {j, Length[idx]}])]
    ],
    {i, Length[idx]}
  ]
]
DiffB[b_B*ex_?(FreeQ[#, _B] &), var] := DiffB[b, var]*ex + b*D[ex, var]
DiffB[ex_Plus, var_] := Map[DiffB[var], ex]
DiffB[ex_List, var_] := Map[DiffB[var], ex]
DiffB[ex_, var_] := Error["Failed to differentiate by ", var, ": ", ex]

IBPBasisStructureId[IBPBasis[_, _, _, dens_, _, _]] := dens // Count[den[_, 0, cut]] // MkString["cut-", #]&

(* FIRE I/O *)

MkFireRulesFile[filename_String, rules_List] :=
  MkFile[filename,
    rules // MapReplace[(B[bid_, idx__] -> ex_) :>
      {
        G[bid, {idx}] -> (Bracket[ex, _B, Factor /* COEF, STEM] //
          Terms // MapReplace[COEF[c_]*STEM[B[bid2_, idx2__]] :> {c, G[bid2, {idx2}]}]) // InputForm,
        ";\n\n"
      }
    ]
  ]

MkFireIntegralsFile[filename_String, ints_List] :=
  SafePut[ints // MapReplace[B[bid_, idx__] :> {bid, {idx}}], filename]

GetFireMasters[filename_String] := filename // SafeGet // #[[2, ;;, 2]]& // MapReplace[{bid_, idx_List} :> B[bid, Sequence @@ idx]]

LoadFireTables[filename_, coeff_: Identity, JoinTerms_: True] := Module[{temp, GGG, data},
    data = SafeGet[filename];
    temp = {GGG[##[[1]]], {GGG[##[[1]]], ##[[2]]} & /@ ##[[2]]} & /@ data[[1]];
    Set[GGG[##[[1]]], G[##[[2, 1]], ##[[2, 2]]]] & /@ data[[2]];
    temp = temp;
    Clear[GGG];
    temp = DeleteCases[temp, {a_, {{a_, "1"}}}];
    temp = {##[[1]], {##[[1]], ToExpression[##[[2]]]} & /@ ##[[2]]} & /@ temp;
    temp = {##[[1]], {##[[1]], coeff[##[[2]]]} & /@ ##[[2]]} & /@ temp;
    If[JoinTerms,
        temp = {##[[1]], Times @@@ ##[[2]]} & /@ temp;
        temp = {##[[1]], Plus @@ ##[[2]]} & /@ temp;
    ];
    Rule @@@ temp // ReplaceAll[G[bid_, idx_List] :> B[bid, Sequence @@ idx]]
 ]

(*
 * GINAC
 *)
Ginsh[ex__] := Module[{tmpsrc, tmpdst, result},
  tmpsrc = MkTemp["g", ".ginsh"];
  tmpdst = tmpsrc <> ".out";
  MkFile[tmpsrc, ex, ";exit;"];
  SafeRun["ginsh ", tmpsrc, " >", tmpdst];
  result = ReadString[tmpdst];
  If[Not[StringQ[result]], Error["Failed to load ", tmpdst]];
  DeleteFile[{tmpsrc, tmpdst}];
  result // StringReplace["E" -> "*10^"] //
    StringSplit[#, "output"][[-1]]& //
    ToExpression
]
GinshEval[x:G[w__, 1], ndigits_] := GinshEval[x, ndigits] =
  N[Ginsh[MkString["Digits=", ndigits, ";output;evalf(G({", Riffle[{w},","], "},1))"]], ndigits]
GinshEval[x:Mzv[w__], ndigits_] := GinshEval[x, ndigits] =
  N[Ginsh[MkString["Digits=", ndigits, ";output;evalf(zeta({",
    {w} // Abs // Riffle[#, ","]&, "},{",
    {w} // Sign // Riffle[#, ","]&, "}))"]], ndigits]
GinshEval[ex_, ndigits_] := ex // ReplaceAll[x:(G[__, 1]|Mzv[__]) :> GinshEval[x, ndigits]]
GinshEval[ndigits_] := GinshEval[#, ndigits]&

ToGinsh[x:G[w__, 1], ndigits_] :=
  {"Digits=", ndigits, ";output;evalf(G({", Riffle[{w},","], "},1))"}
ToGinsh[x:Mzv[w__], ndigits_] :=
  {"Digits=", ndigits, ";output;evalf(zeta({",
    {w} // Abs // Riffle[#, ","]&, "},{",
    {w} // Sign // Riffle[#, ","]&, "}))"}
GinshPEvalMemo[ex_, ndigits_] := ex
GinshPEval[ex_, ndigits_, maxjobs_] := Module[{tmpsrc, tmpdst, vals},
  ex /. x:(G[__, 1]|Mzv[__]) :> GinshPEvalMemo[x, ndigits] // SubexpressionApply[(
    tmpsrc0 = MkTemp["g", ".cmd"];
    tmpsrc = # // Map[MkTemp["g", ".sh"]&];
    tmpdst = tmpsrc // Map[# <> ".out"&];
    MapThread[MkFile, {tmpsrc, # // Map[{ToGinsh[#, ndigits], ";exit;"}&]}];
    MkFile[tmpsrc0, MapThread[{"ginsh <", #1, " >", #2, "\n"}&, {tmpsrc, tmpdst}]];
    SafeRun[MkString["cat ", tmpsrc0, " | xargs -P", maxjobs, " -L1 -IARG sh -c 'ARG'"]];
    vals = tmpdst // Map[
      ReadString /*
      StringReplace["E" -> "*10^"] /*
      (StringSplit[#, "output"][[-1]]&) /*
      ToExpression /*
      (N[#, ndigits]&)
    ];
    DeleteFile[Join[tmpsrc, tmpdst, {tmpsrc0}]];
    MapThread[(GinshEval[#1, ndigits] = #2)&, {#, vals}];
    MapThread[(GinshPEvalMemo[#1, ndigits] = #2)&, {#, vals}];
    vals
  )&, #, G[__, 1]|Mzv[__]]&
]

(* PSLQ *)

MzvLinearBasis[mzvs_List, {0}] := 1
MzvLinearBasis[mzvs_List, {weight_}] := Module[{wtomzv},
  wtomzv = mzvs // GroupBy[MaxZetaWeight];
  IntegerPartitions[weight] /. wtomzv // Cases[{{(_Mzv|_Log) ..} ..}] //
    Map[Apply[Outer[Times, ##]&]] // Flatten // Union
]
MzvLinearBasis[mzvs_List, weight_] := Range[0, weight] // Map[MzvLinearBasis[mzvs, {#}]&] // Flatten

PSLQ[x_, basis_List, basisvalues_List] := Module[{ndigits, cbound, nv},
  FailUnless[Length[basis] === Length[basisvalues]];
  ndigits = basisvalues // Map[Precision] // Min // Floor;
  cbound = ndigits/(Length[basis] + 1)*0.9//Floor;
  Print["PSLQ: basis size ", Length[basis], ", ", ndigits, " digits; max coeff: 10^", cbound];
  nv = Quiet[Check[
    FindIntegerNullVector[Append[basisvalues, x], 10^cbound],
    $Failed,
    {FindIntegerNullVector::norel, FindIntegerNullVector::rnf, FindIntegerNullVector::rnfb}
  ],
  {FindIntegerNullVector::norel, FindIntegerNullVector::rnf, FindIntegerNullVector::rnfb}];
  If[nv === $Failed,
    Print["PSLQ: *FAILED* to find a relation"];
    $Failed
    ,
    Print["PSLQ: resulting relation norm: 10^", Norm[nv] // Log10 // Ceiling // N];
    FailUnless[nv[[-1]] =!= 0];
    -basis.nv[[;;-2]]/nv[[-1]]
  ]
]
