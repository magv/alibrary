(* # Amplitude library (`library.m`)
 *)

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
FasterFactor[ex_^n_] := FasterFactor[ex]^n
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

(* Determine the rank of a matrix using modular arithmetics, fast.
 *)
FastMatrixRank[mx_] := FastMatrixRank[mx, mx // CaseUnion[_Symbol]]
FastMatrixRank[mx_, vars_List] := Module[{mnx},
  Table[
    mxn = mx /. Association[MapThread[Rule, {vars, RandomInteger[{2^20, 2^30}, Length[vars]]}]];
    MatrixRank[mxn, Modulus->RandomPrime[{2^30, 2^31-1}]]
    ,
    5
  ] // Max
];

(* Given a list of vectors, select a linearly independent subset,
 * return the indices of the vectors in it. *)
SelectAnyBasis[vectors_List] := Module[{vars, dim, basisidx, basis, i, newbasis, rank},
  vars = basis // CaseUnion[_Symbol];
  dim = Length[vectors[[1]]];
  basis = {};
  basisidx = {};
  Do[
    Print[i];
    newbasis = Append[basis, vectors[[i]]];
    rank = FastMatrixRank[newbasis, vars];
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

(* Amplitudes & Feynman Rules *)

AmpConjugateMomenta[ex_] := ex \
  /. l1 -> r1 /. l2 -> r2 /. l3 -> r3 /. l4 -> r4 /. l5 -> r5 \
  /. l6 -> r6 /. l7 -> r7 /. l8 -> r8 /. l9 -> r9 /. l10 -> r10
AmpConjugate[ex_] := (ex
  (*
  (* Spin indices *must* all be internal, because there is a
   * gammachain[spinor, spn[-1], X[-1]] at each external line of
   * fermions: here spn[-1] must be renamed, and X[-1] mustn’t.
   *)
  /. (idx : spn)[i_] :> idx[2000 + i]
  (* The rest of the indices are external if negative, and
   * internal otherwise.
   *)
  /. (idx : flv | lor | fun | adj)[i_] :> idx[If[TrueQ[i >= 0], 5000 + i, i]]
  *)
  /. (idx : flv | lor | fun | adj | spn)[i_] :> idx[If[TrueQ[i < 0], -5000 + i, 5000 + i]]
  /. Complex[re_, im_] :> Complex[re, -im]
  /. (chain : gammachain | colorT)[m___, i_, j_] :> chain[Sequence @@ Reverse[{m}], j, i]
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

(* Remove factors that don’t have indices inside from each
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
    List // RunThroughForm[FormCall["flavorsum"]] // First
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
Denominators[p_P] := Amplitude[p] // CaseUnion[_den]
Denominators[d_Diagram] := d // DiagramPropagators // Map[Denominators] // Apply[Join] // NormalizeDens
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

IBPBasisSanityCheck[ibpbasis_List] := (Map[IBPBasisSanityCheck, ibpbasis];)
IBPBasisSanityCheck[IBPBasis[bid_, loopmom_List, extmom_List, dens_List, denmap_, dotmap_]] :=
Module[{normaldens, i, dots, p1, p2, p, m},
  normaldens = dens // DeleteCases[den[_, _, irr]];
  FailUnless[(normaldens /. denmap) === Table[DEN[i], {i, Length[normaldens]}]];
  FailUnless[(MapAt[Minus, normaldens, {;;,1}] /. denmap) === Table[DEN[i], {i, Length[normaldens]}]];
  dots = Table[{p1.p2, p2.p1}, {p1, loopmom}, {p2, Join[loopmom, extmom]}] // Flatten // Union;
  FailUnless[Map[Sort, dots] === Together[dots /.
    dotmap /. DEN[i_] :> dens[[i]] /.
    den[p_] :> 1/p.p /. den[p_, m_, ___] :> 1/(p.p-m) /.
    Dot->ExpandDot /. sp[p1_] :> p1.p1 /. sp[p1_, p2_] :> p1.p2
  ]];
]
IBPBasisSanityCheck[ex_] := Error["Not an IBPBasis: ", ex]

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

(* ## Epsilon form solutions
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

(* Out of a list of expressions choose the ones that can be
 * mappend into a given IBP basis by a symmetry transformation,
 * and return them transformed.
 *
 * The expressions are recognized by the set of den[...] inside.
 *)
MapToBasis[IBPBasis[bid_, loopmom_List, extmom_List, dens_List, denmap_, dotmap_], exprs_List] :=
Module[{densets, symmaps},
  densets = exprs // Map[CaseUnion[_den] /* Select[NotFreeQ[Alternatives @@ loopmom]]];
  symmaps = Join[{dens // DeleteCases[den[_,_,irr]]}, densets] // SymmetryMaps[#, loopmom, extmom]&;
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

(* Is a set of denominators a subtopology of a different set?
 * Subtopology differs from just a subset by the threatment of
 * cut denominators: a denominator set is only a subtopology if
 * the set of cuts is the same.
 *)
SubtopologyQ[superdenset_List, denset_List] := And[
    Count[superdenset, den[_, _, cut]] === Count[denset, den[_, _, cut]],
    SubsetQ[superdenset, denset]
  ]

(* Same as [[UniqueSupersetMapping]], but using [[SubtopologyQ]]
 * instead of `SubsetQ`.
 *)
UniqueSupertopologyMapping[densets_List] := UniqueSupersetMapping[densets, SubtopologyQ]

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
