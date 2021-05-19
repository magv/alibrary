(* # Amplitude library (`alibrary.m`)
 *
 * Tired of relying on large monolitic programs to generate
 * your Feynman amplitudes? What you need is a library: *alibrary*.
 *
 * This file deals with Feynman diagrams and all the related
 * things. Generic Mathematica utils go into [[utils.m]].
 *
 * ## Contents
 * [[table of contents]]
 *
 * * * *
 *
 * First, load the other libraries, [[utils.m]] and [[library.m]].
 *)

Get["utils.m"];
Get["library.m"];

(*
 * ## Diagrams
 * 
 * We use diagrams generated by QGraf and expect them to come
 * in the following format (defined in `qgraf-stylefile`):
 *
 *     Diagram[id, sym-factor, {in-field ...}, {out-field ...}, {propagator ...}, {vertex ...}]
 *
 * where
 * - id is an arbitrary identified of the diagram;
 * - sym-factor is the symmetry factor and the sign of the diagram;
 * - in- and out-fields: F["field", field-idx, vertex-idx, momentum];
 * - propagators:   P["field", from-field-idx, to-field-idx, from-vertex-idx, to-vertex-idx, momentum]
 * - vertices:      V[vertex-idx, "fields", field-idx-1, momentum-1, field-idx-2, momentum-2, ...]
 *
 * All the information here is directly as QGraf provides it,
 * just packaged into a Mathematica expression.
 *)

(* Because `Diagram[]` objects are not `Association`s, here are
 * some functions that help access their parts by names, instead
 * of numbers. (In many places we will access these parts by
 * numbers still; arguably those places should be changed to use
 * these functions).
 *)
DiagramId[Diagram[id_, factor_, ifld_, ofld_, props_, verts_]] := id

(* Note that we store but don’t trust the sign of the diagram
 * that QGraf produces. Use [[DiagramSign]] to compute the actual
 * sign.
 *)
DiagramSymmetryFactor[Diagram[_, factor_, _, _, _, _]] := Abs[factor]

DiagramIncomingFields[Diagram[_, _, ifld_List, _, _, _]] := ifld

DiagramOutgoingFields[Diagram[_, _, _, ofld_List, _, _]] := ofld

DiagramPropagators[Diagram[_, _, _, _, props_List, _]] := props

DiagramVertices[Diagram[_, _, _, _, _, verts_List]] := verts

(* Return the number of closed loops comprised only of fields
 * that match the given (string) pattern. Only really works
 * for fields that always come in a pair in each vertex (e.g.
 * fermions).
 *)
DiagramClosedLoops[fieldpat_] := DiagramFieldLoops[#, fieldpat]&
DiagramClosedLoops[Diagram[_, _, i_List, o_List, p_List, _], fieldpat_] := Module[{edges, SS, II, EE},
  edges = Join[
    i // Cases[ F[fieldpat, fi_, vi_, _] :> SS[fi] <-> II[vi] ],
    o // Cases[ F[fieldpat, fi_, vi_, _] :> II[vi] <-> EE[fi] ],
    p // Cases[ P[fieldpat, fi1_, fi2_, vi1_, vi2_, _] :> II[vi1] <-> II[vi2] ]
  ];
  edges // ConnectedComponents // Select[FreeQ[_SS|_EE]] // Length
]

(* Compute the sign of a diagram by counting fermion loops.
 *
 * Note that we don’t assume to know the names of all fermion
 * fields, and expect them to be passed in explicitly. The fermion
 * field pattern can be something like `"q"|"Q"|"t"|"T"|"c"|"C"`.
 *)
DiagramSign[d_Diagram, fermionpattern_] := (-1)^DiagramClosedLoops[d, fermionpattern]
DiagramSign[fermionpattern_] := DiagramSign[#, fermionpattern]&

(*
 * ## IBP Bases
 *)
(* Expand `sp[..., ...]` and take out constant factors, so
 * that only `sp` between momenta are left. To know which symbols
 * are momenta, this function takes a list of symbols, or a pattern.
 *)
ExpandScalarProducts[momnames_List] := ExpandScalarProducts[Alternatives @@ momnames]
ExpandScalarProducts[mompattern_] := ReplaceAll[sp[a_, b_] :> (
  Expand[a*b] // ReplaceAll[{
    (l:mompattern) (k:mompattern) :> Sort[sp[l, k]],
    (l:mompattern)^2 :> sp[l, l]
  }]
)]

FailUnless[
  ExpandScalarProducts[x|y|z|q][sp[2x+3y, z]] === 2 sp[x,z] + 3 sp[y,z],
  ExpandScalarProducts[x|y|z|q][sp[x+y, x-y]] === sp[x,x] - sp[y,y],
  ExpandScalarProducts[x|y|z|q][sp[a x + b y, c x]] === a c sp[x,x] + b c sp[x,y]
]

(* Convert an expression with `sp` and `den` to `B` notation in
 * a given basis. This is the slow version of it; the faster one
 * uses FORM: see [[AmpFormRun]] and [[FormFnToB]].
 *)
ToB[basis_Association] := ToB[#, basis]&
ToB[ex_, basis_Association] := Module[{indices},
  ex //
    ExpandScalarProducts[Alternatives @@ Join[basis["externalmom"], basis["loopmom"]]] //
    ReplaceAll[basis["nummap"]] //
    ReplaceAll[basis["denmap"]] //
    ReplaceAll[basis["sprules"]] //
    (*Together //*)
    Bracket[#, _bden, #&, (
      indices = Table[0, Length[basis["denominators"]]];
      # /. bden[i_]^n_. :> (indices[[i]] += n; 1) // #* B[basis["id"], Sequence @@ indices] &
    ) &]&
]

(* List IBP relations for a basis, return a list of equations
 * with terms of the form `B[id, n[1], n[2], ...]` and coefficients
 * that depend on `n[i]`.
 *)
IBPRelations[basis_Association] := Module[{dens, i, l, v},
  dens = basis["denominators"];
  Table[
    Product[bden[i]^(n[i]), {i, Length[dens]}]*(
        Sum[-2 n[i] D[dens[[i, 1]], l] sp[v, dens[[i, 1]]] bden[i], {i, Length[dens]}] +
        If[v === l, d, 0]
      )
    ,
    {l, basis["loopmom"]},
    {v, Join[basis["externalmom"], basis["loopmom"]]}
  ] // Flatten // Map[ToB[basis]] // Bracket[#, _B, Together]&
]

(* Figure out the dimensionality of variables in an expression
 * (or a list of expressions) of a given dimensionality.
 *
 * E.g.:
 *
 *     In[1]:= VariableDimensions[m + m^2/q, 1]
 *     Out[1]= {m -> 1, q -> 1}
 *)
VariableDimensions[{}, _] := {}
VariableDimensions[expression_, dimension_] := Module[{DimOf, DimOfSymbol, ex, eqns, solution},
  DimOf[ex_List] := (
    ex // Map[Sow[DimOf[#] == DimOf[ex[[1]]]]&];
    DimOf[ex[[1]]]
  );
  DimOf[ex_Plus] := (
    ex // Apply[List] // Map[Sow[DimOf[#] == DimOf[ex[[1]]]]&];
    DimOf[ex[[1]]]
  );
  DimOf[ex_Times] := ex // Apply[List] // Map[DimOf] // Apply[Plus];
  DimOf[ex_^n_] := DimOf[ex]*n;
  DimOf[ex_?NumberQ] := 0;
  DimOf[ex_Symbol] := DimOfSymbol[ex];
  DimOf[ex_] := Error["Don't know the dimension of: ", ex];
  eqns = Reap[Sow[DimOf[expression // DeleteCases[0]] == dimension];][[2,1]] // Apply[And];
  If[eqns === False, Error["The dimension of ", expression, " can't be ", dimension]];
  solution = eqns // Solve[#, # // CaseUnion[_DimOfSymbol]]&;
  If[solution === {}, Error["Inconsistent dimension in ", expression]];
  solution[[1]] /. DimOfSymbol[ex_] :> ex
]

(* Complete a list of denominators to a full IBP basis, and
 * return an IBP basis object with all the information. This
 * object is used throughout this library, and this is the main
 * way to create it.
 *
 * Example:
 *
 *     In[]:= CompleteIBPBasis[1, {den[l1], den[q-l1]}, {l1, l2}, {q}, {sp[q,q]->qq}]
 *     Out[]=
 *     <|
 *      "id" -> 1, "loopmom" -> {l1, l2}, "externalmom" -> {q},
 *      "sprules" -> {sp[q,q] -> qq}, "invariants" -> {qq}, "massdimensions" -> {qq -> 2},
 *      "denominators" -> { den[l1], den[l1-q], den[l2,0,irr], den[l1+l2,0,irr], den[l2+q,0,irr] },
 *      "denmap" -> <| den[l1] -> bden[1], ... |>,
 *      "nummap" -> <| sp[l1,l1] -> 1/bden[1], ... |>
 *     |>
 *)
CompleteIBPBasis[bid_, denominators_List, loopmom_List, extmom_List, sprules_List] :=
Module[{L, M, p, k, nums, vars, c, mx, candidatemoms, denadd, numadd, cadd, mxadd, dens, Complete, rels},
  L = loopmom // Map[DropLeadingSign] // Apply[Alternatives];
  M = Join[loopmom, extmom] // Map[DropLeadingSign] // Apply[Alternatives];
  dens = denominators // NormalizeDens // Sort;
  nums = dens /. den[p_] :> p^2 /. den[p_, m2_, ___] :> p^2 - m2 // Expand;
  nums = nums /. (l:M) (k:M) :> Sort[sp[l, k]] /. (l:M)^2 :> sp[l, l] /. sprules;
  vars = Tuples[{loopmom, Join[loopmom, extmom]}] //
    Map[DropLeadingSign /* Sort /* Apply[sp]] //
    Union;
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
    (* Higher counts almost always give more IBP terms; the
     * minimal amount is sufficient in our case (but not in
     * general). *)
    Select[Length[#] < 3&] //
    Join[#, Cases[#, {a_, b_} :> {a, -b}]]& //
    (*
    Join[#, Cases[#, {a_, b_, c_} :> {a, -b, c}]]& //
    Join[#, Cases[#, {a_, b_, c_} :> {a, b, -c}]]& //
    Join[#, Cases[#, {a_, b_, c_} :> {a, -b, -c}]]& //
    *)
    Map[Apply[Plus] /* DropLeadingSign] //
    Select[NotFreeQ[L]];
  Complete[dens_, mx_, c_, mini_] := Module[{i},
    If[Length[mx] === Length[vars],
      (*Print["OK"];*)
      Sow[
        <|
          "id" -> bid,
          "loopmom" -> (loopmom // DropLeadingSign),
          "externalmom" -> (extmom // DropLeadingSign),
          "sprules" -> sprules,
          "denominators" -> dens,
          "denmap" -> (
            MapIndexed[#1 -> bden @@ #2 &, dens] //
            DeleteCases[den[_, _, irr] -> _] //
            ReplaceAll[(den[p_, x___] -> y_) :> {den[p, x] -> y, den[-p, x] -> y}] //
            Flatten //
            Association
          ),
          "nummap" -> (
            Inverse[mx].(Map[1/bden[#] &, Range[Length[mx]]] - c) //
            Bracket[#, _bden, Factor]& //
            MapThread[Rule, {vars, #}]& //
            Join[#, # /. sp[a_, b_] :> sp[b, a]]& //
            Association
          ),
          "invariants" -> (sprules[[;;,2]] // CaseUnion[_Symbol]),
          "massdimensions" -> (sprules[[;;,2]] // VariableDimensions[#, 2]&)
        |>
      ]
      ,
      For[i = mini, i < Length[candidatemoms], i++,
        numadd = Expand[candidatemoms[[i]]^2];
        numadd = numadd /. (l:M) (k:M) :> Sort[sp[l, k]] /. (l:M)^2 :> sp[l, l] /. sprules;
        {cadd, mxadd} = CoefficientArrays[numadd, vars] // Normal;
        If[MatrixRank[Append[mx, mxadd]] === Length[mx] + 1,
          (*Print["ADD: ", candidatemoms[[i]]];*)
          Complete[
            Append[dens, den[candidatemoms[[i]], 0, irr]],
            Append[mx, mxadd],
            Append[c, cadd],
            i+1]
          (*Print["DEL: ", candidatemoms[[i]]];*)
        ]
      ]
    ]
  ];
  Reap[Complete[dens, mx, c, 1]][[2]] // Only //
  MinimalBy[(
    rels = IBPRelations[#] // Bracket[#, _B, CO]& // Map[Terms /* Length];
    {Plus@@rels, Plus@@(rels*rels), #["denominators"][[;;,1]]//Map[Terms/*Length]//Apply[Plus]}
  )&]//First
]

InvariantMapUnderMomentaPermutation[basis_, momperm_] :=
Module[{extmom, sprules, i, j, sps, v1, v2, vars, OLD, NEW, x},
  extmom = basis["externalmom"];
  sprules = basis["sprules"];
  sps = Table[
    Sort[sp[extmom[[i]], extmom[[j]]]]
    ,
    {i, 1, Length[extmom]},
    {j, i, Length[extmom]}
  ] // Apply[Join];
  vars = Lookup[basis, "variables", sprules[[;;,2]]//CaseUnion[_Symbol]];
  v1 = sps /. sprules /. x:(Alternatives@@vars) :> OLD[x];
  v2 = sps /. momperm // Map[Sort] // ReplaceAll[sprules] // ReplaceAll[x:(Alternatives@@vars) :> NEW[x]];
  v1 - v2 //
    Map[#==0&] //
    Solve[#, vars //
    Map[OLD]]& //
    Only //
    ReplaceAll[(NEW|OLD)[x_] :> x] //
    DeleteCases[x_ -> x_]
]

(*
 * ## Feynson
 *)

(* By default look for Feynson in the current directory. *)
If[Not[MatchQ[$Feynson, _String]], $Feynson = "./feynson"; ];

(* Calculate the zero sectors of a given basis. Return a list,
 * each element being `B[basis-id, (1|0), ...]`, listing the topmost
 * zero sectors.
 *)
ZeroSectors[basis_] :=
  RunThrough[$Feynson <> " zero-sectors -sqj3 -", {
      basis["denominators"] /.
        den[p_] :> p^2 /.
        den[p_, m_] :> p^2-m /.
        den[p_, m_, irr] :> p^2-m,
      basis["denominators"] /. den[_, _, cut] -> 1 /. den[___] -> 0,
      basis["loopmom"],
      basis["sprules"] /. Rule->List /. sp -> Times
    }] //
    Map[B[basis["id"], Sequence@@SectorIdToIndices[#, Length[basis["denominators"]]]]&]
ZeroSectors[bases_List] := bases // Map[ZeroSectors] // Apply[Join]

(* Return a pattern that matches zero intergals (in the `B`
 * notation) of a given basis.
 *)
ZeroSectorPattern[basis_] := ZeroSectors[basis] //
  MapReplace[B[bid_, idx__] :> B[bid, {idx} /. 1 -> _ /. 0 -> _?NonPositive // Apply[Sequence]]] //
  Apply[Alternatives]
ZeroSectorPattern[bases_List] := bases // Map[ZeroSectorPattern] // Apply[Alternatives]

(* Return a list of momenta maps, such that applying them to
 * the list of feynman integral families makes symmetries and
 * subtopology relations explicit. So, families that are symmetric
 * will have identical sets of denominators after the maps are
 * applied. Families that are symmetric to a subtopology of a
 * bigger family will have a subsets of the denominators.
 *
 * The families are defined by their set of den[]s.
 *
 * The latter families are guaranteed to be mapped to the former ones.
 *
 * You can use [[UniqueSupertopologyMapping]] to figure out the
 * topmost supertopologies after this.
 *)
SymmetryMaps[families_List, loopmom_List, extmom_List, sprules_] :=
Module[{densets, uniqdensets, densetindices, uniqdensetmaps},
  densets = families // NormalizeDens // Map[
    CaseUnion[_den] /* Union /* Select[NotFreeQ[Alternatives@@loopmom]]
  ];
  densets = densets /. den[p_] :> p^2 /. den[p_, m_] :> p^2-m /. den[p_, m_, cut] :> p^2-m-CUT;
  RunThrough[$Feynson <> " symmetrize -j4 -q -", {densets, loopmom, sprules // Map[Apply[List]]}] // Map[Map[Apply[Rule]]]
];
SymmetryMaps[families_List, loopmom_List, extmom_List] := SymmetryMaps[families, loopmom, extmom, {}]

(* Form function to convert the current expression into B notation.
 * To be used with [[AmpFormRun]]. *)
FormFnToB[bases_List] := MkString[
    "#procedure toBid\n",
    "* Assume Bid^n factor are already supplied.\n",
    "#endprocedure\n",
    "#procedure toBden\n",
    "  ",
    bases // Map[Function[{basis},
      {
        "if (match(only, Bid^", basis["id"], "));\n",
        basis["denmap"] // Normal //
          Map[{
            "    id ", #[[1]]//AmpToForm, " = ",
            #[[2]] /. basis["sprules"] /. bden[n_] :> MkString["Bden", n] // AmpToForm,
            ";\n"
          }&] // Union,
        basis["nummap"] // Normal //
          Map[{
            "    id ", #[[1]] /. sp->(Dot/*Sort) //AmpToForm, " = ",
            #[[2]] /. basis["sprules"] /. bden[n_] :> MkString["Bden", n] // AmpToForm,
            ";\n"
          }&] // Union,
        basis["sprules"] // Normal //
          Map[{
            "    id ", #[[1]] /. sp->(Dot/*Sort) // AmpToForm, " = ",
            #[[2]] /. bden[n_] :> MkString["Bden", n] // AmpToForm,
            ";\n"
          }&] // Union
      }
    ]] // Riffle[#, "  else"]&,
    "  else;\n",
    "    exit \"ERROR: toBden: got a term without a proper Bid^n factor.\";\n",
    "  endif;\n",
    "#endprocedure\n",
    "#call toB(", Length[bases[[1, "denominators"]]], ", toBid, toBden)\n"
  ]

(*
 * ## Kira interface
 *)
(* Kira sorts bases by name instead of adhering to the order
 * of definition. We shall make sure that both the numerical
 * and the lexicographic orders match, which will prevent Kira
 * from messing it up.
 *)
KiraBasisName[bid_] := MkString["b", IntegerDigits[bid, 10, 5]]

(* Create Kira’s `kinematics.yaml` config file.
 *)
MkKiraKinematicsYaml[filename_, extmom_List, sprules_List] :=
  MkFile[filename,
    "kinematics:\n",
    " incoming_momenta: [", extmom // Riffle[#, ", "]&, "]\n",
    " kinematic_invariants:\n",
    sprules[[;;,2]] //
      VariableDimensions[#, 2]& //
      ReplaceAll[(var_ -> dim_) :> {"  - [", var , ", ", dim, "]\n"}],
    " scalarproduct_rules:\n",
    sprules //
      ReplaceAll[sp -> (sp /* Sort)] //
      MapReplace[
      (sp[p_] -> v_) :> {"  - [[", p//InputForm, ",", p//InputForm, "], ", v//InputForm, "]\n"},
      (sp[p1_, p2_] -> v_) :> {"  - [[", p1//InputForm, ",", p2//InputForm, "], ", v//InputForm, "]\n"}
    ] // Union,
    "# symbol_to_replace_by_one: qq"
  ];

(* Create Kira’s `integralfamilies.yaml` config file.
 *)
MkKiraIntegralFamiliesYaml[filename_, bases_List] :=
Module[{loopmom, extmom, dens, basis},
  MkFile[filename,
    "integralfamilies:\n",
    Table[
      loopmom = basis["loopmom"];
      extmom = basis["externalmom"];
      dens = basis["denominators"];
      {
        "  - name: \"", KiraBasisName[basis["id"]], "\"\n",
        "    loop_momenta: [", Riffle[loopmom, ", "], "]\n",
        "    top_level_sectors: [", dens // MapReplace[den[_, _, irr] -> 0, _den -> 1] // IndicesToSectorId, "]\n",
        "    propagators:\n",
        dens // Map[Replace[{
          den[p_] | den[p_, 0, ___] :> {"      - [\"", CForm[p], "\", 0]\n"},
          den[p_, m_, ___] :> {"      - [\"", CForm[p], "\", \"", CForm[m /. sp[q] -> qq], "\"]\n"},
          d_ :> Error["MkKiraConfig: bad denominator form: ", d]
        }]],
        If[NotFreeQ[dens, cut],
          {"    cut_propagators: [",
          Riffle[Range[Length[dens]] // Select[MatchQ[dens[[#]], den[_, _, cut]]&], ", "],
          "]\n"
          }
          ,
          {}
        ]
      }
      ,
      {basis, bases}]
  ];
]

(* Create Kira’s jobs file.
 *)
MkKiraJobsYaml[filename_, bids_List, topsectors_, mode_String] := Module[{bid, sector}, 
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
    "#    preferred_masters: \"preferred-masters\"\n",
    Switch[mode,
      "all", {
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
      },
      "prepare", {
        "    run_symmetries: true\n",
        "    run_initiate: true\n",
        "    run_triangular: false\n",
        "    run_back_substitution: false\n"
      },
      "finish", {
        "    run_symmetries: false\n",
        "    run_initiate: false\n",
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
      }
    ]
  ];
]

(* Create Kira’s integral list file.
 *)
MkKiraIntegrals[dirname_, blist_] := Module[{bid, idlist},
  Do[
    idxlist = blist // CaseUnion[B[bid, idx__] :> {idx}];
    MkFile[dirname <> "/" <> KiraBasisName[bid] <> ".integrals",
      idxlist // Map[{KiraBasisName[bid], "[", Riffle[#, ","], "]\n"}&]
    ];
    ,
    {bid, blist // CaseUnion[B[bid_, ___] :> bid]}];
]

(* Create Kira’s configuration directory for reduction of
 * given integrals under given bases.
 *)
MkKiraConfig[dirname_, bases_List, blist_] :=
Module[{bid, bids, bid2topsector, idxlist},
  If[Not[FileExistsQ[dirname]], CreateDirectory[dirname]];
  If[Not[FileExistsQ[dirname <> "/config"]], CreateDirectory[dirname <> "/config"]];
  bids = blist // CaseUnion[B[bid_, ___] :> bid];
  bid2topsectors = Table[
    idxlist = blist // CaseUnion[B[bid, idx__] :> {idx}];
    bid -> (idxlist // TopSectors // Sort)
    ,
    {bid, bids}] // Association;
  MkKiraKinematicsYaml[dirname <> "/config/kinematics.yaml",
    bases[[1,"externalmom"]], bases[[1,"sprules"]]];
  MkKiraIntegralFamiliesYaml[dirname <> "/config/integralfamilies.yaml", bases // Select[MemberQ[bids, #["id"]]&]];
  MkKiraJobsYaml[dirname <> "/jobs.yaml", bids, bid2topsectors, "all"];
  MkKiraIntegrals[dirname, blist];
]

(* Read the IBP tables from a Kira directory, apply them to a
 * given expression.
 *)
KiraApplyResults[ex_, confdir_String, bases_List] :=
Module[{exx = ex, bids, bvarmap, bid, ibpmapfiles, bvar, table, bmap},
  bids = ex // CaseUnion[B[bid_, __] :> bid];
  bvarmap = Table[
    bvar = KiraBasisName[bid] // ToExpression;
    (bvar[idx__] :> B[$BID, idx]) /. $BID -> bid
    ,
    {bid, bases[[;;,"id"]]}
  ];
  Do[
    Print["* Loading IBP tables for basis ", bid];
    ibpmapfiles = MkString[confdir, "/results/", KiraBasisName[bid], "/kira_", KiraBasisName[bid], ".integrals.m"] // FileNames;
    FailUnless[Length[ibpmapfiles] === 1];
    table = ibpmapfiles // First // SafeGet;
    table = table /. bvarmap // TM;
    BMapLoad[bmap, table] // TM;
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

(* ## Export to TikZ *)
(*
Take a graph defined by edges (pairs of nodes) and produce
coordinates for all vertices by calling Graphviz (sfpd).

Example:

    In[]:= GraphLayoutVertexCoordinates[{{1,2},{1,3},{2,3}}]
    Out[]= {1 -> {272.32, 243.6},
            2 -> {27, 214.2},
            3 -> {175.3, 18}}
*)
GraphLayoutVertexCoordinates[edges:{{_,_} ...}] := Module[{tmp, out, result},
  tmp = MkTemp["graph", ".gv"];
  out = tmp <> ".json";
  MkFile[tmp,
    "graph {\n",
    edges // Map[Apply[List]] // MapReplace[
      {a_, b_} /; MemberQ[legs, a|b] :> {" ", a, " -- ", b, " [len=0.2];\n"},
      {a_, b_} :> {" ", a, " -- ", b, ";\n"}
    ],
    "}\n"
  ];
  Run["sfdp -Tjson -o", out, tmp];
  result = Import[out];
  DeleteFile[{tmp, out}];
  "objects" /. result // Map[("name" /. #) -> (MkString["{", "pos" /. #, "}"] // ToExpression) &]
]

MkTikZGraph[filename_String, dx_, dy_, vleft_, vright_, edges:{{_, _, _} ...}, labels:{{_, _, _, _}...}] :=
Module[{vertexcoords, outer, inner, sx, sy, x1, y1, x2, y2, x, y, bends, b, e, i},
  vertexcoords = edges[[;;,;;2]] // GraphLayoutVertexCoordinates//MapAt[ToExpression, #, {;;,1}]&;
  (* Shorten outer legs by a half. *)
  outer = edges[[;;,;;2]] // Flatten // Union // Select[Count[edges, {#, _, _}|{_, #, _}] === 1&];
  inner = outer // Map[Cases[edges, {#, v_, _}|{v_, #, _} :> v]& /* First];
  vertexcoords = Join[
    MapThread[#1->(#2+#3)/2&, {outer, outer // Map[Replace[vertexcoords]], inner // Map[Replace[vertexcoords]]}],
    vertexcoords // DeleteCases[Rule[Alternatives @@ outer, _]]
  ];
  (* Scale the diagram to desired size *)
  sx = vertexcoords // #[[;;,2,1]]& // dx/(1 + Max[#] - Min[#])&;
  sy = vertexcoords // #[[;;,2,2]]& // dy/(1 + Max[#] - Min[#])&;
  {x1, y1} = vleft // Replace[vertexcoords];
  {x2, y2} = vright // Replace[vertexcoords];
  If[x2 < x1, sx = -sx];
  If[y2 < y1, sy = -sy];
  vertexcoords = vertexcoords // MapReplace[Rule[v_, {x_, y_}] :> Rule[v, {(x - x1)*sx, (y - y1)*sy }]];
  bends = <|
    1 -> {""},
    2 -> {", bend right", ", bend left"},
    3 -> {", bend right=45", "", ", bend left=45"},
    4 -> {", bend right=75, looseness=1.25", ", bend right", ", bend left", ", bend left=75, looseness=1.25"},
    5 -> {", bend right=75, looseness=1.25", ", bend right=45", "", ", bend left=45", ", bend left=75, looseness=1.25"}
  |>;
  MkFile[filename,
    "\\begin{tikzpicture}\n",
    "\t\\begin{pgfonlayer}{nodelayer}\n",
    vertexcoords // MapReplace[(v_ -> {x_, y_}) :>
      {"\t\t\\node [style=", v /. { Alternatives @@ outer -> "none", _ -> "dot"}, "] (", v, ") at (",
        x // Round[#, 0.25]& // NumberForm[#, {Infinity,3}]&, ", ",
        y // Round[#, 0.25]& // NumberForm[#, {Infinity,3}]&, ") {};\n"}
    ],
    labels // MapReplace[{v1_, v2_, style_, text_} :> (
      {x, y} = ((v1 // Replace[vertexcoords]) + (v2 // Replace[vertexcoords]))/2;
      {"\t\t\\node [style=", style, "] (", v1, ":", v2, ") at (",
        x // Round[#, 0.125]& // NumberForm[#, {Infinity,3}]&, ", ",
        y // Round[#, 0.125]& // NumberForm[#, {Infinity,3}]&, ") {", text, "};\n"}
    )],
    "	\\end{pgfonlayer}\n",
    "	\\begin{pgfonlayer}{edgelayer}\n",
    edges //
      GroupBy[Sort[#[[;;2]]]&] //
      Values //
      Map[Function[{edgs},
        b = bends[Length[edgs]];
        Table[
          e = edgs[[i]];
          {"\t\t\\draw [style=", e[[3]], b[[If[Sort[e[[;;2]]]===e[[;;2]], i, Length[edgs]+1-i]]], If[e[[1]] === e[[2]], ", loop", ""], "] (", e[[1]], ") to (", e[[2]], ");\n"},
          {i, Length[edgs]}
        ]
      ]],
    "	\\end{pgfonlayer}\n",
    "\\end{tikzpicture}\n"
  ];
];

(* Create a TikZ file for an integral with given indices. The
 * indices should come in the same order as the propagators of
 * the diagram. Only non-negative indices work at the moment.
 *)
MkIntegralTikZ[filename_String, Diagram[_, _, ifld_List, ofld_List, props_List, verts_List], indices_List] :=
Module[{toremove, shrinkgr, vimap},
  FailUnless[Length[props] === Length[indices]];
  toremove = indices // PositionIndex // Lookup[#, 0, {}]&;
  vimap = Graph[verts[[;;,1]], props[[toremove, 4;;5]] // Map[Apply[UndirectedEdge]]] //
    ConnectedComponents //
    Select[Length[#] > 1&] //
    Map[Sort] //
    Map[Alternatives @@ # -> First[#]&];
  field2style = {"t" -> "top", _ -> "edge"};
  ifield2style = {"t"|"T" -> "incoming top", _ -> "incoming"};
  ofield2style = {"t"|"T" -> "outgoing top", "H" -> "outgoing higgs", _ -> "outgoing"};
  MkTikZGraph[filename, 3.0, 2.0, -1, -2, Join[
      ifld /. F[f_, fi_, vi_, mom_] :> {fi, vi /. vimap, f /. ifield2style},
      ofld /. F[f_, fi_, vi_, mom_] :> {vi /. vimap, fi, f /. ofield2style},
      Transpose[{props, indices}] // Map[Replace[{
        {_P, 0} :> Nothing,
        {P[f_, fi1_, fi2_, vi1_, vi2_, mom_], 1} :> {vi1 /. vimap, vi2 /. vimap, f /. field2style},
        {P[f_, fi1_, fi2_, vi1_, vi2_, mom_], idx_} :> {vi1 /. vimap, vi2 /. vimap, (f /. field2style) <> ",style=edge dot" <> ToString[idx-1]}
      }]]
    ],
    {}
  ];
]

(*
 * ## Export to FIRE & LiteRed
 *)

TrailingIrr[denominators_] := denominators /. {___, trail:Longest[den[_, _, irr] ...]} :> Length[{trail}]
LeadingIrr[denominators_] := denominators /. {lead:Longest[den[_, _, irr] ...], ___} :> Length[{lead}]

PrepareFireStart[basis_, confdir_String] := PrepareFireStart[basis, confdir, {}]
PrepareFireStart[basis_, confdir_String, invariantrules_] :=
Module[{tmpdir, props},
  Print["* Preparing FIRE basis ", basis["id"]];
  FIREPATH = Environment["FIREPATH"];
  FailUnless[FileExistsQ[FIREPATH <> "/FIRE6.m"]];
  props = basis["denominators"] //
    ReplaceAll[invariantrules] //
    ReplaceAll[{sp[p_] :> p^2, sp[p_,q_] :> p*q}] //
    MapReplace[den[p_] :> p^2, den[p_, m_, ___] :> p^2-m];
  tmpdir = MkTemp["fire", ""];
  EnsureCleanDirectory[tmpdir];
  Print["Directory: ", tmpdir];
  RunMathProgram[
    "Off[FrontEndObject::notavail];\n",
    "SetDirectory[\"", FIREPATH, "\"];\n",
    "Get[\"FIRE6.m\"];\n",
    "Internal = ", basis["loopmom"]//InputForm, ";\n",
    "External = ", basis["externalmom"]//InputForm, ";\n",
    "Propagators = ", props // InputForm, ";\n",
    "Replacements = ", basis["sprules"] /. sp[p_] :> p^2 /. sp[p_,q_] :> p*q /. invariantrules // InputForm, ";\n",
    "RESTRICTIONS = ",
      basis["denominators"] // MapIndexed[
        If[MatchQ[#1, den[_, _, cut]],
          Table[If[#2 === {idx}, -1, 0], {idx, Length[basis["denominators"]]}],
          Nothing
        ]&
      ]//InputForm,
    ";\n",
    "Print[\"Internal: \", Internal];\n",
    "Print[\"External: \", External];\n",
    "Print[\"Propagators: \", Propagators//InputForm];\n",
    "Print[\"Replacements: \", Replacements//InputForm];\n",
    "Print[\"RESTRICTIONS: \", RESTRICTIONS];\n",
    "Print[\"* PrepareIBP[]\"];\n",
    "PrepareIBP[];\n",
    "Print[\"* Prepare[]\"];\n",
    "Prepare[AutoDetectRestrictions->False];\n",
    "Print[\"* SaveStart[]\"];\n",
    "SaveStart[\"", tmpdir, "/start\"];\n",
    "Print[\"* Done with SaveStart[]\"];\n"
  ];
  RunMathProgram[
    "Off[FrontEndObject::notavail];\n",
    "Off[DiskSave::dir];\n",
    "Off[DiskSave::overwrite];\n",
    "SetDirectory[\"", FIREPATH, "/extra/LiteRed/Setup\"];\n",
    "Get[\"LiteRed.m\"];\n",
    "SetDirectory[\"", FIREPATH, "\"];\n",
    "Get[\"FIRE6.m\"];\n",
    "Internal = ", basis["loopmom"]//InputForm, ";\n",
    "External = ", basis["externalmom"]//InputForm, ";\n",
    "Propagators = ", props // InputForm, ";\n",
    "Replacements = ", basis["sprules"] /. sp[p_] :> p^2 /. sp[p_,q_] :> p*q /. invariantrules // InputForm, ";\n",
    "RESTRICTIONS = ",
      basis["denominators"] // MapIndexed[
        If[MatchQ[#1, den[_, _, cut]],
          Table[If[#2 === {idx}, -1, 0], {idx, Length[basis["denominators"]]}],
          Nothing
        ]&
      ]//InputForm,
    ";\n",
    "CreateNewBasis[basisx, Directory->(\"", tmpdir, "/litered\")];\n",
    "GenerateIBP[basisx];\n",
    "Print[\"* AnalyzeSectors[]\"];\n",
    "AnalyzeSectors[basisx,\n",
    "  ", basis["denominators"] // MapReplace[den[_, _, irr] -> 0, den[___] -> _] // InputForm, ",\n",
    "  CutDs -> (", basis["denominators"] // MapReplace[den[_, _, cut] -> 1, den[___] -> 0] // InputForm, ")];\n",
    "Print[\"* FindSymmetries[]\"];\n",
    "FindSymmetries[basisx];\n",
    "Print[\"* DiskSave[]\"];\n",
    "DiskSave[basisx];\n",
    "Print[\"* Done with DiskSave[]\"];\n"
  ];
  EnsureDirectory[confdir];
  RunMathProgram[
    "Off[FrontEndObject::notavail];\n",
    "SetDirectory[\"", FIREPATH, "\"];\n",
    "Get[\"FIRE6.m\"];\n",
    "LoadStart[\"", tmpdir, "/start\"];\n",
    "TransformRules[\"", tmpdir, "/litered\", \"", ExpandFileName[confdir], "/b", basis["id"], ".lbases\", ", basis["id"], "];\n",
    "SaveSBases[\"", ExpandFileName[confdir], "/b", basis["id"], "\"];\n",
    "Print[\"* Done with SaveSBases[]\"];\n"
  ];
  xEnsureNoDirectory[tmpdir];
  MkFile[confdir <> "/b" <> ToString[basis["id"]] <> ".pos",
    "|", LeadingIrr[basis["denominators"]] + 1, ",", Length[basis["denominators"]] - TrailingIrr[basis["denominators"]], "|"
  ];
  Print["* Done with everything"];
]

(*
 * ## Interface to pySecDec
 *)

(* Convert an integral name (`B` notation) into a filename.
 *)
SecDecIntegralName[integral_B] := integral //
  ToString //
  StringReplace[" " -> ""] //
  StringReplace["," -> "_"] //
  StringReplace["[" -> ""] //
  StringReplace["]" -> ""] //
  StringReplace["-" -> "m"]

(* Prepare `*generate.py` files in a given directory for the
 * given list of integrals.
 *)
SecDecPrepare[basedir_String, bases_List, integrals_List] :=
Module[{name, basisid, indices, basis, integral, p, m},
  Quiet[CreateDirectory[basedir], {CreateDirectory::filex}];
  Do[
    name = SecDecIntegralName[integral];
    Print["* Making ", basedir, "/", name, ".*"];
    basisid = integral[[1]];
    indices = integral[[2;;]] // Apply[List];
    basis = bases[[basisid]];
    MkFile[basedir <> "/" <> name <> ".generate.py",
      "#!/usr/bin/env python3\n",
      "import pySecDec as psd\n",
      "loopint = psd.loop_integral.LoopIntegralFromPropagators(\n",
      "  loop_momenta = ['", basis["loopmom"] // Riffle[#, "','"]&, "'],\n",
      "  external_momenta = ['", basis["externalmom"] // Riffle[#, "','"]&, "'],\n",
      "  regulator = 'eps',\n",
      "  propagators = [\n",
      basis["denominators"] /. {
        den[p_] :> {"    '(", p//CForm, ")^2'"},
        den[p_,m_,___] :> {"    '(", p//CForm, ")^2-", m//CForm, "'"}
      } // Riffle[#, ",\n"]&,
      "\n",
      "  ],\n",
      "  powerlist = [", indices // Riffle[#, ","]&, "],\n",
      "  replacement_rules = [\n  ",
      basis["sprules"] //
        ReplaceAll[sp -> (sp /* Sort)] //
        Union //
        MapReplace[
          (sp[p1_, p2_] -> v_) :> {"   ('", p1//InputForm, "*", p2//InputForm, "', '", v//InputForm, "')"}
        ] // Riffle[#, ",\n  "]&,
      "\n  ]\n",
      ")\n",
      "psd.loop_integral.loop_package(\n",
      "    name = '", name, "',\n",
      "    loop_integral = loopint,\n",
      "    real_parameters = [",
        Lookup[basis, "variables", basis["sprules"][[;;,2]] // CaseUnion[_Symbol]] //
          Map[{"'", #, "'"}&] //
          Riffle[#, ", "]&,
      "],\n",
      "    additional_prefactor = '(I*pi^(2-eps)/(2*pi)^(4-2*eps))^", Length[basis["loopmom"]], "',\n",
      "    requested_order = 2,\n",
      "    form_work_space = '50M',\n",
      "    contour_deformation = True\n",
      ")\n"
    ];
    MkFile[basedir <> "/" <> name <> ".integrate.py",
      "#!/usr/bin/env python3\n",
      "import sys\n",
      "import os.path\n",
      "import pySecDec as psd\n",
      "parameters = [float(parameter) for parameter in sys.argv[1:]]\n",
      "#sys.stderr.write(f'Parameters: {parameters}\\n')\n",
      "libfile = os.path.join(os.path.dirname(__file__), '", name, "/", name, "_pylink.so')\n",
      "lib = psd.integral_interface.IntegralLibrary(libfile)\n",
      "lib.use_Vegas(epsrel=1e-4, epsabs=1e-07, maxeval=1000000)\n",
      "int_wo_prefactor, prefactor, int_with_prefactor = lib(real_parameters=parameters)\n",
      "print('{%s,\\n%s}' % psd.integral_interface.series_to_mathematica(int_with_prefactor))\n"
    ];
    ,
    {integral, integrals}];
]

(* Compile the integration libraries for a list of integrals in
 * a directory previously prepared via [[SecDecPrepare]].
 *)
SecDecCompile[basedir_String, integrals_List] := Module[{name, integral},
  Do[
    name = SecDecIntegralName[integral];
    Run[MkString["rm -rf '", basedir, "/", name, "/'"]];
    Run[MkString["cd ", basedir, " && python3 ./", name, ".generate.py"]];
    Run[MkString["make -j4 -C ", basedir, "/", name, "/ pylink"]];
    ,
    {integral, integrals}];
]

(* Integrate a set of integrals at a given phase-space point
 * given by a list of variables. The variables come in the same
 * order as the "variables" key of the basis. The direcory
 * should have already been prepared with [[SecDecPrepare]] and
 * [[SecDecCompile]].
 *)
SecDecIntegrate[basedir_String, integrals_List, variables_List] := Module[{name, integral},
  integrals //
    Map[(
      Print["Integrating ", #, variables];
      name = SecDecIntegralName[#];
      integral -> RunThrough[
        MkString[
          "python3 ", basedir, "/", name, ".integrate.py ",
          variables // Map[InputForm] // Riffle[#, " "]&
        ],
        ""
      ]
    )&]
]
SecDecIntegrate[basedir_String, integral_, variables_List] :=
  SecDecIntegrate[basedir, {integral}, variables] // Only // #[[2]]&
