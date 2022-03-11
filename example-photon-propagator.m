(* # Photon propagator
 *
 * In this example we will calculate symbolically and evaluate
 * numerically a photon propagator. The physical model is QCD
 * with generic color group (symbolic `Nc`, `Ca`, `Cf`, etc),
 * `Nf` light quarks, and `Nft` heavy quarks of squared mass
 * `mt2`.
 *)

(* To start off, for interactive development it is convenient
 * to reduce the width of Mathematica formatted output to 65
 * characters, and force it to clear the history (the `Out[]`
 * variables) so that old expressions would not linger on in
 * the memory.
 *)

SetOptions[$Output, PageWidth -> 65];
$HistoryLength = 2;

(* Load *alibrary* and the [[QCD Feynman rules]] from `amodel-qcd.m`.
 *)

Get["alibrary.m"];
Get[$Apath <> "/amodel-qcd.m"];

(* We shall calculate the corrections of order $\alpha_s^\text{NLOOPS}$,
 * so define `NLOOPS`.
 *)

NLOOPS = 2;

(* ## Diagrams
 *
 * Generate the diagrams with one incoming photon and one outgoing
 * photon with QGraf by the way of [[mkdia.py]].
 *)

SafeRun[$Apath, "/mkdia.py dia-A-A-", NLOOPS, ".m"];
diagrams = SafeGet[MkString["dia-A-A-", NLOOPS, ".m"]];
Print["Loaded ", diagrams//Length, " diagrams"];

(* ## Tensor projectors
 *
 * Because the (amputated) photon propagator has open Lorentz
 * indices corresponding to the incoming and outgoing photons,
 * we need to project it to scalar values somehow.
 *
 * If $H^{\mu\nu}$ is the propagator, and $q$ is the momentum
 * flowing through it, there are two tensor structures that it
 * can be made of:
 *
 * $$ H^{\mu\nu} = A g^{\mu\nu} + B q^\mu q^\nu. $$
 *
 * Because of the Ward identities,
 *
 * $$ q_\mu H^{\mu\nu} = H^{\mu\nu} q_\nu = 0, $$
 *
 * this form is further constrained to just
 *
 * $$ H^{\mu\nu} = H ( g^{\mu\nu} - q^\mu q^\nu / q^2 ). $$
 *
 * Then to get the scalar $H$ from $H^{\mu\nu}$ we can construct
 * a projector as
 *
 * $$ H = H^{\mu\nu} g_{\mu\nu} / (d-1). $$
 *
 * Here it is in Mathematica notation.
 *)

projector = delta[lor[-1], lor[-2]] / (d-1);

(* Apply Feynman rules to get amplitudes out of diagrams.
 *)

amplitudes = diagrams // Map[Amplitude[#] * projector&];

(* ## Zero integral cleanup
 * 
 * Cleanup scaleless integrals. Some of these show up as propagators
 * with zero momentum, which means that a part of the graph is
 * disconnected from the rest, and thus scaleless. We can set
 * these to zero immediately.
 *)

amplitudes2 = amplitudes /. den[0] -> 0 /. momentum[0,_] -> 0;
Print["Non-zero amplitudes: ", amplitudes2//Count[Except[0]], " of ", amplitudes2//Length];

(* In this particular example there is a set of diagrams that are
 * zero by the color factors. For example, those with subdiagrams
 * where a photon turns into a single gluon. In principle we
 * could try to skip these during diagram generation, but we
 * don’t need to. Lets compute color factors instead, and see
 * what turns to zero.
 *)

amplitudes3 = amplitudes2 // RunThroughForm[{ "#call colorsum\n" }];
Print["Non-zero amplitudes: ", amplitudes3//Count[Except[0]], " of ", amplitudes3//Length];

(* ## Integral family construction
 *
 * Next we want to define the integral families onto which we
 * shall map the integrals, and which will be used in the IBP
 * reduction later.
 *
 * To this end, start with the set of denominators per diagram.
 *)

loopmomenta = diagrams // CaseUnion[l1|l2|l3|l4|l5];
externalmomenta = diagrams // CaseUnion[q|q1|q2|q3|q4|q5|p1|p2|p3|p4|p5];
Print["External momenta: ", externalmomenta];
Print["Loop momenta: ", loopmomenta];
FailUnless[Length[loopmomenta] === NLOOPS];

denominatorsets = amplitudes3 // NormalizeDens // Map[
  CaseUnion[_den] /* Select[NotFreeQ[Alternatives@@loopmomenta]]
];
Print["Unique denominator sets: ", denominatorsets // DeleteCases[{}] // Union // Length];

(* In principle we could define the integral families by the
 * denominator sets above, one family per denominator set. This
 * is not very efficient though, as there are symmetries between
 * those families. It’s best to first eliminate denominator sets
 * that are symmetric to others.
 *
 * The symmetries manifest most directly in the Feynman parameter
 * space, as permutations of the parameters. In the momenta space
 * this corresponds to loop momenta shifts, and we would like
 * to have a set of momenta shifts that would make symmetric
 * families explicitly identical, or identical to subsets of
 * bigger families, so we could test if a family is symmetric by
 * just asking if the set of denominators a subset of another
 * family.
 *
 * The tool to compute this momenta mapping is [Feynson], and
 * the interface to it is [[SymmetryMaps]].
 *
 * [feynson]: https://github.com/magv/feynson
 *)

$Feynson = "feynson";
momentamaps = SymmetryMaps[denominatorsets, loopmomenta];
Print["Found ", momentamaps // DeleteCases[{}] // Length, " momenta mappings"];

symmetrizeddenominatorsets =
  MapThread[ReplaceAll, {denominatorsets, momentamaps}] //
  NormalizeDens;

(* Then, the set of unique supersets of the denominator sets is
 * the set of integral families we need.
 *)

{denominatorsupersets, supersetindices} =
  UniqueSupertopologyMapping[symmetrizeddenominatorsets];
Print["Total integral families: ", denominatorsupersets//Length];

(* Let us then construct the IBP basis objects for each unique
 * denominator superset. These objects are just associations
 * storing denominators, and maps from scalar products into the
 * denominators.
 *
 * Also in the case when the denominator set is not sufficient
 * to form the full linear basis of scalar products, we want to
 * complete it; [[CompleteIBPBasis]] will do this for us.
 *)

bases = denominatorsupersets //
  MapIndexed[CompleteIBPBasis[
    First[#2], #1 // NormalizeDens // Sort, loopmomenta, externalmomenta, {sp[q,q]->sqrq}
  ]&];

(* ## Amplitude conversion
 * 
 * OK, now that we have the IBP bases, we can convert the
 * amplitudes to them.
 *
 * One practical thing to start with is to identify the set of
 * sectors (integral family subsets) that correspond to scaleless
 * integrals. This is also done with [Feynson].
 *)

zerosectors = ZeroSectors[bases];

(* Next, just call FORM to do all the tensor summation and
 * conversion to IBP families.
 *)

amplitudesB =
  MapThread[ReplaceAll, {amplitudes3, momentamaps}] //
  # * BID^supersetindices & //
  RunThroughForm[{
    "#call contractmomenta\n",
    "#call sort(after-contractmomenta)\n",
    "#call chaincolorT\n",
    "#call chaingammachain\n",
    "#call flavorsumwithcharge\n",
    "#call colorsum\n",
    "#call sort(after-colorsum)\n",
    "#call polarizationsum\n",
    "#call spinsum\n",
    "#call diractrace\n",
    "#call contractmomenta\n",
    FormCallToB[bases],
    "id mt1^2 = mt2;\n",
    FormCallZeroSectors[zerosectors]
  }] //
  MapWithProgress[FasterFactor];

FailUnless[FreeQ[amplitudesB, l1|l2|l3|l4]];

(* ## IBP reduction
 * 
 * Next, lets do the IBP reduction.
 *
 * We'll use [[KiraIBP]], a simple interface to IBP with [Kira].
 * It is probably too simplistic to work automatically for larger
 * examples, but for this problem it’s ideal.
 *
 * [kira]: https://kira.hepforge.org/
 *)

amplitudesBibp = amplitudesB // KiraIBP[bases];

(* Note that at the moment the mass reconstruction is disabled
 * because it’s overly slow in Kira; the first of the basis
 * invariants is set to 1 automatically during reduction. We’ll
 * take this into account and fix the rest of the amplitude this
 * way too.
 *
 * Hopefully this is temporary.
 *)

amplitudesBibp = amplitudesBibp // ReplaceAll[bases[[1,"invariants",1]]->1];

(* The full amplitude is just the sum of the diagram amplitudes.
 *)

fullamplitude = amplitudesBibp // Apply[Plus] // Bracket[#, _B, Factor]&;

(* A good correctness check is to see if there is any Xi dependence
 * left. None should remain.
 *)

FailUnless[FreeQ[fullamplitude, Xi]];

(* Now we have reduced the amplitude to master integrals.
 *
 * ## Numerical evaluation
 *
 * The final step is to insert the values of the masters. Of
 * course the masters here are known analytically, but as an
 * example let us evaluate them numerically with [pySecDec],
 * each up to $\varepsilon^2$.
 *
 * [pySecDec]: https://github.com/gudrunhe/secdec
 *)

masters = amplitudesBibp  // CaseUnion[_B];
Print["Master integrals: ", masters // Length];

(* The compilation here might take a while. Some of the stuff
 * being compiled will actually not be used by us, but we still
 * need to wait for it. The next release of pySecDec will allow
 * us to skip this.
 *
 * There’s also a way to run this step in parallel, across a
 * computing cluster even. See [[SecDecCompile]] for details.
 *)

SecDecCompile["secdectmpdir", bases, masters // Map[{#, 2}&]];

(* The integration can also be performed in parallel, on a
 * cluster. See [[SecDecIntegrate]] for details.
 *)

pspoint = { sqrq -> 120/100, mt2 -> 34/100 };

{mastervalues, mastererrors} =
  SecDecIntegrate["secdectmpdir", masters, pspoint] //
  Transpose;

(* Finally we have the value and the uncertainty of the full
 * amplitude.
 *)

value = fullamplitude  /.
  d -> 4 - 2*eps /.
  pspoint /.
  MapThread[Rule, {masters, mastervalues}];

error = fullamplitude /.
  d -> 4 - 2*eps /.
  pspoint /.
  MapThread[Rule, {masters, mastererrors}];

(* Because the full amplitude has symbolic contants in it, let
 * us pretty-print it by separating them.
 *)

Print["Value:"];
value //
  Normal //
  BracketAssociation[#, _Symbol | _flvsum | _flvsumt]& //
  Normal //
  Map[(
    Print["+", #[[1]], " *"];
    Print["  ", #[[2]]];
  )&];

Print["Absolute uncertainty:"];
error //
  Normal //
  BracketAssociation[#, _Symbol | _flvsum | _flvsumt]& //
  Normal //
  Map[(
    Print["+", #[[1]], " *"];
    Print["  ", #[[2]]];
  )&];
