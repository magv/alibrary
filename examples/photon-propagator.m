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
 * but in principle we can be completely general. Let us define
 * the number of loops as the `NLOOPS` variable, and use it
 * everywhere below.
 *)

NLOOPS = 2;

(* ## Creating the diagrams
 *
 * Generate the diagrams with one incoming photon and one outgoing
 * photon with QGraf.
 *)

diagrams = Diagrams[{"A"}, {"A"}, NLOOPS];
Print["Loaded ", diagrams//Length, " diagrams"];

(* One can now view the graphical representation of these diagrams
 * directly in Mathematica, or export them to LaTeX; please see
 * [[Displaying diagrams]] for an example.
 *)

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

loopMomenta = diagrams // CaseUnion[l1|l2|l3|l4|l5];
externalMomenta = diagrams // CaseUnion[q|q1|q2|q3|q4|q5|p1|p2|p3|p4|p5];
Print["External momenta: ", externalMomenta];
Print["Loop momenta: ", loopMomenta];
FailUnless[Length[loopMomenta] === NLOOPS];

denominatorSets = amplitudes3 // NormalizeDens // Map[
  CaseUnion[_den] /* Select[NotFreeQ[Alternatives@@loopMomenta]]
];
Print["Unique denominator sets: ", denominatorSets // DeleteCases[{}] // Union // Length];

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
momentaMaps = SymmetryMaps[denominatorSets, loopMomenta];
Print["Found ", momentaMaps // DeleteCases[{}] // Length, " momenta mappings"];

symmetrizedDenominatorSets =
  MapThread[ReplaceAll, {denominatorSets, momentaMaps}] //
  NormalizeDens;

(* Then, the set of unique supersets of the denominator sets is
 * the set of integral families we need.
 *)

{denominatorSupersets, supersetIndices} =
  UniqueSupertopologyMapping[symmetrizedDenominatorSets];
Print["Total integral families: ", denominatorSupersets//Length];

(* Let us then construct the IBP basis objects for each unique
 * denominator superset. These objects are just associations
 * storing denominators, and maps from scalar products into the
 * denominators.
 *
 * Also in the case when the denominator set is not sufficient
 * to form the full linear basis of scalar products, we want to
 * complete it; [[CompleteIBPBasis]] will do this for us.
 *)

bases = denominatorSupersets //
  MapIndexed[CompleteIBPBasis[
    First[#2], #1 // NormalizeDens // Sort, loopMomenta, externalMomenta, {sp[q,q]->sqrq}
  ]&];

(* ## Amplitude conversion
 * 
 * OK, now that we have the IBP bases, we can convert the
 * amplitudes to them.
 *
 * One practical thing to start with is to identify the set of
 * sectors (integral family subsets) that correspond to scaleless
 * integrals. This is also done with [Feynson].
 *
 * [feynson]: https://github.com/magv/feynson
 *)

zeroSectors = ZeroSectors[bases];

(* Next, just call FORM to do all the tensor summation and
 * conversion to IBP families.
 *)

amplitudesB =
  MapThread[ReplaceAll, {amplitudes3, momentaMaps}] //
  # * BID^supersetIndices & //
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
    FormCallZeroSectors[zeroSectors]
  }] //
  MapWithProgress[FasterFactor];

FailUnless[FreeQ[amplitudesB, l1|l2|l3|l4]];

(* ## IBP reduction
 * 
 * Next, lets do the IBP reduction.
 *
 * We’ll use [[KiraIBP]], a simple interface to IBP with [Kira].
 * It is probably too simplistic to work automatically for larger
 * examples, but for this problem it’s ideal.
 *
 * [kira]: https://kira.hepforge.org/
 *
 * Note that for best performance IBP reduction should be
 * performed with one of the mass scales set to `1`. This is
 * possible because we can always restore the overall power of
 * that mass scale by dimensional analysys at the end of the
 * calculation.
 *
 * For this reason, we will instruct Kira to set `mt2` to `1`,
 * and will do the same replacement in the rest of the amplitude
 * ourselves.
 *)

amplitudesBibp = amplitudesB //
  ReplaceAll[mt2 -> 1] //
  KiraIBP[bases, ReplaceByOne->mt2];

(* The full amplitude is just the sum of the diagram amplitudes.
 *)

fullAmplitude = amplitudesBibp // Apply[Plus] // Bracket[#, _B, Factor]&;

(* A good correctness check is to see if the result is
 * gauge-independent. Because our [[QCD Feynman rules]] use the
 * $R_\xi$ gauge and set `Xi`=$\xi-1$, we can check if there is
 * any `Xi` dependence left: none should remain.
 *)

FailUnless[FreeQ[fullAmplitude, Xi]];

(* We now have the amplitude reduced to master integrals.
 *)

Print["Master integral count: ", fullAmplitude // CaseUnion[_B] // Length];

(*
 * ## Numerical evaluation
 *
 * The final step is to insert the values of the masters. Of
 * course the masters here are known analytically, but as an
 * example let us evaluate them numerically with [pySecDec].
 *
 * [pySecDec]: https://github.com/gudrunhe/secdec
 *
 * Although pySecDec was originally designed to evaluate single
 * integrals, there are optimizations that can be performed when
 * evaluating whole amplitudes, and pySecDec knows how to apply
 * those. This is why our strategy will be to give pySecDec the
 * whole `fullAmplitude`, and ask to evaluate it, instead of
 * asking for each integral separately.
 *
 * To this end, let’s inspect the variables our amplitude depends
 * on:
 *)

Print["Variables in the amplitude: ", fullAmplitude // CaseUnion[_Symbol]];
Print["Functions in the amplitude: ", fullAmplitude // CaseUnion[(h_)[___]:>h]];

(* Variables like number of quark flavours and SU(N) constants
 * can be set to numbers to evaluate `fullAmplitude` as a single
 * numerical value, but because the master integrals themselves
 * do not depend on these variables, we can factor them out and
 * split `fullAmplitude` into a sum of prefactors (that we will
 * keep symbolically) multiplied by sums of integrals (that we
 * will evaluate numerically).
 *)

fullAmplitudeByPrefactor = fullAmplitude //
  ReplaceAll[Complex[re_, im_] :> re + im*ImagI] //
  BracketAssociation[Ca | Cf | Na | Tf | d33 | d44 | Nc | Nf | Nft | gH | gs | Xi | ImagI | _flvsum | _flvsumt];

Print["Symbolic prefactors:"];
fullAmplitudeByPrefactor // Keys // PrintIndexed;

(* Now we are ready to prepare the pySecDec integration library.
 * Let us ask pySecDec to expand the amplitudes to order
 * $\mathcal{O}(\varepsilon^2)$.
 *)

basesWithoutMt2 = bases // Map[
  (Append[#, <|"invariants" -> DeleteCases[#["invariants"], mt2]|>]&) /*
  Map[ReplaceAll[mt2 -> 1]]
];

SecDecPrepareSum[
  "secdectmpdir",
  basesWithoutMt2,
  fullAmplitudeByPrefactor // KeyMap[InputForm/*ToString] // Map[ReplaceAll[d->4-2*eps]],
  Order->2];

(* After the library is prepared, it needs to be compiled. This
 * is done by just running the `compile.py` script inside:
 *)

SafeRun["python3 secdectmpdir/compile.py"];

(* The compilation here automatically runs in parallel, but it
 * might still take a while. Once it is done, we are ready for
 * integration.
 *
 * Once we have selected some values for the parameters,
 * we can integrate via [[SecDecIntegrateSum]]:
 *)

pspoint = { sqrq -> 120/100 };

result = SecDecIntegrateSum[
  "secdectmpdir/disteval/sum.json",
  pspoint,
  EpsRel -> 10^-3, 
  EpsAbs -> 10^-8];

(* The integration will automatically run in parallel using all
 * locally available processors. It can also use the local GPUs,
 * although for this we would have needed to build the separate
 * GPU integration libraries during compilation (by setting
 * the `SECDEC_WITH_CUDA_FLAGS` environment variable to e.g.
 * `-gencode arch=compute_80,code=sm_80`, if GPUs with CUDA
 * “Compute Capability” 8.0 are used).
 *
 * Once the integration is over, all that is left for us is to
 * read out the results:
 *)

Print["Amplitude", pspoint // InputForm, "="]
symbolicPrefactors = result["sums"] // Keys;
Do[
  Print["  +", prefactor, " * ("];
  result["sums"][prefactor] // Map[Replace[
    {regulators_, value_, error_} :> (
      Print["    +", regulators // InputForm, "*("];
      Print["       (", value//InputForm, ") +/- "];
      Print["       (", error//InputForm, ")"]
      Print["     )"]
    )
  ]];
  Print["  )"];
  ,
  {prefactor, symbolicPrefactors}];
