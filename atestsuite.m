(* # Amplitude library test suite (`atestsuite.m`)
 *
 * This file contains a collection of tests for [[alibrary.m]]. It
 * can be executed as a script.
 *)

Get["alibrary.m"];

(*
 * ## Tests for [[utils.m]]
 *)

FailUnless[SeriesOrderCoefficient[1/(2+x)+O[x]^2, -1] === 0];
FailUnless[SeriesOrderCoefficient[1/(2+x)+O[x]^2, 0] === 1/2];
FailUnless[SeriesOrderCoefficient[1/(2+x)+O[x]^2, 1] === -1/4];
FailUnless[SeriesOrderCoefficient[1/(2+x)+O[x]^2, 2] === $Failed];
FailUnless[SeriesOrderCoefficient[1+2/x+O[x]^2, -1] === 2];
FailUnless[SeriesOrderCoefficient[1+2/x+O[x]^2, 0] === 1];
FailUnless[SeriesOrderCoefficient[1+2/x+O[x]^2, 1] === 0];
FailUnless[SeriesOrderCoefficient[1+2/x+O[x]^2, 2] === $Failed];

FailUnless[FormatScientific[1.2, 6] === "1.20e0"];
FailUnless[FormatScientific[-1.234, 6] === "-1.2e0"];
FailUnless[FormatScientific[1.2, 4] === "1.e0"];
FailUnless[FormatScientific[0.1234, 6] === "1.2e-1"];
FailUnless[FormatScientific[15.5, 4] === "2.e1"];
FailUnless[FormatScientific[123456, 0] === "1.e5"];
FailUnless[FormatScientific[0, 6] === "0.00e0"];

FailUnless[FormatFixed[1.23, 2] === "1.23"];
FailUnless[FormatFixed[123.456, 2] === "123.46"];
FailUnless[FormatFixed[0.0012, 3] === "0.001"];
FailUnless[FormatFixed[0.0012, 3] === "0.001"];
FailUnless[FormatFixed[0.0006, 3] === "0.001"];
FailUnless[FormatFixed[0.0006, 2] === "0.00"];
FailUnless[FormatFixed[0, 2] === "0.00"];
FailUnless[FormatFixed[65.43, 0] === "65"];
FailUnless[FormatFixed[10^99, 0] === "1" <> StringRepeat["0", 99]];
FailUnless[FormatFixed[10^99, 99] === "1" <> StringRepeat["0", 99] <> "." <> StringRepeat["0", 99]];

FailUnless[ProbablyZeroQ[0] === True];
FailUnless[ProbablyZeroQ[x] === False];
FailUnless[ProbablyZeroQ[(x+1)^2-x^2-2x-1] === True];

Module[{e = y + x y + 1/x/(x - a)/(x + 1) + Hlog[x, {1, y, 3}]/x},
  FailUnless[ FullSimplify[D[HlogInt[e, x], x] - e] === 0 ]]

Module[{e = 10 Hlog[x, {a, b, c}]},
  FailUnless[ FullSimplify[D[HlogInt[e, x], x] - e] === 0 ]]

Module[{e = y x^3/(x-b) + w x / (x-c) + z x^3/(x-d) Hlog[x, {e, f}]},
  FailUnless[ FullSimplify[D[HlogInt[e, x], x] - e] === 0 ]]

Module[{e = y x^3 Hlog[x, {a}]},
  FailUnless[ FullSimplify[D[HlogInt[e, x], x] - e] === 0 ]]

Module[{e = y x^2 Hlog[x, {a, b, c}]},
  FailUnless[ FullSimplify[D[HlogInt[e, x], x] - e] === 0 ]]

Module[{e = (x^3 + 1)/(x - 1) Hlog[x, {a, b}]},
  FailUnless[ FullSimplify[D[HlogInt[e, x], x] - e] === 0 ]]

FailUnless[
  Hpl[1, {0, 0, 0, 2, 0, 0, 0}] // Hpl1ExtractTrail0 // # === -35 Hpl[1, {8}]&,
  Hpl[1, {0, 0, 0, 2, 0, 0, 0}] // HplToMzv // # === -35 Mzv[8]&
];

FailUnless[
  MzvToHpl[Mzv[+3,+2,+2,+5]] === +Hpl[1, {+3,+2,+2,+5}],
  MzvToHpl[Mzv[+3,-2,+2,+5]] === -Hpl[1, {+3,-2,-2,-5}],
  MzvToHpl[Mzv[+3,+2,-2,+5]] === +Hpl[1, {+3,+2,-2,-5}],
  MzvToHpl[Mzv[+3,+2,+2,-5]] === -Hpl[1, {+3,+2,+2,-5}],
  MzvToHpl[Mzv[+3,-2,-2,+5]] === -Hpl[1, {+3,-2,+2,+5}],
  MzvToHpl[Mzv[+3,-2,+2,-5]] === +Hpl[1, {+3,-2,-2,+5}],
  MzvToHpl[Mzv[+3,+2,-2,-5]] === -Hpl[1, {+3,+2,-2,+5}],
  MzvToHpl[Mzv[+3,-2,-2,-5]] === +Hpl[1, {+3,-2,+2,-5}],
  MzvToHpl[Mzv[-3,+2,+2,+5]] === +Hpl[1, {-3,-2,-2,-5}]
];

FailUnless[HlogSeries[Hlog[x, {a,b,c}], 3] === (-1)^3/3! x^3/a/b/c + O[x]^4];

FailUnless[
  FeynmanParametrization[{l^2, (q-l)^2}, {l}, {q^2->q2}, {1, 2}] === {
    (I*Gamma[3 - d/2])/(2^d*E^((I/2)*d*Pi)*Pi^(d/2)),
    x1 + x2, 3 - d,
    q2*x1*x2, -3 + d/2,
    {x1, x2},
    x2
  }
]

FailUnless[
  FeynmanParametrization[{l^2, (p-l)^2, (q-l)^2}, {l}, {q^2->q2}, {1, 0, 2}] ===
  FeynmanParametrization[{l^2, (q-l)^2}, {l}, {q^2->q2}, {1, 2}]
]

(*
 * ## Tests for [[alibrary.m]]
 *)

FailUnless[
  ExpandScalarProducts[x|y|z|q][sp[2x+3y, z]] === 2 sp[x,z] + 3 sp[y,z],
  ExpandScalarProducts[x|y|z|q][sp[x+y, x-y]] === sp[x,x] - sp[y,y],
  ExpandScalarProducts[x|y|z|q][sp[a x + b y, c x]] === a c sp[x,x] + b c sp[x,y]
]

FailUnless[TopSectors[{{-1,-6,1}, {-3,1,1}}] === {
  <|"id" -> 4, "idx" -> {0, 0, 1}, "r" -> 1, "s" -> 7, "d" -> 0|>,
  <|"id" -> 6, "idx" -> {0, 1, 1}, "r" -> 2, "s" -> 3, "d" -> 0|>
}];

FailUnless[TopSectors[{{1,1,1}, {0,2,1}, {3,1,0}}] === {
  <|"id" -> 3, "idx" -> {1, 1, 0}, "r" -> 4, "s" -> 0, "d" -> 2|>,
  <|"id" -> 7, "idx" -> {1, 1, 1}, "r" -> 3, "s" -> 0, "d" -> 1|>
}];

Module[{basis, cbasis1, cbasis2},
  basis = CompleteIBPBasis[1,
    {den[l], den[l+k1], den[l+k2], den[l+k3]},
    {l},
    {k1, k2, k3},
    {sp[k1,k2]->sp12, sp[k1,k3]->sp13, sp[k2,k3]->sp23, sp[k1,k1]->0, sp[k2,k2]->0, sp[k3,k3]->0}
  ];
  FailUnless[(basis["denominators"] /. basis["denmap"]) === Table[DEN[i], {i, 4}]];
  FailUnless[ToB[sp[k1,k2], basis] === sp12 B[1,0,0,0,0]];
  FailUnless[ToB[sp[k1,k3], basis] === sp13 B[1,0,0,0,0]];
  cbasis1 = IBPBasisCross[1, basis, {k1->k2, k2->k1}];
  FailUnless[cbasis1["denominators"] === {den[l], den[l+k2], den[l+k1], den[l+k3]}];
  FailUnless[ToB[sp[k1,k2], cbasis1] === sp12 B[1,0,0,0,0]];
  FailUnless[ToB[sp[k1,k3], cbasis1] === sp13 B[1,0,0,0,0]];
  FailUnless[(cbasis1["denominators"] /. cbasis1["denmap"]) === Table[DEN[i], {i, 4}]];
  cbasis2 = IBPBasisCross[1, cbasis1, {k1->k2, k2->k1}];
  FailUnless[(cbasis2["denominators"] /. cbasis2["denmap"]) === Table[DEN[i], {i, 4}]];
  FailUnless[Not[IBPBasisSameQ[basis, cbasis1]]];
  FailUnless[IBPBasisSameQ[basis, cbasis2]];
]

(* ### Test `diractrace`.
 *)

DiracTrace = RunThroughForm[{ "#call chaingammachain\n", "#call diractrace\n" }]

FailUnless[DiracTrace[ gammatrace[gamma[lor[1]]] ] === 0];

FailUnless[DiracTrace[ gammatrace[gamma[lor[1]], gamma[lor[2]], gamma[lor3]] ] === 0];

FailUnless[
  DiracTrace[ gammatrace[gamma[lor[1]], gamma[lor[2]]] ] ===
  4 delta[lor[1], lor[2]]
];

FailUnless[
  DiracTrace[ gammatrace[gamma[lor[1]], gamma[lor[2]], gamma[lor[3]], gamma[lor[4]]] ] ===
  + 4 delta[lor[1],lor[2]] delta[lor[3],lor[4]]
  - 4 delta[lor[1],lor[3]] delta[lor[2],lor[4]]
  + 4 delta[lor[1],lor[4]] delta[lor[2],lor[3]]
];

FailUnless[
  DiracTrace[gammatrace[
      gamma[lor[1]], gamma[lor[2]], gamma[lor[3]],
      gamma[lor[4]], gamma[lor[5]], gamma[lor[6]]
    ]] ===
  + 4 delta[lor[1],lor[2]] delta[lor[3],lor[4]] delta[lor[5],lor[6]]
  - 4 delta[lor[1],lor[2]] delta[lor[3],lor[5]] delta[lor[4],lor[6]]
  + 4 delta[lor[1],lor[2]] delta[lor[3],lor[6]] delta[lor[4],lor[5]]
  - 4 delta[lor[1],lor[3]] delta[lor[2],lor[4]] delta[lor[5],lor[6]]
  + 4 delta[lor[1],lor[3]] delta[lor[2],lor[5]] delta[lor[4],lor[6]]
  - 4 delta[lor[1],lor[3]] delta[lor[2],lor[6]] delta[lor[4],lor[5]]
  + 4 delta[lor[1],lor[4]] delta[lor[2],lor[3]] delta[lor[5],lor[6]]
  - 4 delta[lor[1],lor[4]] delta[lor[2],lor[5]] delta[lor[3],lor[6]]
  + 4 delta[lor[1],lor[4]] delta[lor[2],lor[6]] delta[lor[3],lor[5]]
  - 4 delta[lor[1],lor[5]] delta[lor[2],lor[3]] delta[lor[4],lor[6]]
  + 4 delta[lor[1],lor[5]] delta[lor[2],lor[4]] delta[lor[3],lor[6]]
  - 4 delta[lor[1],lor[5]] delta[lor[2],lor[6]] delta[lor[3],lor[4]]
  + 4 delta[lor[1],lor[6]] delta[lor[2],lor[3]] delta[lor[4],lor[5]]
  - 4 delta[lor[1],lor[6]] delta[lor[2],lor[4]] delta[lor[3],lor[5]]
  + 4 delta[lor[1],lor[6]] delta[lor[2],lor[5]] delta[lor[3],lor[4]]
];

FailUnless[
  DiracTrace[ gammatrace[slash[p1], gamma[lor[1]], slash[p2], gamma[lor[1]]] ] ===
  + 8 dot[p1, p2] - 4 dot[p1, p2] d
];

(* ### Test flavorsum
 *)

FlavorSum = RunThroughForm["#call flavorsumwithcharge\n"];

FailUnless[
  FlavorSum[ deltaf[flv[1], flv[1]] ] ===
  flvsum[1]
];

FailUnless[
  FlavorSum[ deltaf[flv[1],flv[2]] deltaf[flv[2],flv[3]] deltaf[flv[3],flv[1]] ] ===
  flvsum[1]
];

FailUnless[
  FlavorSum[ deltaf[flv[1],flv[2]] deltaf[flv[2],flv[1]] chargeQ[flv[2]] ] ===
  flvsum[chargeQ]
];

FailUnless[
  FlavorSum[
    deltaf[flv[1],flv[2]] deltaf[flv[2],flv[1]] chargeQ[flv[1]] chargeQ[flv[2]]
  ] ===
  flvsum[chargeQ^2]
];

FailUnless[
  FlavorSum[ deltaf[flv[1],flv[1]] chargeQ[flv[1]]^4 ] ===
  flvsum[chargeQ^4]
];

(* ## Test amodel-qcd.m
 *)

Get[$Apath <> "/amodel-qcd.m"];

(* Tree-level A->Qq amplitude squared *)

diagrams = Diagrams[{"A"}, {"Q", "q"}, 0];
amplitude = diagrams // Map[Amplitude] // Apply[Plus];
amplitude2 = (
  amplitude
  AmpConjugate[amplitude]
  (-delta[lor[-1], lor[-1]//AmpConjugate])
  CutAmplitudeGlue[diagrams[[1]], diagrams[[1]]]
) //
  RunThroughForm[{
    "#call contractmomenta\n",
    "#call sort(after-contractmomenta)\n",
    "#call chaincolorT\n",
    "#call chaingammachain\n",
    "#call flavorsumwithcharge\n",
    "#call colorsum\n",
    "#call sort(after-colorsum)\n",
    "#call spinsum\n",
    "#call diractrace\n"
}] //
  Factor;
FailUnless[amplitude2 === 4 (d-2) Nc dot[p1, p2] flvsum[chargeQ^2]];

(* Tree-level A->Qqg amplitude squared *)

diagrams = Diagrams[{"A"}, {"Q", "q", "g"}, 0];
amplitude = diagrams // Map[Amplitude] // Apply[Plus];
amplitude2 = (
  amplitude
  AmpConjugate[amplitude]
  (-delta[lor[-1], lor[-1]//AmpConjugate])
  CutAmplitudeGlue[diagrams[[1]], diagrams[[1]]]
) //
  RunThroughForm[{
    "#call contractmomenta\n",
    "#call sort(after-contractmomenta)\n",
    "#call chaincolorT\n",
    "#call chaingammachain\n",
    "#call flavorsumwithcharge\n",
    "#call colorsum\n",
    "#call sort(after-colorsum)\n",
    "#call spinsum\n",
    "#call diractrace\n",
    "#procedure kinematics\n",
    "  id dot(p1?, p2?) = p1.p2;\n",
    "  id q = p1 + p2 + p3;\n",
    "  id p1.p1 = 0;\n",
    "  id p2.p2 = 0;\n",
    "  id p3.p3 = 0;\n",
    "  id p1.p2 = s12/2;\n",
    "  id p1.p3 = s13/2;\n",
    "  id p2.p3 = s23/2;\n",
    "#endprocedure\n",
    "id Na = Cf*Nc/Tf;\n",
    "id den(p1?) = 1/dot(p1, p1);\n",
    "denominators inv;\n",
    "argument inv;\n",
    "  #call kinematics\n",
    "endargument;\n",
    "#call kinematics\n"
}] // ReplaceAll[inv[x_] :> 1/x] // Factor;
FailUnless[amplitude2 ===
  gs^2 flvsum[chargeQ^2] 2 (d-2) Cf Nc/(s13 s23)*(
    + 4 s12^2
    + 4 s12 s13
    - 2 s13^2
    + d s13^2
    + 4 s12 s23
    - 8 s13 s23
    + 2 d s13 s23
    - 2 s23^2
    + d s23^2
)];
