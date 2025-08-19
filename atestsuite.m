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

counter = 0;
FailUnless[({1,2,3,1,3,4} // UniqueApply[Map[(counter += 1; #)&]]) === {1,2,3,1,3,4}];
FailUnless[counter === 4];

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

FailUnless[AmpConjugate[{flv[1], flv[-1]}] === {flv[5001], flv[-5001]}];
FailUnless[AmpConjugate[{lor[1], lor[-1]}] === {lor[5001], lor[-5001]}];
FailUnless[AmpConjugate[{fun[1], fun[-1]}] === {fun[5001], fun[-5001]}];
FailUnless[AmpConjugate[{adj[1], adj[-1]}] === {adj[5001], adj[-5001]}];

FailUnless[
  AmpConjugate[colorf[adj[1], adj[2], adj[3]]] ===
  colorf[adj[5001], adj[5002], adj[5003]]
];

FailUnless[
  AmpConjugate[colorT[adj[1], adj[2], adj[3]]] ===
  colorT[adj[5001], adj[5003], adj[5002]]
];

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

(* ### Tree-level A->Qq amplitude squared *)

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

(* ### Tree-level A->Qqg amplitude squared *)

diagrams = Diagrams[{"A"}, {"Q", "q", "g"}, 0];
amplitude = diagrams // Map[Amplitude] // Apply[Plus];
amplitude2 = (
  amplitude
  AmpConjugate[amplitude]
  (-delta[lor[-1], lor[-1]//AmpConjugate])
  CutAmplitudeGlue[diagrams[[1]], diagrams[[1]]]
) //
  (* ReplaceAll[den[p3+paxial[p3]] -> 0] // *)
  ReplaceAll[paxial[p3] -> pax3] //
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
    "#call contractmomenta\n",
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

(* ### Tree-level A->Qqgg amplitude squared *)

diagrams = Diagrams[{"A"}, {"Q", "q", "g", "g"}, 0][[;;]];
amplitudel = diagrams // Map[Amplitude] // Apply[Plus];
amplituder = diagrams // Map[Amplitude/*AmpConjugate] // Apply[Plus];
amplitude2 = (
  amplitudel amplituder
  (-delta[lor[-1], lor[-1]//AmpConjugate])
  CutAmplitudeGlue[diagrams[[1]], diagrams[[1]]]
) //
  ReplaceAll[den[p3+paxial[p3]] -> 0] //
  ReplaceAll[paxial[p4] -> pax4] //
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
    "#call contractmomenta\n",
    "#procedure kinematics\n",
    "  id dot(p1?, p2?) = p1.p2;\n",
    "  id q = p1 + p2 + p3 + p4;\n",
    "  id p1.p1 = 0;\n",
    "  id p2.p2 = 0;\n",
    "  id p3.p3 = 0;\n",
    "  id p4.p4 = 0;\n",
    "  id p1.p2 = s12/2;\n",
    "  id p1.p3 = s13/2;\n",
    "  id p1.p4 = s14/2;\n",
    "  id p2.p3 = s23/2;\n",
    "  id p2.p4 = s24/2;\n",
    "  id p3.p4 = s34/2;\n",
    "  id pax4.pax4 = 0;\n",
    "  id p1?.p2? = dot(p1, p2);\n",
    "#endprocedure\n",
    "id Na = Cf*Nc/Tf;\n",
    "id den(p1?) = 1/dot(p1, p1);\n",
    "denominators inv;\n",
    "argument inv;\n",
    "  #call kinematics\n",
    "endargument;\n",
    "#call kinematics\n"
}] //
  ReplaceAll[inv[x_] :> 1/x] //
  Bracket[#, _flvsum|Ca|Cf|gs|Nc|Tf, Factor]&;
FailUnless[FreeQ[amplitude2, Xi]];

correctamplitude2 = gs^4 flvsum[chargeQ^2] (
  +Cf^2*Nc (
    -8*den1*(-2+d)^2
    -4*den2*(-2+d)^2
    -8*den3*(-2+d)^2
    -4*den4*(-2+d)^2
    +16*den6*(30-15*d+2*d^2)
    +16*den7*(30-15*d+2*d^2)
    +16*den1*den3*(-4+d)*(-2+d)*s12
    +16*den2*den4*(-4+d)*(-2+d)*s12
    -2*den1*den2*(-8+d)*(-2+d)^2*s12
    -2*den3*den4*(-8+d)*(-2+d)^2*s12
    +8*den7^2*(-2+d)^2*(s12+s13+s14)
    +8*den6^2*(-2+d)^2*(s12+s23+s24)
    +16*den6*den7*(-2+d)*((-2+d)*s12-2*s34)
    +2*den3*den7^2*(-2+d)^3*(s12+s13+s14)*s34
    +2*den4*den7^2*(-2+d)^3*(s12+s13+s14)*s34
    +2*den1*den6^2*(-2+d)^3*(s12+s23+s24)*s34
    +2*den2*den6^2*(-2+d)^3*(s12+s23+s24)*s34
    +4*den1*den4*(12*(-2+d)*s12-2*(-10+d)*(-3+d)*s14-2*(-10+d)*(-3+d)*s23+(-7+d)*(-4+d)*(-2+d)*s34)
    +4*den2*den3*(12*(-2+d)*s12-2*(-10+d)*(-3+d)*s13-2*(-10+d)*(-3+d)*s24+(-7+d)*(-4+d)*(-2+d)*s34)
    +2*den2*den3*den4*s12*(8*(-2+d)*s12+2*(-2+d)^2*s13-(-4+d)*(32-14*d+d^2)*s34)
    +2*den1*den3*den4*s12*(8*(-2+d)*s12+2*(-2+d)^2*s14-(-4+d)*(32-14*d+d^2)*s34)
    +2*den1*den2*den4*s12*(8*(-2+d)*s12+2*(-2+d)^2*s23-(-4+d)*(32-14*d+d^2)*s34)
    +2*den1*den2*den3*s12*(8*(-2+d)*s12+2*(-2+d)^2*s24-(-4+d)*(32-14*d+d^2)*s34)
    +2*den2*den4*den7*s12*(8*(-2+d)*s12+2*(-2+d)^2*s13+(-6+d)*(20-10*d+d^2)*s34)
    +2*den1*den3*den7*s12*(8*(-2+d)*s12+2*(-2+d)^2*s14+(-6+d)*(20-10*d+d^2)*s34)
    +2*den2*den4*den6*s12*(8*(-2+d)*s12+2*(-2+d)^2*s23+(-6+d)*(20-10*d+d^2)*s34)
    +2*den1*den3*den6*s12*(8*(-2+d)*s12+2*(-2+d)^2*s24+(-6+d)*(20-10*d+d^2)*s34)
    +2*den3*den6*den7*s34*(2*(-2+d)*(2+d)*s12-2*(-2+d)^2*s14+(144-80*d+14*d^2-d^3)*s34)
    +2*den1*den6*den7*s34*(2*(-2+d)*(2+d)*s12-2*(-2+d)^2*s24+(144-80*d+14*d^2-d^3)*s34)
    -2*den1*den7*(8*(-2+d)*s12-2*(64-30*d+3*d^2)*s14-2*(-2+d)^2*s24+(144-80*d+14*d^2-d^3)*s34)
    -2*den3*den6*(8*(-2+d)*s12-2*(-2+d)^2*s14-2*(64-30*d+3*d^2)*s24+(144-80*d+14*d^2-d^3)*s34)
    +2*den4*den6*den7*s34*(2*(-2+d)*(2+d)*s12+2*(-2+d)^2*s14+(152-88*d+16*d^2-d^3)*s34)
    -2*den4*den6*(8*(-2+d)*s12+2*(-2+d)^2*s14-2*(64-30*d+3*d^2)*s23+(152-88*d+16*d^2-d^3)*s34)
    +2*den2*den6*den7*s34*(2*(-2+d)*(2+d)*s12+2*(-2+d)^2*s24+(152-88*d+16*d^2-d^3)*s34)
    -2*den2*den7*(8*(-2+d)*s12-2*(64-30*d+3*d^2)*s13+2*(-2+d)^2*s24+(152-88*d+16*d^2-d^3)*s34)
    +2*den4*den7*((-6+d)*(-4+d)*(-2+d)*s12-2*(-2+d)^2*s13-2*(-2+d)^2*s14+(-152+120*d-32*d^2+3*d^3)*s34)
    +2*den2*den6*((-6+d)*(-4+d)*(-2+d)*s12-2*(-2+d)^2*s23-2*(-2+d)^2*s24+(-152+120*d-32*d^2+3*d^3)*s34)
    +2*den3*den7*((-6+d)*(-4+d)*(-2+d)*s12-2*(-2+d)^2*s13-2*(-2+d)^2*s14+(-144+112*d-30*d^2+3*d^3)*s34)
    +2*den1*den6*((-6+d)*(-4+d)*(-2+d)*s12-2*(-2+d)^2*s23-2*(-2+d)^2*s24+(-144+112*d-30*d^2+3*d^3)*s34)
    +4*den2*den3*den6*den7*(s12-s34)*(4*(-2+d)*s12^2-8*(-3+d)*s12*s34+(-12+d^2)*s34^2)
    +4*den1*den4*den6*den7*(s12-s34)*(4*(-2+d)*s12^2-8*(-3+d)*s12*s34+(-12+d^2)*s34^2)
    +2*den2*den3*den7*(16*(-2+d)*s12^2+2*(-2+d)*(2+d)*s12*s13+2*(-2+d)^2*s13^2+(2+d)*(28-12*d+d^2)*s12*s34+(8+20*d-10*d^2+d^3)*s13*s34+2*(-12+d^2)*s34^2)
    +2*den1*den4*den7*(16*(-2+d)*s12^2+2*(-2+d)*(2+d)*s12*s14+2*(-2+d)^2*s14^2+(2+d)*(28-12*d+d^2)*s12*s34+(8+20*d-10*d^2+d^3)*s14*s34+2*(-12+d^2)*s34^2)
    +2*den1*den4*den6*(16*(-2+d)*s12^2+2*(-2+d)*(2+d)*s12*s23+2*(-2+d)^2*s23^2+(2+d)*(28-12*d+d^2)*s12*s34+(8+20*d-10*d^2+d^3)*s23*s34+2*(-12+d^2)*s34^2)
    +2*den2*den3*den6*(16*(-2+d)*s12^2+2*(-2+d)*(2+d)*s12*s24+2*(-2+d)^2*s24^2+(2+d)*(28-12*d+d^2)*s12*s34+(8+20*d-10*d^2+d^3)*s24*s34+2*(-12+d^2)*s34^2)
    +2*den1*den3*den6*den7*s12*(8*(-2+d)*s12^2-8*(-4+d)*s12*s34-(-4+d)*(32-14*d+d^2)*s34^2)
    +2*den2*den4*den6*den7*s12*(8*(-2+d)*s12^2-8*(-4+d)*s12*s34-(-4+d)*(32-14*d+d^2)*s34^2)
    +2*den1*den2*den3*den4*s12*(8*(-2+d)*s12^2+8*(-4+d)*s12*s34-(-4+d)*(32-14*d+d^2)*s34^2)
  )
  +Ca*Cf*Nc*(
    -den1*(-10+d)*(-4+d)^2
    -den3*(-10+d)*(-4+d)^2
    -16*den5*(-2+d)
    +2*den2*(-2+d)^2
    +2*den4*(-2+d)^2
    +4*den6*(-54+33*d-9*d^2+d^3)
    +4*den7*(-54+33*d-9*d^2+d^3)
    -8*den1*den3*(-4+d)*(-2+d)*s12
    -8*den2*den4*(-4+d)*(-2+d)*s12
    +den1*den2*(-8+d)*(-2+d)^2*s12
    +den3*den4*(-8+d)*(-2+d)^2*s12
    +2*den7^2*(-2+d)^3*(s12+s13+s14)
    +2*den4*den5*(-2+d)*(8*s12+2*(-2+d)*s13+(2+d)*s14+4*(-4+d)*s23)
    +2*den1*den4*den5*(4*(-2+d)*s12^2+4*(-2+d)*s12*s14+(-2+d)^2*s14^2+4*(-2+d)*s12*s23+(-4+d)*(-14+3*d)*s14*s23+(-2+d)^2*s23^2)
    +2*den4*den5*den6*(4*(-2+d)*s12^2+4*(-2+d)*s12*s14+(-2+d)^2*s14^2+4*(-2+d)*s12*s23+(-64+30*d-3*d^2)*s14*s23+(-2+d)^2*s23^2)
    +8*den5^2*den6*(-2+d)^2*s14*(s14-2*s24)
    -16*den5^2*den6*den7*(-2+d)^2*s12*s14*s24
    +8*den5*den7^2*(-2+d)^2*(s12+s13+s14)*s24
    -8*den5^2*den7*(-2+d)^2*(2*s14-s24)*s24
    +8*den5^2*den7^2*(-2+d)^2*(s12+s13+s14)*s24^2
    +2*den6^2*(-2+d)^3*(s12+s23+s24)
    +8*den5*den6^2*(-2+d)^2*s14*(s12+s23+s24)
    +8*den5^2*den6^2*(-2+d)^2*s14^2*(s12+s23+s24)
    -4*den5*den7*(-2+d)*(16*s12+(2+d)*s13+(2+d)*s14-2*(-4+d)*s24)
    +2*den1*den5*(12*(-2+d)*s12+(-32+6*d+d^2)*s14+2*(-2+d)*d*s23+(-2+d)^2*s24)
    +2*den2*den5*(-2+d)*(8*s12+4*(-4+d)*s13+2*(-2+d)*s23+(2+d)*s24)
    -4*den5*den6*(-2+d)*(16*s12-2*(-4+d)*s14+(2+d)*s23+(2+d)*s24)
    +2*den3*den5*(12*(-2+d)*s12+2*(-2+d)*d*s13+(-2+d)^2*s14+(-32+6*d+d^2)*s24)
    -8*den5*den6*den7*(-2+d)*(8*s12^2+(-2+d)*s12*s14+(-2+d)*s14^2+(-2+d)*s12*s24-4*s14*s24+(-2+d)*s24^2)
    +2*den2*den3*den5*(4*(-2+d)*s12^2+4*(-2+d)*s12*s13+(-2+d)^2*s13^2+4*(-2+d)*s12*s24+(-4+d)*(-14+3*d)*s13*s24+(-2+d)^2*s24^2)
    +2*den2*den5*den7*(4*(-2+d)*s12^2+4*(-2+d)*s12*s13+(-2+d)^2*s13^2+4*(-2+d)*s12*s24+(-64+30*d-3*d^2)*s13*s24+(-2+d)^2*s24^2)
    +2*den1*den5*den7*(4*(-2+d)*s12^2+4*(-2+d)*s12*s14+(-2+d)^2*s14^2-4*(-2+d)*s12*s24+(64-30*d+3*d^2)*s14*s24+(-2+d)^2*s24^2)
    +2*den3*den5*den6*(4*(-2+d)*s12^2-4*(-2+d)*s12*s14+(-2+d)^2*s14^2+4*(-2+d)*s12*s24+(64-30*d+3*d^2)*s14*s24+(-2+d)^2*s24^2)
    +den3*den7*(-((-2+d)*(32-6*d+d^2)*s12)-(-2+d)*(16-6*d+d^2)*s13+8*(-4+d)*s34)
    +den1*den6*(-((-2+d)*(32-6*d+d^2)*s12)-(-2+d)*(16-6*d+d^2)*s23+8*(-4+d)*s34)
    +4*den6*den7*((-208+112*d-18*d^2+d^3)*s12-2*(-4+d)*(-2+d)*s14-2*(-4+d)*(-2+d)*s24+(12+8*d-3*d^2)*s34)
    +den1*den7*(2*(88-38*d+3*d^2)*s12+(8+20*d-10*d^2+d^3)*s14+(-144+80*d-14*d^2+d^3)*s24+2*(-12+d^2)*s34)
    +den3*den6*(2*(88-38*d+3*d^2)*s12+(-144+80*d-14*d^2+d^3)*s14+(8+20*d-10*d^2+d^3)*s24+2*(-12+d^2)*s34)
    -den1*den4*(-4+d)*(6*(-6+d)*s12+(32-14*d+d^2)*s14+(32-14*d+d^2)*s23+(32-14*d+d^2)*s34)
    -den2*den3*(-4+d)*(6*(-6+d)*s12+(32-14*d+d^2)*s13+(32-14*d+d^2)*s24+(32-14*d+d^2)*s34)
    -den2*den3*den4*s12*(8*(-2+d)*s12+2*(-2+d)^2*s13-(-4+d)*(32-14*d+d^2)*s34)
    -den1*den3*den4*s12*(8*(-2+d)*s12+2*(-2+d)^2*s14-(-4+d)*(32-14*d+d^2)*s34)
    -den1*den2*den4*s12*(8*(-2+d)*s12+2*(-2+d)^2*s23-(-4+d)*(32-14*d+d^2)*s34)
    +den4*den6*(2*(-8+d)*(-10+3*d)*s12-(-10+d)*(-4+d)^2*s14+(-6+d)*(20-10*d+d^2)*s23-(-4+d)*(32-14*d+d^2)*s34)
    +den2*den7*(2*(-8+d)*(-10+3*d)*s12+(-6+d)*(20-10*d+d^2)*s13-(-10+d)*(-4+d)^2*s24-(-4+d)*(32-14*d+d^2)*s34)
    -den1*den2*den3*s12*(8*(-2+d)*s12+2*(-2+d)^2*s24-(-4+d)*(32-14*d+d^2)*s34)
    +den4*den7*(-((-2+d)*(28-8*d+d^2)*s12)-2*(-2+d)^2*s13-(-2+d)*(20-8*d+d^2)*s14-(-6+d)*(20-10*d+d^2)*s34)
    +den2*den6*(-((-2+d)*(28-8*d+d^2)*s12)-2*(-2+d)^2*s23-(-2+d)*(20-8*d+d^2)*s24-(-6+d)*(20-10*d+d^2)*s34)
    -den2*den4*den7*s12*(8*(-2+d)*s12+2*(-2+d)^2*s13+(-6+d)*(20-10*d+d^2)*s34)
    -den1*den3*den7*s12*(8*(-2+d)*s12+2*(-2+d)^2*s14+(-6+d)*(20-10*d+d^2)*s34)
    -den2*den4*den6*s12*(8*(-2+d)*s12+2*(-2+d)^2*s23+(-6+d)*(20-10*d+d^2)*s34)
    -den1*den3*den6*s12*(8*(-2+d)*s12+2*(-2+d)^2*s24+(-6+d)*(20-10*d+d^2)*s34)
    +den3*den6*den7*(-16*(-2+d)*s12^2+2*(-2+d)*(2+d)*s12*s14-2*(-2+d)^2*s14^2+(-200+104*d-16*d^2+d^3)*s12*s34+(144-80*d+14*d^2-d^3)*s14*s34-2*(-12+d^2)*s34^2)
    +den1*den6*den7*(-16*(-2+d)*s12^2+2*(-2+d)*(2+d)*s12*s24-2*(-2+d)^2*s24^2+(-200+104*d-16*d^2+d^3)*s12*s34+(144-80*d+14*d^2-d^3)*s24*s34-2*(-12+d^2)*s34^2)
    -den1*den3*den6*den7*s12*(8*(-2+d)*s12^2-8*(-4+d)*s12*s34-(-4+d)*(32-14*d+d^2)*s34^2)
    -den2*den4*den6*den7*s12*(8*(-2+d)*s12^2-8*(-4+d)*s12*s34-(-4+d)*(32-14*d+d^2)*s34^2)
    -den1*den2*den3*den4*s12*(8*(-2+d)*s12^2+8*(-4+d)*s12*s34-(-4+d)*(32-14*d+d^2)*s34^2)
    +den4*den6*den7*(-16*(-2+d)*s12^2-2*(-2+d)*(2+d)*s12*s14-2*(-2+d)^2*s14^2+(-8+d)*(-6+d)*(-4+d)*s12*s34+(-10+d)*(-4+d)^2*s14*s34+(-4+d)*(32-14*d+d^2)*s34^2)
    +den2*den6*den7*(-16*(-2+d)*s12^2-2*(-2+d)*(2+d)*s12*s24-2*(-2+d)^2*s24^2+(-8+d)*(-6+d)*(-4+d)*s12*s34+(-10+d)*(-4+d)^2*s24*s34+(-4+d)*(32-14*d+d^2)*s34^2)
  )
) /. {
  den1 -> 1/s13,
  den2 -> 1/s14,
  den3 -> 1/s23,
  den4 -> 1/s24,
  den5 -> 1/s34,
  den6 -> 1/(s13 + s14 + s34),
  den7 -> 1/(s23 + s24 + s34)
};

numpoint = {s12, s13, s14, s23, s24, s34} // Map[(# -> RandomInteger[{1, 99}])&];
FailUnless[Together[amplitude2 - correctamplitude2 /. numpoint] === 0];

(* ### Tree-level A->QqQq amplitude squared *)

diagrams = Diagrams[{"A"}, {"Q", "q", "Q", "q"}, 0];
amplitudel = diagrams[[;;]] // Map[Amplitude] // Apply[Plus];
amplituder = diagrams[[;;]] // Map[Amplitude/*AmpConjugate] // Apply[Plus];
amplitude2 = (
  amplitudel amplituder
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
    "#call contractmomenta\n",
    "#procedure kinematics\n",
    "  id dot(p1?, p2?) = p1.p2;\n",
    "  id q = p1 + p2 + p3 + p4;\n",
    "  id p1.p1 = 0;\n",
    "  id p2.p2 = 0;\n",
    "  id p3.p3 = 0;\n",
    "  id p4.p4 = 0;\n",
    "  id p1.p2 = s12/2;\n",
    "  id p1.p3 = s13/2;\n",
    "  id p1.p4 = s14/2;\n",
    "  id p2.p3 = s23/2;\n",
    "  id p2.p4 = s24/2;\n",
    "  id p3.p4 = s34/2;\n",
    "#endprocedure\n",
    "id Na = Cf*Nc/Tf;\n",
    "id den(p1?) = 1/dot(p1, p1);\n",
    "denominators inv;\n",
    "argument inv;\n",
    "  #call kinematics\n",
    "endargument;\n",
    "#call kinematics\n"
}] //
  ReplaceAll[inv[x_] :> 1/x] //
  Bracket[#, _flvsum|Ca|Cf|gs|Nc|Tf, Factor]&;

correctamplitude2 = gs^4 (
 +Cf*Nc*Tf*flvsum[chargeQ]^2*(
  -4*den7*(-8+3*d)
  -4*den8*(-8+3*d)
  +4*den1*d
  -4*den3*d
  +8*den6*(-8+3*d)
  -16*den1*den7*(-2+d)*s24
  -4*den5*den4*(-8*s14+d*(3*s14+s24))
  -4*den6*den4*(-8*(s13+s23)+d*(3*s13-s14+2*s23+s24))
  +4*den1*den4*(-8*(s13+s14+s23)+d*(3*s13+2*s14+2*s23+2*s24))
  +4*den2*den6*(-8+3*d)*s24
  +8*den1*den6*(-8+3*d)*s24
  -4*den1*den4*den8*(-16+5*d)*(s13+s14)*s24
  -4*den1*den6*den4*(-16+5*d)*(s13+s23)*s24
  -16*den1*den4*den7*(s23+s24)*(d*s14-2*(s14-s23+s24))
  -4*den6*den4*den7*(-8*(s14^2-2*s14*s23+s23^2+s14*s24+3*s23*s24)+d*(4*s14^2-4*s14*s23+3*s23^2+6*s23*s24+3*s24^2))
  +4*den5*den4*den8*(-8*(s13+s14-s24)+d*(3*s13+2*s14-s24))*s24
  +4*den6*den4*den8*s24*(-8*(s13+s14+s24)+d*(2*s13+3*s14+s24))
  +4*den1*den5*den4*(-8*(s14^2-s14*s23+3*s14*s24+2*s23*s24)+d*(3*s14^2-s14*s23+3*s23^2+6*s14*s24+2*s23*s24+3*s24^2))
  +4*den5*den4*den7*(-8*(s14^2-s14*s23+2*s14*s24+3*s23*s24+s24^2)+d*(4*s14^2+3*s23^2+4*s14*s24+6*s23*s24+3*s24^2))
  +4*den1*den5*den4*den7*s23*(3*d*(s23+s24)^2-8*s24*(3*s23+s24))
  -4*den1*den6*den4*den8*(-8*s13*(s13-3*s24)+3*d*(s13-s24)^2)*s24
  -4*den1*den5*den4*den8*s24*(3*d*(s14+s24)^2-8*s14*(s14+3*s24))
  -4*den1*den6*den4*den7*s24*(3*d*(s23+s24)^2-8*s23*(s23+3*s24))
  +32*den3*den5*(d*s13-2*(s13-s24+s34))
  -4*den3*den6*(d*(s13+4*s14+4*s24)-8*(s14+s34))
  -8*den3*den8*(-8+3*d)*s34
  +4*den5*den8*(d*(3*s13-4*s14-6*s24-5*s34)-8*(s13-s14-3*s24-2*s34))
  -4*den5*den6*(-8*(s14+s23+s24+s34)+d*(2*s14+3*s23+4*s34))
  -4*den2*den7*(-8+3*d)*s34
  +4*den6*den7*(d*(2*s14-4*s23+2*s24-3*s34)-8*(s14-s23-s34))
  +4*den6*den8*(-8*(3*s13+s14-2*s24)+d*(10*s13+3*s14-6*s24-2*s34))
  -4*den2*den3*(-8*(s12+s13+s24)+d*(3*s12+2*s13+2*s24+2*s34))
  -4*den1*den8*(-8*(s13+s14)+d*(3*s13+2*s14+2*s24+s34))
  +4*den2*den8*(-8*(s12+s13)+d*(3*s12+2*s13-s24+s34))
  +4*den3*den7*(-8*(s12+s24)+d*(3*s12+s14+2*s24+2*s34))
  +16*den2*den5*(-2+d)*s34
  -8*den7*den8*(d*(3*s12-2*s34)-4*(2*s12-s34))
  +4*den1*den5*(-8*(2*s14-2*s23+s34)+d*(4*s14-7*s23+4*s24+4*s34))
  +4*den5*den7*(-32*(s14+s24+s34)+d*(12*s14+s23+12*s24+9*s34))
  +4*den2*den3*den8*(-16+5*d)*(s12+s13)*s34
  -4*den1*den5*den7*s23*(d*(2*s23+2*s24-3*s34)-8*(s24-s34))
  +4*den1*den6*den8*s24*(d*(2*s13-2*s24-3*s34)-8*(s13-s34))
  +4*den1*den6*den7*s24*(d*(2*s23+2*s24-3*s34)-8*(s23-s34))
  -4*den2*den5*den6*s24*(d*(3*s23-2*s24-2*s34)-8*(s23-s34))
  +4*den2*den3*den7*(-16+5*d)*(s12+s24)*s34
  +16*den2*den3*den5*(s24+s34)*(d*s13-2*(s13-s24+s34))
  -4*den1*den5*den8*(-8*(s14^2+5*s14*s24+2*s14*s34+s24*s34+s34^2)+d*(3*s14^2+10*s14*s24+7*s24^2+4*s14*s34+8*s24*s34+4*s34^2))
  -4*den2*den3*den6*(-8*(s13^2-s13*s24+3*s13*s34+2*s24*s34)+d*(3*s13^2-s13*s24+3*s24^2+6*s13*s34+2*s24*s34+3*s34^2))
  -4*den3*den5*den6*(-8*(s14^2+s14*s24+2*s14*s34+5*s24*s34+s34^2)+d*(4*s14^2+8*s14*s24+7*s24^2+4*s14*s34+10*s24*s34+3*s34^2))
  -4*den3*den6*den8*s34*(-8*(s13-s14)+d*(2*s13-3*s14+2*s34))
  +4*den2*den5*den7*(-8*(s23-s24)+d*(3*s23-2*s24-2*s34))*s34
  -4*den2*den7*den8*s34*(-8*(s12+s24+s34)+d*(2*s12+3*s24+s34))
  -4*den3*den7*den8*(-8*(s12-s14)+d*(2*s12-3*s14-2*s34))*s34
  +4*den2*den6*den8*(-8*(s13^2-2*s13*s24+s24^2+3*s13*s34+s24*s34)+d*(3*s13^2-4*s13*s24+4*s24^2+6*s13*s34+3*s34^2))
  +4*den3*den5*den7*(-8*(s14^2+2*s14*s24+s24^2+s14*s34+5*s24*s34)+d*(4*s14^2+4*s14*s24+3*s24^2+8*s14*s34+10*s24*s34+7*s34^2))
  -4*den2*den3*den5*den6*s24*(3*d*(s24+s34)^2-8*s34*(3*s24+s34))
  +4*den2*den3*den7*den8*(-8*s12*(s12-3*s34)+3*d*(s12-s34)^2)*s34
  +4*den2*den3*den6*den8*s34*(3*d*(s13+s34)^2-8*s13*(s13+3*s34))
  +4*den2*den3*den5*den7*s34*(3*d*(s24+s34)^2-8*s24*(s24+3*s34))
 )
 +Ca*Cf*Nc*flvsum[chargeQ^2]*(
  +4*den5*(4-4*d+d^2)
  -8*den6*(-6+d)*(-4+d)*(-3+d)
  -2*den8*(-244+176*d-41*d^2+3*d^3)
  -den7*(-8+d)*(-4+d)*(-14+5*d)
  +den3*(-248+144*d-24*d^2+d^3)
  +den2*(-4+d)*(32-14*d+d^2)
  +den4*(-4+d)*(32-14*d+d^2)
  +2*den1*(-4+d)*(28-12*d+d^2)
  -2*den8^2*(-6+d)*(4-4*d+d^2)*(s12+s13+s14)
  +den6*den4*(-4+d)*(d^2*(s13+s14+s23)-2*d*(7*s13+5*s14+7*s23+2*s24)+8*(4*s13+3*s14+4*s23+3*s24))
  +2*den1*den7*(4-4*d+d^2)*(s23+s24)
  +den1*den6*(4*d*(15*s13+11*s23-24*s24)-8*(8*s13+6*s23-17*s24)-2*d^2*(8*s13+6*s23-11*s24)+d^3*(s13+s23-2*s24))
  -2*den7^2*(-8+d)*(4-4*d+d^2)*(s12+s23+s24)
  -den1*den2*(d^3*(s13+2*s24)-32*(2*s13+9*s24)-4*d^2*(4*s13+9*s24)+4*d*(15*s13+46*s24))
  -den4*den8*(8*(s12+7*s13+2*s14-6*s24)-4*d*(2*s12+13*s13+5*s14-6*s24)+2*d^2*(s12+7*s13+4*s14-3*s24)+d^3*(-s13-s14+s24))
  -den2*den4*(-8+d)*(4-4*d+d^2)*s24
  +den3*den4*(d^3*(-s13-s24)+2*d^2*(s12+7*s13-s14+9*s24)+8*(s12+7*s13-3*s14+16*s24)-4*d*(2*s12+13*s13-4*s14+22*s24))
  +den5*den4*(-4+d)*(32-14*d+d^2)*(s14-s24)
  -den4*den7*(d^3*(-s14-s23-s24)+8*(s12+6*s14+4*s23+9*s24)+2*d^2*(s12+3*s14+6*s23+9*s24)-4*d*(2*s12+6*s14+9*s23+17*s24))
  +4*den4*den7*den8*(4-4*d+d^2)*s12*(s14-s24)
  -4*den4*den8^2*(4-4*d+d^2)*(s12+s13+s14)*s24
  +4*den4*den7^2*(4-4*d+d^2)*s14*(s12+s23+s24)
  -den3*den4*den7*s14*(-d^3*s24+2*d^2*(s12-s14+9*s24)-8*d*(s12-2*s14+11*s24)+8*(s12-3*s14+16*s24))
  +den5*den4*den7*(-4+d)*s14*(2*d*(2*s14-7*s23-12*s24)-8*(2*s14-4*s23-7*s24)+d^2*(s23+2*s24))
  -2*den5*den4*den8*(d^2*(s13^2+2*s13*s14+s14^2+2*s14*s24+2*s24^2)-4*d*(s13^2+3*s13*s14+s14^2+s13*s24+4*s14*s24+3*s24^2)+4*(s13^2+4*s13*s14+s14^2+4*s13*s24+8*s14*s24+4*s24^2))
  -2*den6*den4*den7*(d^2*(2*s14^2-2*s14*s23+s23^2+2*s23*s24+s24^2)-4*d*(3*s14^2-4*s14*s23+s23^2-s14*s24+3*s23*s24+s24^2)+4*(4*s14^2-8*s14*s23+s23^2-4*s14*s24+4*s23*s24+s24^2))
  +den2*den4*den8*s24*(-d^3*s13+2*d^2*(s12+9*s13+s24)-8*d*(s12+11*s13+2*s24)+8*(s12+16*s13+3*s24))
  -den2*den6*den4*s24*(d^3*s13-2*d^2*(9*s13+s23+s24)+8*d*(11*s13+s23+2*s24)-8*(16*s13+s23+3*s24))
  -den6*den4*den8*(-4+d)*s24*(d^2*(2*s13+s14)+8*(7*s13+4*s14+2*s24)-2*d*(12*s13+7*s14+2*s24))
  +den3*den5*den4*(-d^3*s24*(s13+s24)+2*d^2*(s13^2+s13*s14+9*s13*s24+9*s24^2)-8*d*(s13^2+2*s13*s14-s14^2+12*s13*s24-s14*s24+11*s24^2)+8*(s13^2+3*s13*s14-2*s14^2+20*s13*s24-4*s14*s24+16*s24^2))
  -den3*den5*den4*den7*s14*(18*d^2*s24^2-d^3*s24^2-16*(s14-2*s24)*(s14+4*s24)+8*d*(s14^2+s14*s24-11*s24^2))
  -den2*den6*den4*den8*s24*(-18*d^2*s13^2+d^3*s13^2-16*(4*s13-s24)*(2*s13+s24)+8*d*(11*s13^2+s13*s24-s24^2))
  -den3*den6*(-4+d)*(d^2*s13-2*d*(5*s13-2*s14-2*s24-2*s34)+8*(3*s13-s14-2*s24-2*s34))
  -den1*den3*(d^3*(s13+s24)+4*d*(22*s13+2*s14+13*s24-4*s34)-8*(16*s13+s14+7*s24-3*s34)-2*d^2*(9*s13+s14+7*s24-s34))
  +2*den6*den7*(2*d^3*(s14-s23)-4*(28*s14-42*s23+10*s24-7*s34)+4*d*(23*s14-29*s23+5*s24-3*s34)+d^2*(-24*s14+26*s23-2*s24+s34))
  +den1*den5*(d^3*(s14+2*s23+s24)+4*d*(s14+34*s23+9*s24-4*s34)-2*d^2*(4*s14+14*s23+7*s24-s34)+8*(4*s14-26*s23-s24+3*s34))
  +2*den2*den5*(4-4*d+d^2)*(s24+s34)
  -den3*den5*(d^3*(s13+s24)-2*d^2*(7*s13-s14+11*s24+5*s34)-8*(20*s13-9*s14+25*s24+12*s34)+4*d*(22*s13-8*s14+31*s24+15*s34))
  +2*den5*den8*(-4*(5*s13-46*s14-29*s24-34*s34)+4*d*(s13-32*s14-22*s24-25*s34)-2*d^3*(s14+s24+s34)+d^2*(s13+28*s14+23*s24+24*s34))
  -2*den5*den6*(d^2*(-24*s14-s23-22*s24-26*s34)+2*d^3*(s14+s24+s34)+4*d*(23*s14+3*s23+18*s24+29*s34)-4*(28*s14+7*s23+18*s24+42*s34))
  -2*den6^2*(-4+d)*(4-4*d+d^2)*(s13+s23+s34)
  -2*den5^2*(-6+d)*(4-4*d+d^2)*(s14+s24+s34)
  +den1*den8*(-8*(32*s13+17*s14+3*s24-15*s34)+8*d*(22*s13+12*s14-s24-10*s34)-4*d^2*(9*s13+5*s14-2*s24-4*s34)+d^3*(2*s13+s14-s24-s34))
  -den6*den8*(-4+d)*(-2*d*(16*s13+7*s14+s24-5*s34)+8*(9*s13+4*s14-2*s34)+d^2*(2*s13+s14+s24-s34))
  +den2*den8*(-4+d)*(d^2*(s12+s13+s34)+8*(4*s12+4*s13+3*s24+3*s34)-2*d*(7*s12+7*s13+2*s24+5*s34))
  +den2*den6*(-2+d)*(d^2*(s13-s24+s34)-2*d*(7*s13-s24+5*s34)+4*(8*s13-s24+6*s34))
  +den3*den7*(-8*(17*s12-18*s14+32*s24-3*s34)+d^3*(s12+2*s24+s34)+8*d*(12*s12-9*s14+22*s24+s34)-4*d^2*(5*s12-2*s14+9*s24+2*s34))
  +den2*den7*(d^3*(s12+s24+2*s34)-2*d^2*(6*s12+s23+9*s24+12*s34)-8*(4*s12+s23+9*s24+22*s34)+4*d*(9*s12+2*s23+17*s24+28*s34))
  +den3*den8*(d^3*(s12+s13+2*s34)-2*d^2*(8*s12+10*s13+11*s34)-8*(8*s12+10*s13+17*s34)+4*d*(15*s12+19*s13+24*s34))
  +den5*den7*(-8*d*(s14-6*s23-12*s24+2*s34)+d^3*(2*s14+s23+2*s24+2*s34)+16*(5*s14-3*s23-8*s24+6*s34)-2*d^2*(6*s14+7*s23+12*s24+6*s34))
  -2*den7*den8*(-4*(54*s12+s14-s24-37*s34)+16*d*(9*s12-7*s34)+2*d^3*(s12-s34)+d^2*(-30*s12+s14-s24+27*s34))
  -4*den3*den5^2*(4-4*d+d^2)*s13*(s14+s24+s34)
  -4*den3*den5*den8*(4-4*d+d^2)*s14*(s13-s34)
  +4*den3*den8^2*(4-4*d+d^2)*(s12+s13+s14)*s34
  -4*den2*den6*den7*(4-4*d+d^2)*s23*(s24-s34)
  +4*den1*den5^2*(4-4*d+d^2)*s23*(s14+s24+s34)
  -4*den1*den6^2*(4-4*d+d^2)*s24*(s13+s23+s34)
  -4*den2*den6^2*(4-4*d+d^2)*s24*(s13+s23+s34)
  +4*den1*den5*den6*(4-4*d+d^2)*(s23-s24)*s34
  +4*den2*den7^2*(4-4*d+d^2)*(s12+s23+s24)*s34
  -den2*den6*den8*(-4+d)*s24*(d^2*(2*s13+s34)+8*(7*s13+2*s24+4*s34)-2*d*(12*s13+2*s24+7*s34))
  +den1*den5*den7*(-2*d^2*(7*s23^2+16*s23*s24+9*s24^2-9*s23*s34)+d^3*(s23^2+2*s23*s24+s24^2-s23*s34)-16*(3*s23^2+12*s23*s24+8*s24^2-9*s23*s34-2*s24*s34-s34^2)+8*d*(6*s23^2+18*s23*s24+11*s24^2-12*s23*s34-s24*s34-s34^2))
  +den1*den6*den8*(d^3*(s13^2-2*s13*s24+s24^2+s24*s34)-2*d^2*(9*s13^2-16*s13*s24+7*s24^2+9*s24*s34)-16*(8*s13^2-12*s13*s24+3*s24^2-2*s13*s34+9*s24*s34-s34^2)+8*d*(11*s13^2-18*s13*s24+6*s24^2-s13*s34+12*s24*s34-s34^2))
  +den1*den2*den5*(d^3*(s23-s24)*s24+2*d^2*(s23^2-9*s23*s24+9*s24^2-s23*s34)+8*(s23^2-20*s23*s24+16*s24^2-3*s23*s34-4*s24*s34-2*s34^2)-8*d*(s23^2-12*s23*s24+11*s24^2-2*s23*s34-s24*s34-s34^2))
  -den1*den3*den6*(d^3*s13*(s13+s24)-2*d^2*(9*s13^2+9*s13*s24+s24^2+s24*s34)-8*(16*s13^2+20*s13*s24+s24^2-4*s13*s34+3*s24*s34-2*s34^2)+8*d*(11*s13^2+12*s13*s24+s24^2-s13*s34+2*s24*s34-s34^2))
  -2*den3*den7*den8*(d^2*(s12^2-2*s12*s14+s14^2-4*s12*s34+2*s14*s34+3*s34^2)-4*d*(s12^2-3*s12*s14+s14^2-7*s12*s34+3*s14*s34+5*s34^2)+4*(s12^2-4*s12*s14+s14^2-12*s12*s34+6*s14*s34+9*s34^2))
  -2*den2*den7*den8*(d^2*(s12^2+2*s12*s24+s24^2-2*s12*s34+2*s34^2)-4*d*(s12^2+3*s12*s24+s24^2-4*s12*s34-s24*s34+3*s34^2)+4*(s12^2+4*s12*s24+s24^2-8*s12*s34-4*s24*s34+4*s34^2))
  -2*den1*den5*den8*(d^2*(s14^2+4*s14*s24+3*s24^2+2*s14*s34+4*s24*s34+2*s34^2)-4*d*(s14^2+7*s14*s24+5*s24^2+4*s14*s34+7*s24*s34+3*s34^2)+4*(s14^2+12*s14*s24+9*s24^2+8*s14*s34+12*s24*s34+4*s34^2))
  -2*den3*den5*den6*(d^2*(2*s14^2+4*s14*s24+3*s24^2+2*s14*s34+4*s24*s34+s34^2)-4*d*(3*s14^2+7*s14*s24+5*s24^2+4*s14*s34+7*s24*s34+s34^2)+4*(4*s14^2+12*s14*s24+9*s24^2+8*s14*s34+12*s24*s34+s34^2))
  -2*den2*den5*den6*(d^2*(s23^2-2*s23*s24+3*s24^2-2*s23*s34+4*s24*s34+s34^2)-4*d*(s23^2-3*s23*s24+5*s24^2-3*s23*s34+7*s24*s34+s34^2)+4*(s23^2-6*s23*s24+9*s24^2-4*s23*s34+12*s24*s34+s34^2))
  -2*den1*den6*den7*(4*(s23^2+12*s23*s24+9*s24^2-4*s23*s34-6*s24*s34+s34^2)-4*d*(s23^2+7*s23*s24+5*s24^2-3*s23*s34-3*s24*s34+s34^2)+d^2*(s23^2+4*s23*s24+3*s24^2-2*s23*s34-2*s24*s34+s34^2))
  +den1*den3*den8*(d^3*s13-8*(16*s13+s14-3*s34)+8*d*(11*s13+s14-2*s34)-2*d^2*(9*s13+s14-s34))*s34
  -den1*den2*den7*(-d^3*s24+8*(s23+16*s24-3*s34)-8*d*(s23+11*s24-2*s34)+2*d^2*(s23+9*s24-s34))*s34
  +den3*den5*den7*(2*d^2*(2*s14^2-16*s14*s24-9*s24^2-5*s14*s34-16*s24*s34-7*s34^2)-8*d*(7*s14^2-17*s14*s24-11*s24^2-18*s24*s34-6*s34^2)+16*(7*s14^2-10*s14*s24-8*s24^2+3*s14*s34-12*s24*s34-3*s34^2)+d^3*(2*s14*s24+s24^2+s14*s34+2*s24*s34+s34^2))
  +den3*den6*den8*(-4+d)*s34*(d^2*(2*s13-s14+s34)+8*(7*s13-4*s14+2*s34)-2*d*(12*s13-7*s14+5*s34))
  -den2*den5*den7*(-4+d)*(-2*d*(7*s23-12*s24-5*s34)+8*(4*s23-7*s24-2*s34)+d^2*(s23-2*s24-s34))*s34
  +den1*den3*den6*den8*s34*(-18*d^2*s13^2+d^3*s13^2-16*(2*s13-s34)*(4*s13+s34)+8*d*(11*s13^2-s13*s34-s34^2))
  +den1*den2*den5*den7*s34*(-18*d^2*s24^2+d^3*s24^2-16*(2*s24-s34)*(4*s24+s34)+8*d*(11*s24^2-s24*s34-s34^2))
 )
 +Cf^2*Nc*flvsum[chargeQ^2]*(
  -8*den5*(4-4*d+d^2)
  -4*den1*(-4+d)*(28-12*d+d^2)
  -2*den2*(-4+d)*(32-14*d+d^2)
  -2*den4*(-4+d)*(32-14*d+d^2)
  -2*den3*(-248+144*d-24*d^2+d^3)
  +2*den7*(-8+d)*(-4+d)*(-14+5*d)
  +4*den8*(-244+176*d-41*d^2+3*d^3)
  +16*den6*(-6+d)*(-4+d)*(-3+d)
  +4*den8^2*(-6+d)*(4-4*d+d^2)*(s12+s13+s14)
  -4*den1*den7*(4-4*d+d^2)*(s23+s24)
  -2*den6*den4*(-4+d)*(d^2*(s13+s14+s23)-2*d*(7*s13+5*s14+7*s23+2*s24)+8*(4*s13+3*s14+4*s23+3*s24))
  +2*den4*den7*(d^3*(-s14-s23-s24)+8*(s12+6*s14+4*s23+9*s24)+2*d^2*(s12+3*s14+6*s23+9*s24)-4*d*(2*s12+6*s14+9*s23+17*s24))
  -2*den3*den4*(d^3*(-s13-s24)+2*d^2*(s12+7*s13-s14+9*s24)+8*(s12+7*s13-3*s14+16*s24)-4*d*(2*s12+13*s13-4*s14+22*s24))
  -2*den5*den4*(-4+d)*(32-14*d+d^2)*(s14-s24)
  +2*den2*den4*(-8+d)*(4-4*d+d^2)*s24
  +2*den4*den8*(8*(s12+7*s13+2*s14-6*s24)-4*d*(2*s12+13*s13+5*s14-6*s24)+2*d^2*(s12+7*s13+4*s14-3*s24)+d^3*(-s13-s14+s24))
  +2*den1*den2*(d^3*(s13+2*s24)-32*(2*s13+9*s24)-4*d^2*(4*s13+9*s24)+4*d*(15*s13+46*s24))
  +4*den7^2*(-8+d)*(4-4*d+d^2)*(s12+s23+s24)
  -2*den1*den6*(4*d*(15*s13+11*s23-24*s24)-8*(8*s13+6*s23-17*s24)-2*d^2*(8*s13+6*s23-11*s24)+d^3*(s13+s23-2*s24))
  -8*den4*den7*den8*(4-4*d+d^2)*s12*(s14-s24)
  -8*den4*den7^2*(4-4*d+d^2)*s14*(s12+s23+s24)
  +8*den4*den8^2*(4-4*d+d^2)*(s12+s13+s14)*s24
  -2*den5*den4*den7*(-4+d)*s14*(2*d*(2*s14-7*s23-12*s24)-8*(2*s14-4*s23-7*s24)+d^2*(s23+2*s24))
  +2*den3*den4*den7*s14*(-d^3*s24+2*d^2*(s12-s14+9*s24)-8*d*(s12-2*s14+11*s24)+8*(s12-3*s14+16*s24))
  +2*den6*den4*den8*(-4+d)*s24*(d^2*(2*s13+s14)+8*(7*s13+4*s14+2*s24)-2*d*(12*s13+7*s14+2*s24))
  -2*den2*den4*den8*s24*(-d^3*s13+2*d^2*(s12+9*s13+s24)-8*d*(s12+11*s13+2*s24)+8*(s12+16*s13+3*s24))
  +2*den2*den6*den4*s24*(d^3*s13-2*d^2*(9*s13+s23+s24)+8*d*(11*s13+s23+2*s24)-8*(16*s13+s23+3*s24))
  +4*den6*den4*den7*(d^2*(2*s14^2-2*s14*s23+s23^2+2*s23*s24+s24^2)-4*d*(3*s14^2-4*s14*s23+s23^2-s14*s24+3*s23*s24+s24^2)+4*(4*s14^2-8*s14*s23+s23^2-4*s14*s24+4*s23*s24+s24^2))
  +4*den5*den4*den8*(d^2*(s13^2+2*s13*s14+s14^2+2*s14*s24+2*s24^2)-4*d*(s13^2+3*s13*s14+s14^2+s13*s24+4*s14*s24+3*s24^2)+4*(s13^2+4*s13*s14+s14^2+4*s13*s24+8*s14*s24+4*s24^2))
  -2*den3*den5*den4*(-d^3*s24*(s13+s24)+2*d^2*(s13^2+s13*s14+9*s13*s24+9*s24^2)-8*d*(s13^2+2*s13*s14-s14^2+12*s13*s24-s14*s24+11*s24^2)+8*(s13^2+3*s13*s14-2*s14^2+20*s13*s24-4*s14*s24+16*s24^2))
  +2*den3*den5*den4*den7*s14*(18*d^2*s24^2-d^3*s24^2-16*(s14-2*s24)*(s14+4*s24)+8*d*(s14^2+s14*s24-11*s24^2))
  +2*den2*den6*den4*den8*s24*(-18*d^2*s13^2+d^3*s13^2-16*(4*s13-s24)*(2*s13+s24)+8*d*(11*s13^2+s13*s24-s24^2))
  +2*den3*den5*(d^3*(s13+s24)-2*d^2*(7*s13-s14+11*s24+5*s34)-8*(20*s13-9*s14+25*s24+12*s34)+4*d*(22*s13-8*s14+31*s24+15*s34))
  -4*den2*den5*(4-4*d+d^2)*(s24+s34)
  -2*den1*den5*(d^3*(s14+2*s23+s24)+4*d*(s14+34*s23+9*s24-4*s34)-2*d^2*(4*s14+14*s23+7*s24-s34)+8*(4*s14-26*s23-s24+3*s34))
  -4*den6*den7*(2*d^3*(s14-s23)-4*(28*s14-42*s23+10*s24-7*s34)+4*d*(23*s14-29*s23+5*s24-3*s34)+d^2*(-24*s14+26*s23-2*s24+s34))
  +2*den1*den3*(d^3*(s13+s24)+4*d*(22*s13+2*s14+13*s24-4*s34)-8*(16*s13+s14+7*s24-3*s34)-2*d^2*(9*s13+s14+7*s24-s34))
  +2*den3*den6*(-4+d)*(d^2*s13-2*d*(5*s13-2*s14-2*s24-2*s34)+8*(3*s13-s14-2*s24-2*s34))
  +4*den7*den8*(-4*(54*s12+s14-s24-37*s34)+16*d*(9*s12-7*s34)+2*d^3*(s12-s34)+d^2*(-30*s12+s14-s24+27*s34))
  -2*den5*den7*(-8*d*(s14-6*s23-12*s24+2*s34)+d^3*(2*s14+s23+2*s24+2*s34)+16*(5*s14-3*s23-8*s24+6*s34)-2*d^2*(6*s14+7*s23+12*s24+6*s34))
  -2*den3*den8*(d^3*(s12+s13+2*s34)-2*d^2*(8*s12+10*s13+11*s34)-8*(8*s12+10*s13+17*s34)+4*d*(15*s12+19*s13+24*s34))
  -2*den2*den7*(d^3*(s12+s24+2*s34)-2*d^2*(6*s12+s23+9*s24+12*s34)-8*(4*s12+s23+9*s24+22*s34)+4*d*(9*s12+2*s23+17*s24+28*s34))
  -2*den3*den7*(-8*(17*s12-18*s14+32*s24-3*s34)+d^3*(s12+2*s24+s34)+8*d*(12*s12-9*s14+22*s24+s34)-4*d^2*(5*s12-2*s14+9*s24+2*s34))
  -2*den2*den6*(-2+d)*(d^2*(s13-s24+s34)-2*d*(7*s13-s24+5*s34)+4*(8*s13-s24+6*s34))
  -2*den2*den8*(-4+d)*(d^2*(s12+s13+s34)+8*(4*s12+4*s13+3*s24+3*s34)-2*d*(7*s12+7*s13+2*s24+5*s34))
  +2*den6*den8*(-4+d)*(-2*d*(16*s13+7*s14+s24-5*s34)+8*(9*s13+4*s14-2*s34)+d^2*(2*s13+s14+s24-s34))
  -2*den1*den8*(-8*(32*s13+17*s14+3*s24-15*s34)+8*d*(22*s13+12*s14-s24-10*s34)-4*d^2*(9*s13+5*s14-2*s24-4*s34)+d^3*(2*s13+s14-s24-s34))
  +4*den5^2*(-6+d)*(4-4*d+d^2)*(s14+s24+s34)
  +4*den6^2*(-4+d)*(4-4*d+d^2)*(s13+s23+s34)
  +4*den5*den6*(d^2*(-24*s14-s23-22*s24-26*s34)+2*d^3*(s14+s24+s34)+4*d*(23*s14+3*s23+18*s24+29*s34)-4*(28*s14+7*s23+18*s24+42*s34))
  -4*den5*den8*(-4*(5*s13-46*s14-29*s24-34*s34)+4*d*(s13-32*s14-22*s24-25*s34)-2*d^3*(s14+s24+s34)+d^2*(s13+28*s14+23*s24+24*s34))
  +8*den3*den5^2*(4-4*d+d^2)*s13*(s14+s24+s34)
  +8*den3*den5*den8*(4-4*d+d^2)*s14*(s13-s34)
  -8*den3*den8^2*(4-4*d+d^2)*(s12+s13+s14)*s34
  -8*den1*den5^2*(4-4*d+d^2)*s23*(s14+s24+s34)
  +8*den2*den6*den7*(4-4*d+d^2)*s23*(s24-s34)
  -8*den2*den7^2*(4-4*d+d^2)*(s12+s23+s24)*s34
  +8*den1*den6^2*(4-4*d+d^2)*s24*(s13+s23+s34)
  +8*den2*den6^2*(4-4*d+d^2)*s24*(s13+s23+s34)
  -8*den1*den5*den6*(4-4*d+d^2)*(s23-s24)*s34
  +2*den2*den6*den8*(-4+d)*s24*(d^2*(2*s13+s34)+8*(7*s13+2*s24+4*s34)-2*d*(12*s13+2*s24+7*s34))
  -2*den1*den2*den5*(d^3*(s23-s24)*s24+2*d^2*(s23^2-9*s23*s24+9*s24^2-s23*s34)+8*(s23^2-20*s23*s24+16*s24^2-3*s23*s34-4*s24*s34-2*s34^2)-8*d*(s23^2-12*s23*s24+11*s24^2-2*s23*s34-s24*s34-s34^2))
  +2*den1*den3*den6*(d^3*s13*(s13+s24)-2*d^2*(9*s13^2+9*s13*s24+s24^2+s24*s34)-8*(16*s13^2+20*s13*s24+s24^2-4*s13*s34+3*s24*s34-2*s34^2)+8*d*(11*s13^2+12*s13*s24+s24^2-s13*s34+2*s24*s34-s34^2))
  -2*den1*den5*den7*(-2*d^2*(7*s23^2+16*s23*s24+9*s24^2-9*s23*s34)+d^3*(s23^2+2*s23*s24+s24^2-s23*s34)-16*(3*s23^2+12*s23*s24+8*s24^2-9*s23*s34-2*s24*s34-s34^2)+8*d*(6*s23^2+18*s23*s24+11*s24^2-12*s23*s34-s24*s34-s34^2))
  -2*den1*den6*den8*(d^3*(s13^2-2*s13*s24+s24^2+s24*s34)-2*d^2*(9*s13^2-16*s13*s24+7*s24^2+9*s24*s34)-16*(8*s13^2-12*s13*s24+3*s24^2-2*s13*s34+9*s24*s34-s34^2)+8*d*(11*s13^2-18*s13*s24+6*s24^2-s13*s34+12*s24*s34-s34^2))
  -2*den1*den3*den8*(d^3*s13-8*(16*s13+s14-3*s34)+8*d*(11*s13+s14-2*s34)-2*d^2*(9*s13+s14-s34))*s34
  +2*den1*den2*den7*(-d^3*s24+8*(s23+16*s24-3*s34)-8*d*(s23+11*s24-2*s34)+2*d^2*(s23+9*s24-s34))*s34
  +4*den1*den6*den7*(4*(s23^2+12*s23*s24+9*s24^2-4*s23*s34-6*s24*s34+s34^2)-4*d*(s23^2+7*s23*s24+5*s24^2-3*s23*s34-3*s24*s34+s34^2)+d^2*(s23^2+4*s23*s24+3*s24^2-2*s23*s34-2*s24*s34+s34^2))
  +4*den3*den5*den6*(d^2*(2*s14^2+4*s14*s24+3*s24^2+2*s14*s34+4*s24*s34+s34^2)-4*d*(3*s14^2+7*s14*s24+5*s24^2+4*s14*s34+7*s24*s34+s34^2)+4*(4*s14^2+12*s14*s24+9*s24^2+8*s14*s34+12*s24*s34+s34^2))
  +4*den2*den5*den6*(d^2*(s23^2-2*s23*s24+3*s24^2-2*s23*s34+4*s24*s34+s34^2)-4*d*(s23^2-3*s23*s24+5*s24^2-3*s23*s34+7*s24*s34+s34^2)+4*(s23^2-6*s23*s24+9*s24^2-4*s23*s34+12*s24*s34+s34^2))
  +4*den2*den7*den8*(d^2*(s12^2+2*s12*s24+s24^2-2*s12*s34+2*s34^2)-4*d*(s12^2+3*s12*s24+s24^2-4*s12*s34-s24*s34+3*s34^2)+4*(s12^2+4*s12*s24+s24^2-8*s12*s34-4*s24*s34+4*s34^2))
  +4*den1*den5*den8*(d^2*(s14^2+4*s14*s24+3*s24^2+2*s14*s34+4*s24*s34+2*s34^2)-4*d*(s14^2+7*s14*s24+5*s24^2+4*s14*s34+7*s24*s34+3*s34^2)+4*(s14^2+12*s14*s24+9*s24^2+8*s14*s34+12*s24*s34+4*s34^2))
  +4*den3*den7*den8*(d^2*(s12^2-2*s12*s14+s14^2-4*s12*s34+2*s14*s34+3*s34^2)-4*d*(s12^2-3*s12*s14+s14^2-7*s12*s34+3*s14*s34+5*s34^2)+4*(s12^2-4*s12*s14+s14^2-12*s12*s34+6*s14*s34+9*s34^2))
  -2*den3*den6*den8*(-4+d)*s34*(d^2*(2*s13-s14+s34)+8*(7*s13-4*s14+2*s34)-2*d*(12*s13-7*s14+5*s34))
  +2*den2*den5*den7*(-4+d)*(-2*d*(7*s23-12*s24-5*s34)+8*(4*s23-7*s24-2*s34)+d^2*(s23-2*s24-s34))*s34
  -2*den3*den5*den7*(2*d^2*(2*s14^2-16*s14*s24-9*s24^2-5*s14*s34-16*s24*s34-7*s34^2)-8*d*(7*s14^2-17*s14*s24-11*s24^2-18*s24*s34-6*s34^2)+16*(7*s14^2-10*s14*s24-8*s24^2+3*s14*s34-12*s24*s34-3*s34^2)+d^3*(2*s14*s24+s24^2+s14*s34+2*s24*s34+s34^2))
  -2*den1*den3*den6*den8*s34*(-18*d^2*s13^2+d^3*s13^2-16*(2*s13-s34)*(4*s13+s34)+8*d*(11*s13^2-s13*s34-s34^2))
  -2*den1*den2*den5*den7*s34*(-18*d^2*s24^2+d^3*s24^2-16*(2*s24-s34)*(4*s24+s34)+8*d*(11*s24^2-s24*s34-s34^2))
 )
 +Cf*Nc*Tf*flvsum[1]*flvsum[chargeQ^2]*(
  -16*den6*(-4+d)*(-2+d)
  -16*den7*(-4+d)*(-2+d)
  -16*den8*(-4+d)*(-2+d)
  +8*den1*(-4+d)*(-2+d)
  +8*den2*(-4+d)*(-2+d)
  +8*den3*(-4+d)*(-2+d)
  +8*den4*(-4+d)*(-2+d)
  -8*den8^2*(4-4*d+d^2)*(s12+s13+s14)
  +4*den4*den8*(d^2*(s13+s14)-4*(2*s12-s13-s14-4*s24)+4*d*(s12-s13-s14-s24))
  -8*den7^2*(4-4*d+d^2)*(s12+s23+s24)
  +4*den4*den7*(-4*(2*s12-4*s14-s23-s24)+4*d*(s12-s14-s23-s24)+d^2*(s23+s24))
  -16*den4*den7^2*(-2+d)*s14*(s12+s23+s24)
  -16*den4*den8^2*(-2+d)*(s12+s13+s14)*s24
  -16*den4^2*den7*(-2+d)*s14*(s14-2*s24)
  +32*den4^2*den7*den8*(-2+d)*s12*s14*s24
  -16*den4^2*den7^2*(-2+d)*s14^2*(s12+s23+s24)
  -16*den1^2*den5*(-2+d)*s23*(s23-2*s24)
  +16*den4^2*den8*(-2+d)*(2*s14-s24)*s24
  +16*den1^2*den6*(-2+d)*(2*s23-s24)*s24
  +16*den4*den7*den8*(d*(s12^2+s12*s14+s14^2+s12*s24+s24^2)-2*(s12^2+s12*s14+s14^2+s12*s24+2*s14*s24+s24^2))
  -16*den4^2*den8^2*(-2+d)*(s12+s13+s14)*s24^2
  +4*den3*den8*(d^2*(s12+s13)-4*d*(s12+s13-s14+s34)+4*(s12+s13-2*s14+4*s34))
  +4*den2*den7*(d^2*(s12+s24)-4*d*(s12-s23+s24+s34)+4*(s12-2*s23+s24+4*s34))
  +8*den6*den7*(-4+d)*(d*(s14-s23)-2*(s14-2*s23-s24-s34))
  +4*den1*den6*(d^2*(s13+s23)+4*(s13+s23+4*s24-2*s34)-4*d*(s13+s23+s24-s34))
  +4*den1*den5*(d^2*(s14+s24)+4*(s14+4*s23+s24-2*s34)-4*d*(s14+s23+s24-s34))
  -8*den6^2*(4-4*d+d^2)*(s13+s23+s34)
  -8*den5^2*(4-4*d+d^2)*(s14+s24+s34)
  +8*den5*den8*(-4+d)*(d*(-s14-s24-s34)+2*(s13+2*s14+s24+2*s34))
  -8*den5*den6*(-4+d)*(d*(s14+s24+s34)-2*(s14+s23+2*s24+2*s34))
  +4*den2*den6*(d^2*(s13+s34)-4*d*(s13-s23+s24+s34)+4*(s13-2*s23+4*s24+s34))
  -4*den3*den5*(d^2*(-s24-s34)-4*(4*s13-2*s14+s24+s34)+4*d*(s13-s14+s24+s34))
  -8*den7*den8*(-4+d)*(d*(s12-s34)-2*(2*s12+s14+s24-s34))
  -16*den3*den5^2*(-2+d)*s13*(s14+s24+s34)
  -16*den3^2*den5*(-2+d)*s13*(s13-2*s34)
  -16*den3^2*den5^2*(-2+d)*s13^2*(s14+s24+s34)
  -16*den3*den8^2*(-2+d)*(s12+s13+s14)*s34
  +32*den3^2*den5*den8*(-2+d)*s13*s14*s34
  -16*den1*den5^2*(-2+d)*s23*(s14+s24+s34)
  -16*den1^2*den5^2*(-2+d)*s23^2*(s14+s24+s34)
  -16*den1*den6^2*(-2+d)*s24*(s13+s23+s34)
  -16*den2*den6^2*(-2+d)*s24*(s13+s23+s34)
  -16*den2*den7^2*(-2+d)*(s12+s23+s24)*s34
  -16*den2^2*den6*(-2+d)*s24*(s24-2*s34)
  +32*den1^2*den5*den6*(-2+d)*s23*s24*s34
  +32*den2^2*den6*den7*(-2+d)*s23*s24*s34
  -16*den1^2*den6^2*(-2+d)*s24^2*(s13+s23+s34)
  -16*den2^2*den6^2*(-2+d)*s24^2*(s13+s23+s34)
  +16*den3^2*den8*(-2+d)*(2*s13-s34)*s34
  +16*den2^2*den7*(-2+d)*(2*s24-s34)*s34
  +16*den3*den5*den8*(d*(s13^2+s13*s14+s14^2+s14*s34+s34^2)-2*(s13^2+s13*s14+s14^2+2*s13*s34+s14*s34+s34^2))
  +16*den2*den6*den7*(d*(s23^2+s23*s24+s24^2+s23*s34+s34^2)-2*(s23^2+s23*s24+s24^2+s23*s34+2*s24*s34+s34^2))
  +16*den1*den5*den6*(d*(s23^2+s24^2+s23*s34+s24*s34+s34^2)-2*(s23^2+2*s23*s24+s24^2+s23*s34+s24*s34+s34^2))
  -16*den3^2*den8^2*(-2+d)*(s12+s13+s14)*s34^2
  -16*den2^2*den7^2*(-2+d)*(s12+s23+s24)*s34^2
 )
) /. {
  den1 -> 1/s12,
  den2 -> 1/s14,
  den3 -> 1/s23,
  den4 -> 1/s34,
  den5 -> 1/(s12+s13+s23),
  den6 -> 1/(s12+s14+s24),
  den7 -> 1/(s13+s14+s34),
  den8 -> 1/(s23+s24+s34)
};

numpoint = {s12, s13, s14, s23, s24, s34} // Map[(# -> RandomInteger[{1, 99}])&];
FailUnless[Together[amplitude2 - correctamplitude2 /. numpoint] === 0];
