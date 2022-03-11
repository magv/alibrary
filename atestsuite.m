(* # Amplitude library test suite (`atestsuite.m`)
 *
 * This file contains a collection of tests for [[alibrary.m]]. It
 * can be executed as a script.
 *)

Get["alibrary.m"];

(*
 * ## Tests for [[utils.m]]
 *)

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

