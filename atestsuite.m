(* # Amplitude library test suite (`atestsuite.m`)
 *
 * This file contains a collection of tests for [[alibrary.m]]. It
 * can be executed as a script.
 *)

Get["alibrary.m"];

(*
 * ## Tests for [[utils.m]]
 *)

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

(*
 * ## Tests for [[alibrary.m]]
 *)

FailUnless[
  ExpandScalarProducts[x|y|z|q][sp[2x+3y, z]] === 2 sp[x,z] + 3 sp[y,z],
  ExpandScalarProducts[x|y|z|q][sp[x+y, x-y]] === sp[x,x] - sp[y,y],
  ExpandScalarProducts[x|y|z|q][sp[a x + b y, c x]] === a c sp[x,x] + b c sp[x,y]
]
