(* # Scalar Feynman rules
 *
 * These are the Feynman rules for a model with scalar particles
 * names "s", coupled as
 *
 * $$ L = g3/3! phi^3 + g4/4! phi^4 + g5/5! phi^5 + g6/6! phi^6. $$
 *)

$QGrafModel = "
# Scalars
[s, s, +]
# Vertices
[s, s, s]
[s, s, s, s]
[s, s, s, s, s]
[s, s, s, s, s, s]
";

$MasslessFieldPattern = "s";

$FermionFieldPattern = Alternatives[];

(* Propagators *)
Amplitude[P["s", fi1_, fi2_, _, _, p_]] := I den[p]

(* Vertices *)
Amplitude[V[_, "sss", fi1_, p1_, fi2_, p2_, fi3_, p3_]] := I g3
Amplitude[V[_, "ssss", fi1_, p1_, fi2_, p2_, fi3_, p3_, fi4_, p4_]] := I g4
Amplitude[V[_, "sssss", fi1_, p1_, fi2_, p2_, fi3_, p3_, fi4_, p4_, fi5_, p5_]] := I g5
Amplitude[V[_, "ssssss", fi1_, p1_, fi2_, p2_, fi3_, p3_, fi4_, p4_, fi5_, p5_, fi5_, p5_]] := I g6

(* Final states *)
CutAmplitudeGlue[F[f:"s", fi_, _, mom_], F[f_, fi_, _, mom_]] := 1

(* Graphvis & TikZ styles *)
FieldGraphvizColor["s"] = 10;
FieldTikZStyle["s"] = "edge";
