(* # Scalar Feynman rules
 *
 * These are the Feynman rules for a model with scalar particles
 * names "s", coupled as
 *
 * $$ L = \frac{g_3}{3!} \phi^3 + \frac{g_4}{4!} \phi^4 + \frac{g_5}{5!} \phi^5 + \frac{g_6}{6!} \phi^6. $$
 *)

(* ## QGraf model
 *)

$QGrafModel = "\
# Scalars
[s, s, +]
# Vertices
[s, s, s]
[s, s, s, s]
[s, s, s, s, s]
[s, s, s, s, s, s]";

$QGrafOptions = "nosnail,notadpole";

$QGrafExtra = "";

(* ## Field classification
 *)

$MasslessFieldPattern = "s";

(* ## Propagators
 *)

Amplitude[P["s", fi1_, fi2_, _, _, p_]] := I den[p]

(* ## Vertices
 *)

Amplitude[V[_, "sss", fi1_, p1_, fi2_, p2_, fi3_, p3_]] := I g3
Amplitude[V[_, "ssss", fi1_, p1_, fi2_, p2_, fi3_, p3_, fi4_, p4_]] := I g4
Amplitude[V[_, "sssss", fi1_, p1_, fi2_, p2_, fi3_, p3_, fi4_, p4_, fi5_, p5_]] := I g5
Amplitude[V[_, "ssssss", fi1_, p1_, fi2_, p2_, fi3_, p3_, fi4_, p4_, fi5_, p5_, fi5_, p5_]] := I g6

(* ## Final states
 *)

CutAmplitudeGlue[F[f:"s", fi_, _, mom_], F[f_, fi_, _, mom_]] := 1

(* ## Graphviz & TikZ styles
 *)

FieldGraphvizColor["s"] = 10;
FieldTikZStyle["s"] = "edge";
