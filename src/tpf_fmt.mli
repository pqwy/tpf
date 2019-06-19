open Tpf

val g_pp0: 'x g0 -> 'x Fmt.t
val g_pp1: ('a, 'x) g1 -> 'a Fmt.t -> 'x Fmt.t
val g_pp2: ('a, 'b, 'x) g2 -> 'a Fmt.t -> 'b Fmt.t -> 'x Fmt.t
val g_pp3: ('a, 'b, 'c, 'x) g3 -> 'a Fmt.t -> 'b Fmt.t -> 'c Fmt.t -> 'x Fmt.t
val g_pp4: ('a, 'b, 'c, 'd, 'x) g4 -> 'a Fmt.t -> 'b Fmt.t -> 'c Fmt.t ->
           'd Fmt.t -> 'x Fmt.t
val g_pp5: ('a, 'b, 'c, 'd, 'e, 'x) g5 -> 'a Fmt.t -> 'b Fmt.t -> 'c Fmt.t ->
           'd Fmt.t -> 'e Fmt.t -> 'x Fmt.t
val g_pp6: ('a, 'b, 'c, 'd, 'e, 'f, 'x) g6 -> 'a Fmt.t -> 'b Fmt.t -> 'c Fmt.t ->
           'd Fmt.t -> 'e Fmt.t -> 'f Fmt.t ->'x Fmt.t
val g_pp7: ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'x) g7 -> 'a Fmt.t -> 'b Fmt.t ->
           'c Fmt.t -> 'd Fmt.t -> 'e Fmt.t -> 'f Fmt.t -> 'g Fmt.t -> 'x Fmt.t
val g_pp8: ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'x) g8 -> 'a Fmt.t -> 'b Fmt.t ->
           'c Fmt.t -> 'd Fmt.t -> 'e Fmt.t -> 'f Fmt.t -> 'g Fmt.t ->
           'h Fmt.t -> 'x Fmt.t
val g_pp9: ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'x) g9 -> 'a Fmt.t ->
           'b Fmt.t -> 'c Fmt.t -> 'd Fmt.t -> 'e Fmt.t -> 'f Fmt.t ->
           'g Fmt.t -> 'h Fmt.t -> 'i Fmt.t -> 'x Fmt.t

(* To construct functions > 9. But there is no g10. Drop? *)
type p
val g_pp: ('a, p) Tpf.view -> 'a Fmt.t
val ($) : (('a, p) app -> 'b) -> 'a Fmt.t -> 'b
