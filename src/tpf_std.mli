open Tpf

val unit : unit g0
val pair : ('a, 'b, 'a * 'b) g2
val list : ('a, 'a list) g1
val seq : ('a, 'a Seq.t) g1
val option : ('a, 'a option) g1
val result : ('a, 'b, ('a, 'b) result) g2

type 'a eq = 'a -> 'a -> bool

val eq0 : ('x) g0 -> 'x eq
val eq1 : ('a, 'x) g1 -> 'a eq -> 'x eq
val eq2 : ('a, 'b, 'x) g2 -> 'a eq -> 'b eq -> 'x eq
val eq3 : ('a, 'b, 'c, 'x) g3 -> 'a eq -> 'b eq -> 'c eq -> 'x eq
val eq4 : ('a, 'b, 'c, 'd, 'x) g4 -> 'a eq -> 'b eq -> 'c eq -> 'd eq -> 'x eq
val eq5 : ('a, 'b, 'c, 'd, 'e, 'x) g5 -> 'a eq -> 'b eq -> 'c eq -> 'd eq -> 'e eq -> 'x eq
val eq6 : ('a, 'b, 'c, 'd, 'e, 'f, 'x) g6 -> 'a eq -> 'b eq -> 'c eq -> 'd eq -> 'e eq -> 'f eq -> 'x eq
val eq7 : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'x) g7 -> 'a eq -> 'b eq -> 'c eq -> 'd eq -> 'e eq -> 'f eq -> 'g eq -> 'x eq
val eq8 : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'x) g8 -> 'a eq -> 'b eq -> 'c eq -> 'd eq -> 'e eq -> 'f eq -> 'g eq -> 'h eq -> 'x eq
val eq9 : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'x) g9 -> 'a eq -> 'b eq -> 'c eq -> 'd eq -> 'e eq -> 'f eq -> 'g eq -> 'h eq -> 'i eq -> 'x eq

type 'a cmp = 'a -> 'a -> int

val cmp0 : ('x) g0 -> 'x cmp
val cmp1 : ('a, 'x) g1 -> 'a cmp -> 'x cmp
val cmp2 : ('a, 'b, 'x) g2 -> 'a cmp -> 'b cmp -> 'x cmp
val cmp3 : ('a, 'b, 'c, 'x) g3 -> 'a cmp -> 'b cmp -> 'c cmp -> 'x cmp
val cmp4 : ('a, 'b, 'c, 'd, 'x) g4 -> 'a cmp -> 'b cmp -> 'c cmp -> 'd cmp -> 'x cmp
val cmp5 : ('a, 'b, 'c, 'd, 'e, 'x) g5 -> 'a cmp -> 'b cmp -> 'c cmp -> 'd cmp -> 'e cmp -> 'x cmp
val cmp6 : ('a, 'b, 'c, 'd, 'e, 'f, 'x) g6 -> 'a cmp -> 'b cmp -> 'c cmp -> 'd cmp -> 'e cmp -> 'f cmp -> 'x cmp
val cmp7 : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'x) g7 -> 'a cmp -> 'b cmp -> 'c cmp -> 'd cmp -> 'e cmp -> 'f cmp -> 'g cmp -> 'x cmp
val cmp8 : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'x) g8 -> 'a cmp -> 'b cmp -> 'c cmp -> 'd cmp -> 'e cmp -> 'f cmp -> 'g cmp -> 'h cmp -> 'x cmp
val cmp9 : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'x) g9 -> 'a cmp -> 'b cmp -> 'c cmp -> 'd cmp -> 'e cmp -> 'f cmp -> 'g cmp -> 'h cmp -> 'i cmp -> 'x cmp
