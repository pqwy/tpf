open Tpf

val unit : unit g0
val pair : ('a, 'b, 'a * 'b) g2
val list : ('a, 'a list) g1
val seq : ('a, 'a Seq.t) g1
val option : ('a, 'a option) g1
val result : ('a, 'b, ('a, 'b) result) g2
