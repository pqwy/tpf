open Tpf

val unit : unit data0
val pair : ('a, 'b, 'a * 'b) data2
val triple : ('a, 'b, 'c, 'a * 'b * 'c) data3
val quadruple : ('a, 'b, 'c, 'd, 'a * 'b * 'c * 'd) data4
val list : ('a, 'a list) data1
val seq : ('a, 'a Seq.t) data1
val option : ('a, 'a option) data1
val result : ('a, 'b, ('a, 'b) result) data2

module Eq: sig
  type 'a eq = 'a -> 'a -> bool
  include S with type 'a q := 'a eq and type 'a r := 'a eq
end

module Cmp: sig
  type 'a cmp = 'a -> 'a -> int
  include S with type 'a q := 'a cmp and type 'a r := 'a cmp
end
