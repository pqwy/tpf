open Tpf
open Cmdliner

type p
val (!:) : 'a Arg.conv -> ('a, p) app

module Opt: sig
  val gfun : ('a, p) schema -> 'a Term.t
  include S with type 'a r := 'a Term.t and type 'a q := 'a Arg.conv
end
module Opt_def: sig
  val gfun : ('a, p) view -> 'a -> 'a Term.t
  include S with type 'a r := 'a -> 'a Term.t and type 'a q := 'a Arg.conv
end
module Pos: sig
  val gfun : ('a, p) schema -> int -> 'a Term.t * int
  include S with type 'a r := int -> 'a Term.t * int and type 'a q := 'a Arg.conv
end
module Pos_def: sig
  val gfun : ('a, p) view -> 'a -> int -> 'a Term.t * int
  include S with type 'a r := 'a -> int -> 'a Term.t * int and type 'a q := 'a Arg.conv
end
