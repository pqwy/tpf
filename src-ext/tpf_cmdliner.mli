(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Tpf
open Cmdliner

include P with type 'a q := 'a Arg.conv

module Opt: sig
  val g : ('a, p) schema -> 'a Term.t
  include Data with
    type 'a r := 'a Term.t and type 'a q := 'a Arg.conv
end
module Opt_def: sig
  val g : ('a, p) view -> 'a -> 'a Term.t
  include Data with
    type 'a r := 'a -> 'a Term.t and type 'a q := 'a Arg.conv
end
module Pos: sig
  val g : ('a, p) schema -> int -> 'a Term.t * int
  include Data with
    type 'a r := int -> 'a Term.t * int and type 'a q := 'a Arg.conv
end
module Pos_def: sig
  val g : ('a, p) view -> 'a -> int -> 'a Term.t * int
  include Data with
    type 'a r := 'a -> int -> 'a Term.t * int and type 'a q := 'a Arg.conv
end
