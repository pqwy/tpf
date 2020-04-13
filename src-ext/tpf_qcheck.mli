(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(** Generic QCheck.arbitrary.

    Note that if your type has interesting recursion, this will probably not
    work as it is not possible to fixpoint through {!arbitrary}.

    Most of the work here is being done by {!Tpf_std.Random} and {!Tpf_fmt},
    which you can use directly if this is the case. *)

open Tpf
open QCheck

(** {1 Generic [arbitrary]} *)
include P with type 'a q := 'a arbitrary
val g_arb : ('a, p) view -> ('a, p) schema -> ?base:'a -> int -> 'a arbitrary
(** [g_arb view schema ~base size] is the {!arbitrary} instance over ['a].

    [base] and [size] are used by {!Tpf_std.Random.g_gen} to bound the recursion
    depth. *)

(** {1 [data] interface} *)
include Data
  with type 'a q := 'a arbitrary
  and type 'a r := ?base:'a -> int -> 'a arbitrary
