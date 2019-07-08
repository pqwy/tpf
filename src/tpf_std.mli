(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(** Tpf nice-to-haves.

    Neither the generic function definitions nor their use depends on this
    module, but it is nice to have. *)

open Tpf

val fix: (('a -> 'b) -> ('a -> 'b)) -> ('a -> 'b)
(** [fix f x] is [f (fix f) x], the usual fixpoint on functions.

    This one maximizes sharing. It only causes [f _] to be evaluated once.
    [fix] likes functions of the form [fun self -> ... (fun x -> ...)].

    Sharing is caring. *)

(** {1 Stdlib types}

    {{!data0}[data]} representations of some common types. *)

val unit : unit data0
val pair : ('a, 'b, 'a * 'b) data2
val triple : ('a, 'b, 'c, 'a * 'b * 'c) data3
val quadruple : ('a, 'b, 'c, 'd, 'a * 'b * 'c * 'd) data4

val list : ('a, 'a list) data1
val seq : ('a, 'a Seq.t) data1
val option : ('a, 'a option) data1
val result : ('a, 'b, ('a, 'b) result) data2

(** {1 Useful generic functions} *)

(** Equality.

    It behaves like the built-in one, but maintains abstraction. *)
module Eq: sig
  type 'a eq = 'a -> 'a -> bool
  type p
  type q
  val ($$) : (('a, p) app -> 'b) * (('a, q) app -> 'c) -> 'a eq -> 'b * 'c
  val g_eq : ('a, p) view * ('a, q) view -> 'a eq
  include Data with type 'a q := 'a eq and type 'a r := 'a eq
end

(** Comparison.

    It behaves like the built-in one, using the lexicographic ordering on
    constructor arguments, but maintains abstraction. *)
module Cmp: sig
  type 'a cmp = 'a -> 'a -> int
  type p
  type q
  val ($$) : (('a, p) app -> 'b) * (('a, q) app -> 'c) -> 'a cmp -> 'b * 'c
  val g_cmp : ('a, p) view * ('a, q) view -> 'a cmp
  include Data with type 'a q := 'a cmp and type 'a r := 'a cmp
end

(** [iter]. *)
module Iter: sig
  include P with type 'a q := 'a -> unit
  val g_iter : ('a, p) view -> 'a -> unit
  include Data with type 'a q := 'a -> unit and type 'a r := 'a -> unit
end

(** [Endo] is like [map] which cannot changed the type. *)
module Endo: sig
  include P with type 'a q := 'a -> 'a
  val g_endo : ('a, p) view -> 'a -> 'a
  include Data with type 'a q := 'a -> 'a and type 'a r := 'a -> 'a
end

(** Generic folds.

    These are a little different from the usual folds. Instead of being
    parameterized per constructor, they are parameterized per contained type. *)
module Fold: sig
  module Make (T: sig type t end) : sig
    include P with type 'a q := 'a -> T.t -> T.t
    val g_fold : ('a, p) view -> 'a -> T.t -> T.t
  end
  type ('a, 'x) t = 'a -> 'x -> 'x
  val data0 : 'a data0 ->
              ('a, 'x) t
  val data1 : ('a, 'b) data1 ->
              ('a, 'x) t -> ('b, 'x) t
  val data2 : ('a, 'b, 'c) data2 ->
              ('a, 'x) t -> ('b, 'x) t -> ('c, 'x) t
  val data3 : ('a, 'b, 'c, 'd) data3 ->
              ('a, 'x) t -> ('b, 'x) t -> ('c, 'x) t -> ('d, 'x) t
  val data4 : ('a, 'b, 'c, 'd, 'e) data4 ->
              ('a, 'x) t -> ('b, 'x) t -> ('c, 'x) t -> ('d, 'x) t -> ('e, 'x) t
  val data5 : ('a, 'b, 'c, 'd, 'e, 'f) data5 ->
              ('a, 'x) t -> ('b, 'x) t -> ('c, 'x) t -> ('d, 'x) t ->
              ('e, 'x) t -> ('f, 'x) t
  val data6 : ('a, 'b, 'c, 'd, 'e, 'f, 'g) data6 ->
              ('a, 'x) t -> ('b, 'x) t -> ('c, 'x) t -> ('d, 'x) t ->
              ('e, 'x) t -> ('f, 'x) t -> ('g, 'x) t
  (* val data7 : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) data7 -> *)
  (*             ('a, 'x) t -> ('b, 'x) t -> ('c, 'x) t -> ('d, 'x) t -> *)
  (*             ('e, 'x) t -> ('f, 'x) t -> ('g, 'x) t -> ('h, 'x) t *)

end

(** Random type inhabitants. *)
module Random: sig
  open Random
  include P with type 'a q := State.t -> 'a
  val g_gen : ('a, p) schema -> ?base:'a -> int -> State.t -> 'a
  (** [g_gen schema ~base size s] generates a random inhabitant of the type
      described by [schema] using the random state [s].

      The [size] parameter limits the recursion depth. When [size < 1],
      {ul
      {- returns [base], if specified; or}
      {- returns the first nullary constructor in [schema], if any; or}
      {- raises.}}

      @raise Invalid_argument if [size < 1], [base] is not specified, and there
      are no nullary constructors in ['a]. *)

  include Data
    with type 'a q := State.t -> 'a
    and type 'a r := ?base:'a -> int -> State.t -> 'a
end
