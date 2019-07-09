(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(** Tpf nice-to-haves. *)

open Tpf

val fix: (('a -> 'b) -> ('a -> 'b)) -> ('a -> 'b)
(** [fix f x] is [f (fix f) x], the usual fixpoint on functions.
    This one maximizes sharing: [f _] is evaluated only once.
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

(** {1:applicative Applicative traversals}

    {{!Tpf.core}Spines} essentially encode values as expressions in the free
    applicative functor.

    This API provides a way to uniformly eliminate spines by interpreting them
    in a chosen applicative. This captures a slightly restricted, but
    significant class of generic functions.

    {b Note.} You can safely ignore this if you don't feel like applicatives
    today. *)

(* Note - gfuns break symmetry, but are necessary to internalize recursion :/ . *)

(** {{!Tpf.view}View}-flavored applicative. *)
module type AppV = sig
  type 'a t
  val pure : 'a -> 'a t
  val app : ('a -> 'b) t -> 'a t -> 'b t
  val gfun : meta -> 'a t -> 'a t
end

(** {{!Tpf.view}View} traversal. *)
module AppV (A: AppV): sig
  include P with type 'a q := 'a -> 'a A.t
  val gfun : ('a, p) view -> 'a -> 'a A.t
  include Data with type 'a q := 'a -> 'a A.t and type 'a r := 'a -> 'a A.t
end

(** {{!Tpf.schema}Schema}-flavored applicative. *)
module type AppS = sig
  type 'a t
  val pure : 'a -> 'a t
  val app : ('a -> 'b) t -> 'a t -> 'b t
  val retract : 'a t Lazy.t -> 'a t
  val gfun : ('a t Lazy.t * meta) list -> 'a t
end

(** {{!Tpf.schema}Schema} traversal. *)
module AppS (A: AppS) : sig
  include P with type 'a q := 'a A.t
  val gfun : ('a, p) schema -> 'a A.t
  include Data with type 'a q := 'a A.t and type 'a r := 'a A.t
end
