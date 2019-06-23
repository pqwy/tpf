(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

type (+'a, +'f) app

(** {1:core Core}

    Two types of spines, view and schema, are at the heart of generic
    representation. *)

(** [V] is for [View].

    An instantiated {e view spine} encodes a value and its pointwise operations
    (or queries).

    The type parameters are:
    {ul
    {- [a] — the type this spine represents;}
    {- [r] — the type of recursive points; and}
    {- [q] — the type constructor for queries.}} *)
module V : sig
  type (+_, _, +_) spine =
  | K : 'a -> ('a, 'r, 'q) spine
  | A : ('a -> 'b, 'r, 'q) spine * 'a * ('a, 'q) app -> ('b, 'r, 'q) spine
  | R : ('r -> 'b, 'r, 'q) spine * 'r -> ('b, 'r, 'q) spine
end

(** [S] is for [Schema].

    An instantiated {e schema spine} encodes a type constructor and its
    pointwise operations (or queries).

    The type parameters are:
    {ul
    {- [a] — the type this spine represents;}
    {- [r] — the type of recursive points; and}
    {- [q] — the type constructor for queries.}} *)
module S : sig
  type (+_, _, +_) spine =
  | K : 'a -> ('a, 'r, 'q) spine
  | A : ('a -> 'b, 'r, 'q) spine * ('a, 'q) app -> ('b, 'r, 'q) spine
  | R : ('r -> 'b, 'r, 'q) spine -> ('b, 'r, 'q) spine
end

type meta = { name : string; index : int; fields : string array }
(** Meta blocks collect extra information about a constructor (or variant).

    Constraints:

    {ul
    {- Identity is meaningful. If [m1] and [m2] represent constructors of the
       same type, and [m1 = m2], then [m1 == m2] must hold. }
    {- Indices of meta blocks are a form of constructor identity and must be
       distinct within a type.}
    {- If the constructor is plain record, its [name] must be [""]. }
    {- If the constructor is a variant, its [fields] must be [[||]]. If it is a
       plain or an inline record, the number fields must match the number of
       layers in the spine.}}

    These constrains may be relied upon by generic functions. *)

type ('a, +'q) view = ('a -> ('a, 'a, 'q) V.spine) * ('a -> meta)
(** [view] is the generic representation for consumer functions.
    It allows {e viewing} a value as a {{V.spine}spine}, and/or as a meta block.


    [view] is equivalent to [gfoldl] from {e SYB}. *)

type ('a, +'q) schema = (('a, 'a, 'q) S.spine * meta) list
(** [schema] is the generic representation for producer functions.
    It encodes the set of constructors of a type.


    [schema] is equivalent to [gunfold] from {e SYB}. *)

(** {1:help Helper functions} *)

(** [spine] and [meta] are [fst] and [snd], named suggestively. *)

val spine : 'a * 'b -> 'a
val meta : 'a * 'b -> 'b

val variant : ?fields:string array -> string -> int -> meta
(** [variant ~fields name index] is a {{!meta}meta block} for the constructor
    named [name], with index [index] within its type.

    If [fields] are provided, the constructor is an inline record. *)

val record : string array -> meta
(** [record fields] is a {{!meta}meta block} for a plain record. *)

val pp_meta : Format.formatter -> meta -> unit
(** [pp_meta] pretty-prints a meta block in a human-readable way. *)

val name : meta -> string
(** [name meta] is the name of this meta block. *)

val fields : meta -> int
(** [fields meta] is the number of fields of this meta block. *)

val field : meta -> int -> string
(** [field meta i] is the [i]-th field of this meta block.

    @raise Invalid_argument when [meta] does not contain [i]-th field. *)

val has_field : meta -> string -> bool
(** Checks if the meta block has the named field. *)

(** {1:data Data} *)

(** [(..., 'q, 'res) needs[n]] is an [n]-ary function from [(_, 'q) app]
    to ['res]. *)

type ('q, 'res) needs0 = 'res
type ('a, 'q, 'res) needs1 =
  ('a, 'q) app -> ('q, 'res) needs0
type ('a, 'b, 'q, 'res) needs2 =
  ('a, 'q) app -> ('b, 'q, 'res) needs1
type ('a, 'b, 'c, 'q, 'res) needs3 =
  ('a, 'q) app -> ('b, 'c, 'q, 'res) needs2
type ('a, 'b, 'c, 'd, 'q, 'res) needs4 =
  ('a, 'q) app -> ('b, 'c, 'd, 'q, 'res) needs3
type ('a, 'b, 'c, 'd, 'e, 'q, 'res) needs5 =
  ('a, 'q) app -> ('b, 'c, 'd, 'e, 'q, 'res) needs4
type ('a, 'b, 'c, 'd, 'e, 'f, 'q, 'res) needs6 =
  ('a, 'q) app -> ('b, 'c, 'd, 'e, 'f, 'q, 'res) needs5
type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'q, 'res) needs7 =
  ('a, 'q) app -> ('b, 'c, 'd, 'e, 'f, 'g, 'q, 'res) needs6
type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'q, 'res) needs8 =
  ('a, 'q) app -> ('b, 'c, 'd, 'e, 'f, 'g, 'h, 'q, 'res) needs7
type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'q, 'res) needs9 =
  ('a, 'q) app -> ('b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'q, 'res) needs8

(** [data[n]] packages up the {!view] and {!schema} for a single type which
    contains [n] other types. It is equivalent to [Data] in {e SYB}.

    This is the easiest {e generic representation} to handle, but it is not
    necessary.

    Types supporting a generic representation are expected to export [data[n]]
    values.

    Generic functions are expected to export an interface for [data[n]],
    together with a naked function that operates directly on a {!view} or
    {!schema}. *)

type 'x data0 =
  { view   : 'q. ('x, 'q) view
  ; schema : 'q. ('x, 'q) schema }
type ('a, 'x) data1 =
  { view   : 'q. ('a, 'q, ('x, 'q) view) needs1
  ; schema : 'q. ('a, 'q, ('x, 'q) schema) needs1 }
type ('a, 'b, 'x) data2 =
  { view   : 'q. ('a, 'b, 'q, ('x, 'q) view) needs2
  ; schema : 'q. ('a, 'b, 'q, ('x, 'q) schema) needs2 }
type ('a, 'b, 'c, 'x) data3 =
  { view   : 'q. ('a, 'b, 'c, 'q, ('x, 'q) view) needs3
  ; schema : 'q. ('a, 'b, 'c, 'q, ('x, 'q) schema) needs3 }
type ('a, 'b, 'c, 'd, 'x) data4 =
  { view   : 'q. ('a, 'b, 'c, 'd, 'q, ('x, 'q) view) needs4
  ; schema : 'q. ('a, 'b, 'c, 'd, 'q, ('x, 'q) schema) needs4 }
type ('a, 'b, 'c, 'd, 'e, 'x) data5 =
  { view   : 'q. ('a, 'b, 'c, 'd, 'e, 'q, ('x, 'q) view) needs5
  ; schema : 'q. ('a, 'b, 'c, 'd, 'e, 'q, ('x, 'q) schema) needs5 }
type ('a, 'b, 'c, 'd, 'e, 'f, 'x) data6 =
  { view   : 'q. ('a, 'b, 'c, 'd, 'e, 'f, 'q, ('x, 'q) view) needs6
  ; schema : 'q. ('a, 'b, 'c, 'd, 'e, 'f, 'q, ('x, 'q) schema) needs6 }
type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'x) data7 =
  { view   : 'q. ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'q, ('x, 'q) view) needs7
  ; schema : 'q. ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'q, ('x, 'q) schema) needs7 }
type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'x) data8 =
  { view   : 'q. ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'q, ('x, 'q) view) needs8
  ; schema : 'q. ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'q, ('x, 'q) schema) needs8 }
type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'x) data9 =
  { view   : 'q. ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'q, ('x, 'q) view) needs9
  ; schema : 'q. ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'q, ('x, 'q) schema) needs9 }

(** Signature for a bundle of generic functions. *)
module type S = sig

  type 'a q
  (** Generic function's query type. *)

  type 'a r
  (** Generic function's result type.

      {b Note.} [q] and [r] are not constrained to be the same, even if they
      often are. *)

  (** The functions [data[n]] are variants of the same generic function,
      operating on the corresponding generic representation types. *)

  val data0 : 'x data0 -> 'x r
  val data1 : ('a, 'x) data1 -> 'a q -> 'x r
  val data2 : ('a, 'b, 'x) data2 -> 'a q -> 'b q -> 'x r
  val data3 : ('a, 'b, 'c, 'x) data3 -> 'a q -> 'b q -> 'c q -> 'x r
  val data4 : ('a, 'b, 'c, 'd, 'x) data4 -> 'a q -> 'b q -> 'c q -> 'd q -> 'x r
  val data5 : ('a, 'b, 'c, 'd, 'e, 'x) data5 -> 'a q -> 'b q -> 'c q -> 'd q ->
              'e q -> 'x r
  val data6 : ('a, 'b, 'c, 'd, 'e, 'f, 'x) data6 -> 'a q -> 'b q -> 'c q ->
              'd q -> 'e q -> 'f q -> 'x r
  val data7 : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'x) data7 -> 'a q -> 'b q -> 'c q ->
              'd q -> 'e q -> 'f q -> 'g q -> 'x r
  val data8 : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'x) data8 -> 'a q -> 'b q ->
              'c q -> 'd q -> 'e q -> 'f q -> 'g q -> 'h q -> 'x r
  val data9 : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'x) data9 -> 'a q -> 'b q ->
              'c q -> 'd q -> 'e q -> 'f q -> 'g q -> 'h q -> 'i q -> 'x r
end

(** Interface between the outside world and a [spine].

    There are only three critical symbols needed by users and/or implementors of
    generic functions:

    {ul
    {- Users need [p] and [:!], to inject from ['a Q.q] to [('a, p) app].}
    {- Implementors need [p] and [!], to project from [('a, p) app] to
       ['a Q.q]. The rest of this module is provided for the implementor's
       convenience.}}

    Minimal complete interface to a generic function consists of [p], [:!], and
    either a function of type {[val f: ('a, p) view -> ... -> 'a -> ...]}
    or {[val g: ('a, p) schema -> ...]}.

    A more complete interface adds a family of functions like

{[val f0 : 'x data0 -> ...
val f1 : ('a, 'x) data1 -> 'a Q.q -> ...
val f2 : ('a, 'b, 'x) data2 -> 'a Q.q -> 'b Q.q -> ...
...
]}

    There are two exporting strategies:

    {ul
    {- Use the {!View} or {!Schema} functors, which have a pre-canned module
       type, and accept the function naming; or}
    {- Manually construct these entry points, perhaps by using the functions
       [data[n]] provided below, and spell out their types in the signature.}}
*)
module Generic (Q: sig type 'a q end) : sig

  type 'a q = 'a Q.q

  type p
  (** Proxy representing [Q.q].

      [p]s exist only to round-trip ['a Q.q] through [spine].

      The only possible operations involving [p] are the two below. *)

  external (!:) : 'a q -> ('a, p) app = "%identity"
  (** [!:x] injects into the proxy. *)

  external (!) : ('a, p) app -> 'a q = "%identity"
  (** [!x] projects from the proxy. *)

  val data0 : ('g -> 'r) -> (p, 'g) needs0 ->
              'r
  val data1 : ('g -> 'r) -> ('a, p, 'g) needs1 ->
              'a q -> 'r
  val data2 : ('g -> 'r) -> ('a, 'b, p, 'g) needs2 ->
              'a q -> 'b q -> 'r
  val data3 : ('g -> 'r) -> ('a, 'b, 'c, p, 'g) needs3 ->
              'a q -> 'b q -> 'c q -> 'r
  val data4 : ('g -> 'r) -> ('a, 'b, 'c, 'd, p, 'g) needs4 ->
              'a q -> 'b q -> 'c q -> 'd q -> 'r
  val data5 : ('g -> 'r) -> ('a, 'b, 'c, 'd, 'e, p, 'g) needs5 ->
              'a q -> 'b q -> 'c q -> 'd q -> 'e q -> 'r
  val data6 : ('g -> 'r) -> ('a, 'b, 'c, 'd, 'e, 'f, p, 'g) needs6 ->
              'a q -> 'b q -> 'c q -> 'd q -> 'e q -> 'f q -> 'r
  val data7 : ('g -> 'r) -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, p, 'g) needs7 ->
              'a q -> 'b q -> 'c q -> 'd q -> 'e q -> 'f q -> 'g q -> 'r
  val data8 : ('g -> 'r) -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, p, 'g) needs8 ->
              'a q -> 'b q -> 'c q -> 'd q -> 'e q -> 'f q -> 'g q -> 'h q -> 'r
  val data9 : ('g -> 'r) -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, p, 'g) needs9 ->
              'a q -> 'b q -> 'c q -> 'd q -> 'e q -> 'f q -> 'g q -> 'h q -> 'i q -> 'r

  (** Functors for uniformly exporting [data[n]] interface.

      {b Note.} They {e do not include} types [q] and [r] from {!S}; to describe
      their output with [S] in a signature, you must eliminate these types. *)

  module View (F: sig
    type 'a r
    val gfun: ('a, p) view -> 'a r
  end) : S with type 'a q := 'a Q.q and type 'a r := 'a F.r

  module Schema (F: sig
    type 'a r
    val gfun: ('a, p) schema -> 'a r
  end) : S with type 'a q := 'a Q.q and type 'a r := 'a F.r
end
