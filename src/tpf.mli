(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(** [Tpf] — Tagless/Trivial polytypic functions *)

(** {1:over Overview:}

{ul
{- {!V}, {!S} — View and Schema spines.}
{- {!view}, {!schema} and {!meta}.}
{- {!data0} .. {!data9} — package up [view * schema].}
{- {!P}, {!Data} — signatures to ease exporting generic functions.}
{- {!Generic} —  start here when writing a generic function.}
{- {{!metaf}Metablock helpers}.}}
*)

(** {1:core The Core} *)

type (+'a, +'f) app

(* A {e spine} is a typed sequence of one-step constructor applications.
   Two types of spines, {{!V}view} and {{!S}schema}, are at the heart of
   generic representation. *)

(** [V] is for {!view}.

    An instantiated {e view spine} encodes a value and its pointwise queries.

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

(** [S] is for {!schema}.

    An instantiated {e schema spine} encodes a type constructor and its
    pointwise queries.

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

type meta
(** Meta blocks collect extra information about a constructor. *)

type ('a, +'q) view = ('a -> ('a, 'a, 'q) V.spine) * ('a -> meta)
(** Generic representation for consumer functions.
    It can {e view} a value as a {!V.spine} or {!meta}.

    [view] is equivalent to [gfoldl] in {e SYB}. *)

type ('a, +'q) schema = (('a, 'a, 'q) S.spine * meta) list
(** Generic representation for producer functions.
    It encodes the set of constructors of a type.


    [schema] is equivalent to [gunfold] in {e SYB}. *)

val spine : 'a * 'b -> 'a
(** [spine] is [fst]. Spine is always first. *)

val meta : 'a * 'b -> 'b
(** [meta] is [snd]. Meta block is always second. *)

(** {1:data Data} *)

(** [(..., 'q, 'res) app[n]] is just an alias for an [n]-ary function
    from [(_, 'q) app] to ['res]. *)

type ('q, 'res) app0 = 'res
type ('a, 'q, 'res) app1 =
  ('a, 'q) app -> ('q, 'res) app0
type ('a, 'b, 'q, 'res) app2 =
  ('a, 'q) app -> ('b, 'q, 'res) app1
type ('a, 'b, 'c, 'q, 'res) app3 =
  ('a, 'q) app -> ('b, 'c, 'q, 'res) app2
type ('a, 'b, 'c, 'd, 'q, 'res) app4 =
  ('a, 'q) app -> ('b, 'c, 'd, 'q, 'res) app3
type ('a, 'b, 'c, 'd, 'e, 'q, 'res) app5 =
  ('a, 'q) app -> ('b, 'c, 'd, 'e, 'q, 'res) app4
type ('a, 'b, 'c, 'd, 'e, 'f, 'q, 'res) app6 =
  ('a, 'q) app -> ('b, 'c, 'd, 'e, 'f, 'q, 'res) app5
type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'q, 'res) app7 =
  ('a, 'q) app -> ('b, 'c, 'd, 'e, 'f, 'g, 'q, 'res) app6
type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'q, 'res) app8 =
  ('a, 'q) app -> ('b, 'c, 'd, 'e, 'f, 'g, 'h, 'q, 'res) app7
type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'q, 'res) app9 =
  ('a, 'q) app -> ('b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'q, 'res) app8

(** [data] packages up the {!view} and {!schema} for a single type which
    contains [n] other types. This is the easiest generic representation to
    handle, but it is not necessary.

    Generically representable types should export their [data] value.

    Generic functions should export a [data]-based interface, together with a
    naked function that operates directly on a {!view} or {!schema}.

    [data] is equivalent to [Data] in {e SYB}. *)

type 'x data0 =
  { view   : 'q. ('x, 'q) view
  ; schema : 'q. ('x, 'q) schema }
type ('a, 'x) data1 =
  { view   : 'q. ('a, 'q, ('x, 'q) view) app1
  ; schema : 'q. ('a, 'q, ('x, 'q) schema) app1 }
type ('a, 'b, 'x) data2 =
  { view   : 'q. ('a, 'b, 'q, ('x, 'q) view) app2
  ; schema : 'q. ('a, 'b, 'q, ('x, 'q) schema) app2 }
type ('a, 'b, 'c, 'x) data3 =
  { view   : 'q. ('a, 'b, 'c, 'q, ('x, 'q) view) app3
  ; schema : 'q. ('a, 'b, 'c, 'q, ('x, 'q) schema) app3 }
type ('a, 'b, 'c, 'd, 'x) data4 =
  { view   : 'q. ('a, 'b, 'c, 'd, 'q, ('x, 'q) view) app4
  ; schema : 'q. ('a, 'b, 'c, 'd, 'q, ('x, 'q) schema) app4 }
type ('a, 'b, 'c, 'd, 'e, 'x) data5 =
  { view   : 'q. ('a, 'b, 'c, 'd, 'e, 'q, ('x, 'q) view) app5
  ; schema : 'q. ('a, 'b, 'c, 'd, 'e, 'q, ('x, 'q) schema) app5 }
type ('a, 'b, 'c, 'd, 'e, 'f, 'x) data6 =
  { view   : 'q. ('a, 'b, 'c, 'd, 'e, 'f, 'q, ('x, 'q) view) app6
  ; schema : 'q. ('a, 'b, 'c, 'd, 'e, 'f, 'q, ('x, 'q) schema) app6 }
type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'x) data7 =
  { view   : 'q. ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'q, ('x, 'q) view) app7
  ; schema : 'q. ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'q, ('x, 'q) schema) app7 }
type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'x) data8 =
  { view   : 'q. ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'q, ('x, 'q) view) app8
  ; schema : 'q. ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'q, ('x, 'q) schema) app8 }
type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'x) data9 =
  { view   : 'q. ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'q, ('x, 'q) view) app9
  ; schema : 'q. ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'q, ('x, 'q) schema) app9 }

(** {1 Writing generic functions} *)

(** Packages up the basic user-facing part of a {!Generic} instance.

    As access to [p] and the injection are necessary to use a generic function,
    this signature gets exported a lot. *)
module type P = sig
  type 'a q
  (** Query type. *)

  type p
  (** [q] proxy. *)

  val (!) : 'a q -> ('a, p) app
  (** Injection. *)
end

(** Packages up {{!data0}[data]}-based entry points to a generic function.

    [q] is the {e query} that we need at each type that we encounter.

    [r] is the function's result.

    [q] and [r] are not constrained to be the same, but they often are. *)
module type Data = sig

  type 'a q
  (** What we need at each type. *)

  type 'a r
  (** What we provide, in return. *)

  (** The functions [data[n]] are variants of the same generic function,
      operating on the corresponding [data] types. *)

  val data0 : 'x data0 ->
              'x r
  val data1 : ('a, 'x) data1 ->
              'a q -> 'x r
  val data2 : ('a, 'b, 'x) data2 ->
              'a q -> 'b q -> 'x r
  val data3 : ('a, 'b, 'c, 'x) data3 ->
              'a q -> 'b q -> 'c q -> 'x r
  val data4 : ('a, 'b, 'c, 'd, 'x) data4 ->
              'a q -> 'b q -> 'c q -> 'd q -> 'x r
  val data5 : ('a, 'b, 'c, 'd, 'e, 'x) data5 ->
              'a q -> 'b q -> 'c q -> 'd q -> 'e q -> 'x r
  val data6 : ('a, 'b, 'c, 'd, 'e, 'f, 'x) data6 ->
              'a q -> 'b q -> 'c q -> 'd q -> 'e q -> 'f q -> 'x r
  val data7 : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'x) data7 ->
              'a q -> 'b q -> 'c q -> 'd q -> 'e q -> 'f q -> 'g q -> 'x r
  val data8 : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'x) data8 ->
              'a q -> 'b q -> 'c q -> 'd q -> 'e q -> 'f q -> 'g q -> 'h q ->
              'x r
  val data9 : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'x) data9 ->
              'a q -> 'b q -> 'c q -> 'd q -> 'e q -> 'f q -> 'g q -> 'h q ->
              'i q -> 'x r
end

(** Interface between the outside world and a [spine].

    It contains only three necessary symbols:

    {ul
    {- {e users} need [p] and [!], to inject from ['a Q.q] to [('a, p) app];
       while}
    {- {e implementors} need [p] and [!:], to project from [('a, p) app] to
       ['a Q.q].}}

    The rest of this module is provided for the implementor's convenience.

    Minimal complete interface to a generic function consists of [p], [!], and
    a function that looks like one of
{[val f: ('a, p) view -> ... -> 'a -> ...
val g: ('a, p) schema -> ...]}

    A more complete interface adds a family of functions like

{[val f0 : 'x data0 -> ...
val f1 : ('a, 'x) data1 -> 'a Q.q -> ...
val f2 : ('a, 'b, 'x) data2 -> 'a Q.q -> 'b Q.q -> ...
...
]}

    These can be

    {ul
    {- produced with the {!View} and {!Schema} functors, which have pre-canned
       module types, but fixed names; or}
    {- constructed manually, perhaps by using the functions {{!data0}[data[n]]},
       with their signature spelled out by hand.}}
*)
module Generic (Q: sig type 'a q end) : sig

  type 'a q = 'a Q.q
  (** Query type for this (group of) function(s). It gives the action to be done
      for each constructor argument. *)

  type p
  (** Proxy representing [Q.q].

      [p]s exists only to embed ['a Q.q] in a [spine].

      The only possible operations involving [p] are the two below. *)

  external (!) : 'a q -> ('a, p) app = "%identity"
  (** [!x] injects into the proxy. *)

  external (!:) : ('a, p) app -> 'a q = "%identity"
  (** [!:x] projects from the proxy. *)

  module P: P with type p = p and type 'a q := 'a Q.q
  (** Groups {!p} and {!(!)}, above, for easy export. *)

  (** Functors generating a [data[n]] interface.

      {b Note.} They {e do not include} types [q] and [r] from {!Data}; when
      describing their output type in signatures using {!Data}, you must eliminate
      [q] and [r]. *)

  (** [View] equips a generic consumer [gfun] with the
      {{!Tpf.data0}[data[n]]} interface, for easy export. *)
  module View (F: sig
    type 'a r
    val gfun: ('a, p) view -> 'a r
  end) : Data with type 'a q := 'a Q.q and type 'a r := 'a F.r

  (** [Schema] equips a generic producer [gfun] the the
      {{!Tpf.data0}[data[n]]} interface, for easy export. *)
  module Schema (F: sig
    type 'a r
    val gfun: ('a, p) schema -> 'a r
  end) : Data with type 'a q := 'a Q.q and type 'a r := 'a F.r

  (** Helpers for manually exporting generic functions.

      [app[n] k f] converts [f: ('a, p) app -> ...] into {{!q}['a q -> ...]} and
      applies [k] to it.

      For instance, {!View} is given by
{[let data0 (d: _ data0) = app0 gfun d.view
let data1 (d: _ data1) = app1 gfun d.view
...
]}
   *)

  val app0 : ('cont -> 'res) -> (p, 'cont) app0 ->
             'res
  val app1 : ('cont -> 'res) -> ('a, p, 'cont) app1 ->
             'a q -> 'res
  val app2 : ('cont -> 'res) -> ('a, 'b, p, 'cont) app2 ->
             'a q -> 'b q -> 'res
  val app3 : ('cont -> 'res) -> ('a, 'b, 'c, p, 'cont) app3 ->
             'a q -> 'b q -> 'c q -> 'res
  val app4 : ('cont -> 'res) -> ('a, 'b, 'c, 'd, p, 'cont) app4 ->
             'a q -> 'b q -> 'c q -> 'd q -> 'res
  val app5 : ('cont -> 'res) -> ('a, 'b, 'c, 'd, 'e, p, 'cont) app5 ->
             'a q -> 'b q -> 'c q -> 'd q -> 'e q -> 'res
  val app6 : ('cont -> 'res) -> ('a, 'b, 'c, 'd, 'e, 'f, p, 'cont) app6 ->
             'a q -> 'b q -> 'c q -> 'd q -> 'e q -> 'f q -> 'res
  val app7 : ('cont -> 'res) -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, p, 'cont) app7 ->
             'a q -> 'b q -> 'c q -> 'd q -> 'e q -> 'f q -> 'g q -> 'res
  val app8 : ('cont -> 'res) -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, p, 'cont) app8 ->
             'a q -> 'b q -> 'c q -> 'd q -> 'e q -> 'f q -> 'g q -> 'h q -> 'res
  val app9 : ('cont -> 'res) -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, p, 'cont) app9 ->
             'a q -> 'b q -> 'c q -> 'd q -> 'e q -> 'f q -> 'g q -> 'h q -> 'i q -> 'res
end

(** {1:metaf Manipulating [meta]} *)

val variant : ?labels:string array -> int -> string -> meta
(** [variant ~labels index name] is a variant, a sum type component.

    [index] is this constructor position in the type definition, and must be
    unique within the type. [name] is the constructor name, and must not be
    [""]. If [labels] are specified, the constructor is an inline record. *)

val record : string array -> meta
(** [record labels] is a record. It must be the only constructor in the type. *)

val name : meta -> string
(** [name meta] is constructor name. If [meta] is a record, its name is [""]. *)

val index : meta -> int
(** [index meta] is constructor position in the type definition. *)

val labels : meta -> int
(** [labels meta] is the number of constructor labels. *)

val label : meta -> int -> string
(** [label meta i] is [i]-th constructor label.

    @raise Invalid_argument then the constructor does not have [i]-th label. *)

val has_label : meta -> string -> bool
(** [has_label m name] is [true] iff the constructor has a label [name]. *)

val pp_meta : Format.formatter -> meta -> unit
(** [pp_meta] pretty-prints a meta block in a human-readable way. *)
