(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(** Generic JSON converters.

    Generic {{!Enc}injections} / {{!Dec}projections} between a type and a
    general purpose {{!json}JSON tree}.

    The tree type is non-generative, so it works with any underlying library for
    (de-)serialization. Howerver, the particular tree shape was chosen for
    compatibility with {{:https://github.com/ocaml-community/yojson}Joyson}.  *)

open Tpf

type json =
  [ `Null | `Bool of bool | `Int of int | `Float of float| `String of string
  | `List of json list | `Assoc of (string * json) list ]
(** JSON trees.

    {e Note.} This is {! Yojson.Basic.t}. Hence the rather arbitrary
    [Int] / [Float] split. *)

type error = [`Msg of string]
(** Decoder error. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf e] pretty-prints [e] on [ppf]. *)

type 'a e = 'a -> json
(** JSON encoders. *)

type 'a d = json -> ('a, error) result
(** JSON decoders. *)

(** Generic JSON encoder.

    Records are represented as JSON objects, and constant constructors are
    represented as JSON strings.

    The representation of variants is determined by the {{!sum}[~sum]} argument
    (default {!arr}). *)
module Enc : sig

  include P with type 'a q := 'a e

  type sum = string -> json list -> json
  (** Sum-formatting functions. [f name args] is the JSON encoding of
      constructor named [name], with arguments [args]. *)

  val arr : sum
  (** Formats [K (a0, a1, ...)] as [["K", a0, a1, ...]]. *)

  val obj : ?tag:string -> ?values:string -> sum
  (** Formats [K (a0, a1, ...)] as [{ tag: "K", values: [a0, a1, ...]}].

      [tag] defaults to ["tag"] and [values] defaults to ["values"]. *)

  val g_to_json : ?sum:sum -> ('a, p) view -> 'a e

  val data0 : ?sum:sum -> 'x data0 ->
              'x e
  val data1 : ?sum:sum -> ('a, 'x) data1 ->
              'a e -> 'x e
  val data2 : ?sum:sum -> ('a, 'b, 'x) data2 ->
              'a e -> 'b e -> 'x e
  val data3 : ?sum:sum -> ('a, 'b, 'c, 'x) data3 ->
              'a e -> 'b e -> 'c e -> 'x e
  val data4 : ?sum:sum -> ('a, 'b, 'c, 'd, 'x) data4 ->
              'a e -> 'b e -> 'c e -> 'd e -> 'x e
  val data5 : ?sum:sum -> ('a, 'b, 'c, 'd, 'e, 'x) data5 ->
              'a e -> 'b e -> 'c e -> 'd e -> 'e e -> 'x e
  val data6 : ?sum:sum -> ('a, 'b, 'c, 'd, 'e, 'f, 'x) data6 ->
              'a e -> 'b e -> 'c e -> 'd e -> 'e e -> 'f e -> 'x e
  val data7 : ?sum:sum -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'x) data7 ->
              'a e -> 'b e -> 'c e -> 'd e -> 'e e -> 'f e -> 'g e -> 'x e
  val data8 : ?sum:sum -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'x) data8 ->
              'a e -> 'b e -> 'c e -> 'd e -> 'e e -> 'f e -> 'g e -> 'h e -> 'x e
  val data9 : ?sum:sum -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'x) data9 ->
              'a e -> 'b e -> 'c e -> 'd e -> 'e e -> 'f e -> 'g e -> 'h e -> 'i e -> 'x e
end

(** Generic JSON decoder.

    Records are expected as JSON objects, and constant constructors are expected
    as JSON strings.

    The representation of variants is determined by the {{!sum}[~sum]} argument
    (default {!arr}). *)
module Dec: sig

  include P with type 'a q := 'a d

  type sum = (string * json list) d
  (** Sum-destructuring functions. [f json] is [Ok (name, args)] when [json]
      encodes a constructor named [name] with arguments [args], or [Error _]
      otherwise. *)

  val arr : sum
  (** Destructures [K (a0, a1, ...)] from [["K", a0, a1, ...]]. *)

  val obj : ?tag:string -> ?values:string -> sum
  (** Destructures [K (a0, a1, ...)] from [{ tag: "K", values: [a0, a1, ...]}].

      [tag] defaults to ["tag"] and [values] defaults to ["values"]. *)


  val g_of_json : ?sum:sum -> ('a, p) schema -> 'a d

  val data0 : ?sum:sum -> 'x data0 ->
              'x d
  val data1 : ?sum:sum -> ('a, 'x) data1 ->
              'a d -> 'x d
  val data2 : ?sum:sum -> ('a, 'b, 'x) data2 ->
              'a d -> 'b d -> 'x d
  val data3 : ?sum:sum -> ('a, 'b, 'c, 'x) data3 ->
              'a d -> 'b d -> 'c d -> 'x d
  val data4 : ?sum:sum -> ('a, 'b, 'c, 'd, 'x) data4 ->
              'a d -> 'b d -> 'c d -> 'd d -> 'x d
  val data5 : ?sum:sum -> ('a, 'b, 'c, 'd, 'e, 'x) data5 ->
              'a d -> 'b d -> 'c d -> 'd d -> 'e d -> 'x d
  val data6 : ?sum:sum -> ('a, 'b, 'c, 'd, 'e, 'f, 'x) data6 ->
              'a d -> 'b d -> 'c d -> 'd d -> 'e d -> 'f d -> 'x d
  val data7 : ?sum:sum -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'x) data7 ->
              'a d -> 'b d -> 'c d -> 'd d -> 'e d -> 'f d -> 'g d -> 'x d
  val data8 : ?sum:sum -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'x) data8 ->
              'a d -> 'b d -> 'c d -> 'd d -> 'e d -> 'f d -> 'g d -> 'h d -> 'x d
  val data9 : ?sum:sum -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'x) data9 ->
              'a d -> 'b d -> 'c d -> 'd d -> 'e d -> 'f d -> 'g d -> 'h d -> 'i d -> 'x d
end
