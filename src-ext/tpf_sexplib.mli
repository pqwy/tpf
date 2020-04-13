(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(** Generic Sexplib converters.

    Generic {{!Enc}injections} / {{!Dec}projections} between a type and
    {{: https://github.com/janestreet/sexplib0}Sexplib}'s {! Sexplib0.Sexp.t}.

    Types are mapped to S-expressions in Sexplib-compatible way. *)

open Tpf
open Sexplib0

(** {1 Encoders} *)

type 'a e = 'a -> Sexp.t
(** Encoder type. *)

(** Generic Sexp encoder. *)
module Enc : sig
  include P with type 'a q := 'a e
  val g_to_sexp : ('a, p) view -> 'a e
  include Data with type 'a q := 'a e and type 'a r := 'a e
end

(** {1 Decoders} *)

type 'a d = Sexp.t -> 'a
(** Decoder type. *)

(** Generic Sexp decoder. *)
module Dec : sig
  include P with type 'a q := 'a d
  val g_of_sexp : ('a, p) schema -> 'a d
  include Data with type 'a q := 'a d and type 'a r := 'a d
end

(** {1 Sexp as generic objects} *)

val data_sexp: (string, Sexp.t list, Sexp.t) data2
(** Generic representation of {! Sexplib0.Sexp.t} itself. *)

