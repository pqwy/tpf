(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(** Generic Sexplib converters. *)

open Tpf
open Sexplib0

val sexp: (string, Sexp.t list, Sexp.t) data2
(** Representation of [Sexp.t] itself. *)

(** Generic [to_sexp]. *)
module To : sig
  type 'a e = 'a -> Sexp.t
  include P with type 'a q := 'a e
  include Data with type 'a q := 'a e and type 'a r := 'a e
  val g_to_sexp : ('a, p) view -> 'a e
end

(** Generic [of_sexp]. *)
module Of : sig
  type 'a d = Sexp.t -> 'a
  include P with type 'a q := 'a d
  include Data with type 'a q := 'a d and type 'a r := 'a d
  val g_of_sexp : ('a, p) schema -> 'a d
end
