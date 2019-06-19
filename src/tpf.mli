(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

type (+'a, +'f) app

type meta = { name : string; index : int; fields : string list }

module V : sig
  type (+_, _, +_) spine =
  | K : 'a * meta -> ('a, 'x, 'res) spine
  | A : ('a -> 'b, 'x, 'res) spine * 'a * ('a, 'res) app -> ('b, 'x, 'res) spine
  | R : ('x -> 'b, 'x, 'res) spine * 'x -> ('b, 'x, 'res) spine
end
module S : sig
  type (+_, _, +_) spine =
  | K : 'a * meta -> ('a, 'x, 'res) spine
  | A : ('a -> 'b, 'x, 'res) spine * ('a, 'res) app -> ('b, 'x, 'res) spine
  | R : ('x -> 'b, 'x, 'res) spine -> ('b, 'x, 'res) spine
end

type ('a, +'res) view = 'a -> ('a, 'a, 'res) V.spine
type ('a, +'res) schema = ('a, 'a, 'res) S.spine list

val variant : ?fields : string list -> string -> int -> meta
val record : string list -> meta
val meta : ('a, 'b, 'c) V.spine -> meta

type 'x g0 =
  { view   : 'r. ('x, 'r) view
  ; schema : 'r. ('x, 'r) schema }

type ('a, 'x) g1 =
  { view   : 'r. ('a, 'r) app -> ('x, 'r) view
  ; schema : 'r. ('a, 'r) app -> ('x, 'r) schema }

type ('a, 'b, 'x) g2 =
  { view   : 'r. ('a, 'r) app -> ('b, 'r) app -> ('x, 'r) view
  ; schema : 'r. ('a, 'r) app -> ('b, 'r) app -> ('x, 'r) schema }

type ('a, 'b, 'c, 'x) g3 =
  { view   : 'r. ('a, 'r) app -> ('b, 'r) app -> ('c, 'r) app -> ('x, 'r) view
  ; schema : 'r. ('a, 'r) app -> ('b, 'r) app -> ('c, 'r) app -> ('x, 'r) schema }

type ('a, 'b, 'c, 'd, 'x) g4 =
  { view   : 'r. ('a, 'r) app -> ('b, 'r) app -> ('c, 'r) app -> ('d, 'r) app ->
                 ('x, 'r) view
  ; schema : 'r. ('a, 'r) app -> ('b, 'r) app -> ('c, 'r) app -> ('d, 'r) app ->
                 ('x, 'r) schema }

type ('a, 'b, 'c, 'd, 'e, 'x) g5 =
  { view   : 'r. ('a, 'r) app -> ('b, 'r) app -> ('c, 'r) app -> ('d, 'r) app ->
                 ('e, 'r) app -> ('x, 'r) view
  ; schema : 'r. ('a, 'r) app -> ('b, 'r) app -> ('c, 'r) app -> ('d, 'r) app ->
                 ('e, 'r) app -> ('x, 'r) schema }

type ('a, 'b, 'c, 'd, 'e, 'f, 'x) g6 =
  { view   : 'r. ('a, 'r) app -> ('b, 'r) app -> ('c, 'r) app -> ('d, 'r) app ->
                 ('e, 'r) app -> ('f, 'r) app -> ('x, 'r) view
  ; schema : 'r. ('a, 'r) app -> ('b, 'r) app -> ('c, 'r) app -> ('d, 'r) app ->
                 ('e, 'r) app -> ('f, 'r) app -> ('x, 'r) schema }

type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'x) g7 =
  { view   : 'r. ('a, 'r) app -> ('b, 'r) app -> ('c, 'r) app -> ('d, 'r) app ->
                 ('e, 'r) app -> ('f, 'r) app -> ('g, 'r) app -> ('x, 'r) view
  ; schema : 'r. ('a, 'r) app -> ('b, 'r) app -> ('c, 'r) app -> ('d, 'r) app ->
                 ('e, 'r) app -> ('f, 'r) app -> ('g, 'r) app -> ('x, 'r) schema }

type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'x) g8 =
  { view   : 'r. ('a, 'r) app -> ('b, 'r) app -> ('c, 'r) app -> ('d, 'r) app ->
                 ('e, 'r) app -> ('f, 'r) app -> ('g, 'r) app -> ('h, 'r) app ->
                 ('x, 'r) view
  ; schema : 'r. ('a, 'r) app -> ('b, 'r) app -> ('c, 'r) app -> ('d, 'r) app ->
                 ('e, 'r) app -> ('f, 'r) app -> ('g, 'r) app -> ('h, 'r) app ->
                 ('x, 'r) schema }

type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'x) g9 =
  { view   : 'r. ('a, 'r) app -> ('b, 'r) app -> ('c, 'r) app -> ('d, 'r) app ->
                 ('e, 'r) app -> ('f, 'r) app -> ('g, 'r) app -> ('h, 'r) app ->
                 ('i, 'r) app -> ('x, 'r) view
  ; schema : 'r. ('a, 'r) app -> ('b, 'r) app -> ('c, 'r) app -> ('d, 'r) app ->
                 ('e, 'r) app -> ('f, 'r) app -> ('g, 'r) app -> ('h, 'r) app ->
                 ('i, 'r) app -> ('x, 'r) schema }


module Generic (R: sig type 'a r end) : sig

  type 'a r
  type p

  external w : 'a r -> ('a, p) app = "%identity"
  external (!!) : ('a, p) app -> 'a r = "%identity"
  val ($) : (('a, p) app -> 'b) -> 'a r -> 'b [@@inline]

  val v0 : (('x, p) view -> 'y) -> 'x g0 -> 'y
  val v1 : (('x, p) view -> 'y) -> ('a, 'x) g1 -> 'a r -> 'y
  val v2 : (('x, p) view -> 'y) -> ('a, 'b, 'x) g2 -> 'a r -> 'b r -> 'y
  val v3 : (('x, p) view -> 'y) -> ('a, 'b, 'c, 'x) g3 ->
           'a r -> 'b r -> 'c r -> 'y
  val v4 : (('x, p) view -> 'y) -> ('a, 'b, 'c, 'd, 'x) g4 ->
           'a r -> 'b r -> 'c r -> 'd r -> 'y
  val v5 : (('x, p) view -> 'y) -> ('a, 'b, 'c, 'd, 'e, 'x) g5 ->
           'a r -> 'b r -> 'c r -> 'd r -> 'e r -> 'y
  val v6 : (('x, p) view -> 'y) -> ('a, 'b, 'c, 'd, 'e, 'f, 'x) g6 ->
           'a r -> 'b r -> 'c r -> 'd r -> 'e r -> 'f r -> 'y
  val v7 : (('x, p) view -> 'y) -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'x) g7 ->
           'a r -> 'b r -> 'c r -> 'd r -> 'e r -> 'f r -> 'g r -> 'y
  val v8 : (('x, p) view -> 'y) -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'x) g8 ->
           'a r -> 'b r -> 'c r -> 'd r -> 'e r -> 'f r -> 'g r -> 'h r -> 'y
  val v9 : (('x, p) view -> 'y) -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'x) g9 ->
           'a r -> 'b r -> 'c r -> 'd r -> 'e r -> 'f r -> 'g r -> 'h r -> 'i r -> 'y

  val s0 : (('x, p) schema -> 'y) -> 'x g0 -> 'y
  val s1 : (('x, p) schema -> 'y) -> ('a, 'x) g1 -> 'a r -> 'y
  val s2 : (('x, p) schema -> 'y) -> ('a, 'b, 'x) g2 -> 'a r -> 'b r -> 'y
  val s3 : (('x, p) schema -> 'y) -> ('a, 'b, 'c, 'x) g3 ->
           'a r -> 'b r -> 'c r -> 'y
  val s4 : (('x, p) schema -> 'y) -> ('a, 'b, 'c, 'd, 'x) g4 ->
           'a r -> 'b r -> 'c r -> 'd r -> 'y
  val s5 : (('x, p) schema -> 'y) -> ('a, 'b, 'c, 'd, 'e, 'x) g5 ->
           'a r -> 'b r -> 'c r -> 'd r -> 'e r -> 'y
  val s6 : (('x, p) schema -> 'y) -> ('a, 'b, 'c, 'd, 'e, 'f, 'x) g6 ->
           'a r -> 'b r -> 'c r -> 'd r -> 'e r -> 'f r -> 'y
  val s7 : (('x, p) schema -> 'y) -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'x) g7 ->
           'a r -> 'b r -> 'c r -> 'd r -> 'e r -> 'f r -> 'g r -> 'y
  val s8 : (('x, p) schema -> 'y) -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'x) g8 ->
           'a r -> 'b r -> 'c r -> 'd r -> 'e r -> 'f r -> 'g r -> 'h r -> 'y
  val s9 : (('x, p) schema -> 'y) -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'x) g9 ->
           'a r -> 'b r -> 'c r -> 'd r -> 'e r -> 'f r -> 'g r -> 'h r -> 'i r -> 'y

end with type 'a r = 'a R.r
