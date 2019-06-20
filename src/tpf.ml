(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(* Core types. *)

type (+'a, +'f) app

type meta = { name : string; index : int; fields : string list }

module V = struct
  type (+_, _, +_) spine =
  | K : 'a * meta -> ('a, 'x, 'res) spine
  | R : ('x -> 'b, 'x, 'res) spine * 'x -> ('b, 'x, 'res) spine
  | A : ('a -> 'b, 'x, 'res) spine * 'a * ('a, 'res) app -> ('b, 'x, 'res) spine
end
module S = struct
  type (+_, _, +_) spine =
  | K : 'a * meta -> ('a, 'x, 'res) spine
  | R : ('x -> 'b, 'x, 'res) spine -> ('b, 'x, 'res) spine
  | A : ('a -> 'b, 'x, 'res) spine * ('a, 'res) app -> ('b, 'x, 'res) spine
end

type ('a, +'res) view = 'a -> ('a, 'a, 'res) V.spine
type ('a, +'res) schema = ('a, 'a, 'res) S.spine list

(* Metablock stuff. *)

let variant ?(fields = []) name index = { name; index; fields }
let record fields = { name = ""; index = 0; fields }

let rec v_meta: 'a 'b 'c. ('a, 'b, 'c) V.spine -> _ = V.(function
| K (_, m) -> m
| R (s, _) -> v_meta s
| A (s, _, _) -> v_meta s)
let rec s_meta: 'a 'b 'c. ('a, 'b, 'c) S.spine -> _ = S.(function
| K (_, m) -> m
| R s -> s_meta s
| A (s, _) -> s_meta s)
let v_name s = (v_meta s).name
let s_name s = (s_meta s).name
let v_fields s = (v_meta s).fields
let s_fields s = (s_meta s).fields

(* Generic representations of n-point types -- "generics". *)

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

(* Generic function families. *)

module type Gfun = sig
  type 'a r
  val g0 :  'x g0 -> 'x r
  val g1 :  ('a, 'x) g1 -> 'a r -> 'x r
  val g2 :  ('a, 'b, 'x) g2 -> 'a r -> 'b r -> 'x r
  val g3 :  ('a, 'b, 'c, 'x) g3 -> 'a r -> 'b r -> 'c r -> 'x r
  val g4 :  ('a, 'b, 'c, 'd, 'x) g4 -> 'a r -> 'b r -> 'c r -> 'd r -> 'x r
  val g5 :  ('a, 'b, 'c, 'd, 'e, 'x) g5 -> 'a r -> 'b r -> 'c r -> 'd r ->
            'e r -> 'x r
  val g6 :  ('a, 'b, 'c, 'd, 'e, 'f, 'x) g6 -> 'a r -> 'b r -> 'c r -> 'd r ->
            'e r -> 'f r -> 'x r
  val g7 :  ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'x) g7 -> 'a r -> 'b r -> 'c r ->
            'd r -> 'e r -> 'f r -> 'g r -> 'x r
  val g8 :  ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'x) g8 -> 'a r -> 'b r -> 'c r ->
            'd r -> 'e r -> 'f r -> 'g r -> 'h r -> 'x r
  val g9 :  ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'x) g9 -> 'a r -> 'b r ->
            'c r -> 'd r -> 'e r -> 'f r -> 'g r -> 'h r -> 'i r -> 'x r
end

(* [app] instantiation. *)

module Generic (R: sig type 'a r end) = struct

  type 'a r = 'a R.r

  (* The "brand". *)
  type p
  external w   : 'a r -> ('a, p) app = "%identity"
  external (!!) : ('a, p) app -> 'a r = "%identity"
  let ($) f x = f (w x)

  (* Helpers to pick out view/schema and inject n arguments. *)

  let v0 x (y: _ g0)                   = x y.view
  let v1 x (y: _ g1) a                 = x (y.view (w a))
  let v2 x (y: _ g2) a b               = x (y.view (w a) (w b))
  let v3 x (y: _ g3) a b c             = x (y.view (w a) (w b) (w c))
  let v4 x (y: _ g4) a b c d           = x (y.view (w a) (w b) (w c) (w d))
  let v5 x (y: _ g5) a b c d e         = x (y.view (w a) (w b) (w c) (w d) (w e))
  let v6 x (y: _ g6) a b c d e f       = x (y.view (w a) (w b) (w c) (w d) (w e) (w f))
  let v7 x (y: _ g7) a b c d e f g     = x (y.view (w a) (w b) (w c) (w d) (w e) (w f) (w g))
  let v8 x (y: _ g8) a b c d e f g h   = x (y.view (w a) (w b) (w c) (w d) (w e) (w f) (w g) (w h))
  let v9 x (y: _ g9) a b c d e f g h i = x (y.view (w a) (w b) (w c) (w d) (w e) (w f) (w g) (w h) (w i))

  let s0 x (y: _ g0)                   = x y.schema
  let s1 x (y: _ g1) a                 = x (y.schema (w a))
  let s2 x (y: _ g2) a b               = x (y.schema (w a) (w b))
  let s3 x (y: _ g3) a b c             = x (y.schema (w a) (w b) (w c))
  let s4 x (y: _ g4) a b c d           = x (y.schema (w a) (w b) (w c) (w d))
  let s5 x (y: _ g5) a b c d e         = x (y.schema (w a) (w b) (w c) (w d) (w e))
  let s6 x (y: _ g6) a b c d e f       = x (y.schema (w a) (w b) (w c) (w d) (w e) (w f))
  let s7 x (y: _ g7) a b c d e f g     = x (y.schema (w a) (w b) (w c) (w d) (w e) (w f) (w g))
  let s8 x (y: _ g8) a b c d e f g h   = x (y.schema (w a) (w b) (w c) (w d) (w e) (w f) (w g) (w h))
  let s9 x (y: _ g9) a b c d e f g h i = x (y.schema (w a) (w b) (w c) (w d) (w e) (w f) (w g) (w h) (w i))

  (* The same, but module-level. *)

  module View_f (F: sig val f: ('a, p) view -> 'a r end) = struct
    type nonrec 'a r = 'a r
    let g0 g = v0 F.f g
    let g1 g = v1 F.f g
    let g2 g = v2 F.f g
    let g3 g = v3 F.f g
    let g4 g = v4 F.f g
    let g5 g = v5 F.f g
    let g6 g = v6 F.f g
    let g7 g = v7 F.f g
    let g8 g = v8 F.f g
    let g9 g = v9 F.f g
  end

  module Schema_f (F: sig val f: ('a, p) schema -> 'a r end) = struct
    type nonrec 'a r = 'a r
    let g0 g = s0 F.f g
    let g1 g = s1 F.f g
    let g2 g = s2 F.f g
    let g3 g = s3 F.f g
    let g4 g = s4 F.f g
    let g5 g = s5 F.f g
    let g6 g = s6 F.f g
    let g7 g = s7 F.f g
    let g8 g = s8 F.f g
    let g9 g = s9 F.f g
  end
end
