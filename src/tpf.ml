(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

type (+'a, +'f) app

type meta = { name : string; index : int; fields : string list }

module V = struct
  type (+_, _, +_) spine =
  | K : 'a * meta -> ('a, 'x, 'res) spine
  | A : ('a -> 'b, 'x, 'res) spine * 'a * ('a, 'res) app -> ('b, 'x, 'res) spine
  | R : ('x -> 'b, 'x, 'res) spine * 'x -> ('b, 'x, 'res) spine
end
module S = struct
  type (+_, _, +_) spine =
  | K : 'a * meta -> ('a, 'x, 'res) spine
  | A : ('a -> 'b, 'x, 'res) spine * ('a, 'res) app -> ('b, 'x, 'res) spine
  | R : ('x -> 'b, 'x, 'res) spine -> ('b, 'x, 'res) spine
end

type ('a, +'res) view = 'a -> ('a, 'a, 'res) V.spine
type ('a, +'res) schema = ('a, 'a, 'res) S.spine list

let variant ?(fields = []) name index = { name; index; fields }
let record fields = { name = ""; index = 0; fields }

let rec meta: type a b c. (a, b, c) V.spine -> _ = V.(function
| K (_, c) -> c
| A (s, _, _) -> meta s
| R (s, _) -> meta s)

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

module Generic (R: sig type 'a r end) = struct

  type p
  type 'a r = 'a R.r

  external w   : 'a r -> ('a, p) app = "%identity"
  external (!!) : ('a, p) app -> 'a r = "%identity"

  let ($) f x = f (w x)

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

end
