(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(* Core types. *)

type (+'a, +'f) app

module V = struct
  type (+_, _, +_) spine =
  | K : 'a -> ('a, 'x, 'res) spine
  | R : ('x -> 'b, 'x, 'res) spine * 'x -> ('b, 'x, 'res) spine
  | A : ('a -> 'b, 'x, 'res) spine * 'a * ('a, 'res) app -> ('b, 'x, 'res) spine
end

module S = struct
  type (+_, _, +_) spine =
  | K : 'a -> ('a, 'x, 'res) spine
  | R : ('x -> 'b, 'x, 'res) spine -> ('b, 'x, 'res) spine
  | A : ('a -> 'b, 'x, 'res) spine * ('a, 'res) app -> ('b, 'x, 'res) spine
end

type meta = { name : string; index : int; fields : string array }

type ('a, +'res) view = ('a -> ('a, 'a, 'res) V.spine) * ('a -> meta)
type ('a, +'res) schema = (('a, 'a, 'res) S.spine * meta) list

let spine = fst and meta = snd

(* Generic representations of n-point types -- "generics." *)

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
  external (!:) : 'a r -> ('a, p) app = "%identity"
  external (!) : ('a, p) app -> 'a r = "%identity"

  (* Helpers to pick out view/schema and inject n arguments. *)

  let v0 x (y: _ g0)                   = x y.view
  let v1 x (y: _ g1) a                 = x (y.view !:a)
  let v2 x (y: _ g2) a b               = x (y.view !:a !:b)
  let v3 x (y: _ g3) a b c             = x (y.view !:a !:b !:c)
  let v4 x (y: _ g4) a b c d           = x (y.view !:a !:b !:c !:d)
  let v5 x (y: _ g5) a b c d e         = x (y.view !:a !:b !:c !:d !:e)
  let v6 x (y: _ g6) a b c d e f       = x (y.view !:a !:b !:c !:d !:e !:f)
  let v7 x (y: _ g7) a b c d e f g     = x (y.view !:a !:b !:c !:d !:e !:f !:g)
  let v8 x (y: _ g8) a b c d e f g h   = x (y.view !:a !:b !:c !:d !:e !:f !:g !:h)
  let v9 x (y: _ g9) a b c d e f g h i = x (y.view !:a !:b !:c !:d !:e !:f !:g !:h !:i)

  let s0 x (y: _ g0)                   = x y.schema
  let s1 x (y: _ g1) a                 = x (y.schema !:a)
  let s2 x (y: _ g2) a b               = x (y.schema !:a !:b)
  let s3 x (y: _ g3) a b c             = x (y.schema !:a !:b !:c)
  let s4 x (y: _ g4) a b c d           = x (y.schema !:a !:b !:c !:d)
  let s5 x (y: _ g5) a b c d e         = x (y.schema !:a !:b !:c !:d !:e)
  let s6 x (y: _ g6) a b c d e f       = x (y.schema !:a !:b !:c !:d !:e !:f)
  let s7 x (y: _ g7) a b c d e f g     = x (y.schema !:a !:b !:c !:d !:e !:f !:g)
  let s8 x (y: _ g8) a b c d e f g h   = x (y.schema !:a !:b !:c !:d !:e !:f !:g !:h)
  let s9 x (y: _ g9) a b c d e f g h i = x (y.schema !:a !:b !:c !:d !:e !:f !:g !:h !:i)

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

(* pp *)

let invalid_arg fmt = Format.kasprintf invalid_arg fmt

let pf = Format.fprintf
let string = Format.pp_print_string
let semi ppf () = pf ppf ";@ "
let pp_iter ?(sep=semi) iter pp ppf v =
  let first = ref true in
  let f x = if !first then first := false else sep ppf (); pp ppf x in
  iter f v

let pp_meta ppf m = match m.fields with
| [||] -> pf ppf "%s (...)" m.name
| fs ->
    pf ppf "@[<1>%a{%a}@]"
    (fun ppf -> function "" -> () | n -> string ppf (n ^ " ")) m.name
    (pp_iter Array.iter string) fs

(* Metablock stuff. *)

let variant ?(fields = [||]) name index = { name; index; fields }
let record fields = { name = ""; index = 0; fields }

let name m = m.name
let fields m = Array.length m.fields
let has_field { fields; _ } f = Array.exists (String.equal f) fields

let err_field i m = invalid_arg "Tpf: invalid field #%d of %a" i pp_meta m
let field ({ fields; _ } as m) i =
  Array.(if 0 <= i && i < length fields then unsafe_get fields i
         else err_field i m)
