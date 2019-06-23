(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(* Core types. *)

type (+'a, +'f) app

module V = struct
  type (+_, _, +_) spine =
  | K : 'a -> ('a, 'r, 'q) spine
  | A : ('a -> 'b, 'r, 'q) spine * 'a * ('a, 'q) app -> ('b, 'r, 'q) spine
  | R : ('r -> 'b, 'r, 'q) spine * 'r -> ('b, 'r, 'q) spine
end

module S = struct
  type (+_, _, +_) spine =
  | K : 'a -> ('a, 'r, 'q) spine
  | A : ('a -> 'b, 'r, 'q) spine * ('a, 'q) app -> ('b, 'r, 'q) spine
  | R : ('r -> 'b, 'r, 'q) spine -> ('b, 'r, 'q) spine
end

type meta = { name : string; index : int; fields : string array }

type ('a, +'q) view = ('a -> ('a, 'a, 'q) V.spine) * ('a -> meta)
type ('a, +'q) schema = (('a, 'a, 'q) S.spine * meta) list

let spine = fst and meta = snd

(* Generic representations of n-point types -- "generics." *)

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

(* Generic function families. *)

module type S = sig

  type 'a q
  type 'a r
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



(* [app] instantiation. *)

module Generic (Q: sig type 'a q end) = struct

  type 'a q = 'a Q.q

  type p (* Also known as the "brand". *)
  external (!:) : 'a q -> ('a, p) app = "%identity"
  external (!) : ('a, p) app -> 'a q = "%identity"

  let data0 x y                   = x y
  let data1 x y a                 = x (y !:a)
  let data2 x y a b               = x (y !:a !:b)
  let data3 x y a b c             = x (y !:a !:b !:c)
  let data4 x y a b c d           = x (y !:a !:b !:c !:d)
  let data5 x y a b c d e         = x (y !:a !:b !:c !:d !:e)
  let data6 x y a b c d e f       = x (y !:a !:b !:c !:d !:e !:f)
  let data7 x y a b c d e f g     = x (y !:a !:b !:c !:d !:e !:f !:g)
  let data8 x y a b c d e f g h   = x (y !:a !:b !:c !:d !:e !:f !:g !:h)
  let data9 x y a b c d e f g h i = x (y !:a !:b !:c !:d !:e !:f !:g !:h !:i)

  module View (F: sig type 'a r val gfun: ('a, p) view -> 'a r end):
  S with type 'a q := 'a q and type 'a r := 'a F.r = struct
    let data0 (d: _ data0) = data0 F.gfun d.view
    let data1 (d: _ data1) = data1 F.gfun d.view
    let data2 (d: _ data2) = data2 F.gfun d.view
    let data3 (d: _ data3) = data3 F.gfun d.view
    let data4 (d: _ data4) = data4 F.gfun d.view
    let data5 (d: _ data5) = data5 F.gfun d.view
    let data6 (d: _ data6) = data6 F.gfun d.view
    let data7 (d: _ data7) = data7 F.gfun d.view
    let data8 (d: _ data8) = data8 F.gfun d.view
    let data9 (d: _ data9) = data9 F.gfun d.view
  end

  module Schema (F: sig type 'a r val gfun: ('a, p) schema -> 'a r end):
  S with type 'a q := 'a q and type 'a r := 'a F.r = struct
    let data0 (d: _ data0) = data0 F.gfun d.schema
    let data1 (d: _ data1) = data1 F.gfun d.schema
    let data2 (d: _ data2) = data2 F.gfun d.schema
    let data3 (d: _ data3) = data3 F.gfun d.schema
    let data4 (d: _ data4) = data4 F.gfun d.schema
    let data5 (d: _ data5) = data5 F.gfun d.schema
    let data6 (d: _ data6) = data6 F.gfun d.schema
    let data7 (d: _ data7) = data7 F.gfun d.schema
    let data8 (d: _ data8) = data8 F.gfun d.schema
    let data9 (d: _ data9) = data9 F.gfun d.schema
  end
end

(* pp *)

let invalid_arg fmt = Format.kasprintf invalid_arg fmt

let pf = Format.fprintf
let pp_iter ?(sep = fun ppf () -> pf ppf ";@ ") iter pp ppf v =
  let first = ref true in
  let f x = if !first then first := false else sep ppf (); pp ppf x in
  iter f v

let pp_meta ppf m = match m.fields with
| [||] -> pf ppf "%s (...)" m.name
| fs ->
    pf ppf "@[<1>%a{%a}@]"
    (fun ppf -> function "" -> () | n -> pf ppf "%s " n) m.name
    (pp_iter Array.iter (fun ppf -> pf ppf "%s")) fs

(* Metablock stuff. *)

let variant ?(fields = [||]) name index = { name; index; fields }
let record fields = { name = ""; index = 0; fields }

let name m = m.name
let fields m = Array.length m.fields
let has_field { fields; _ } f = Array.exists (String.equal f) fields

let err_field i m = invalid_arg "Tpf: invalid field #%d of %a" i pp_meta m
let field ({ fields; _ } as m) i =
  if 0 <= i && i < Array.length fields then
    Array.unsafe_get fields i
  else err_field i m
