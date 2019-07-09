(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(* View types. *)

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

type meta = { index : int; name : string; labels : string array }

type ('a, +'q) view = ('a -> ('a, 'a, 'q) V.spine) * ('a -> meta)
type ('a, +'q) schema = (('a, 'a, 'q) S.spine * meta) list

let spine = fst and meta = snd

(* Generic representations of n-point types -- "generics." *)

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

(* Client-facing exports. *)

module type P = sig
  type p
  type 'a q
  val (!) : 'a q -> ('a, p) app
end

module type Data = sig

  type 'a q
  type 'a r
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

(* [(_, _) app] instantiation. *)

module Generic (Q: sig type 'a q end) = struct

  type 'a q = 'a Q.q

  type p (* The "brand". *)
  external (!) : 'a q -> ('a, p) app = "%identity"
  external (!:) : ('a, p) app -> 'a q = "%identity"

  module P = struct
    type nonrec p = p
    let (!) = (!)
  end

  let app0 x y                   = x y
  let app1 x y a                 = x (y !a)
  let app2 x y a b               = x (y !a !b)
  let app3 x y a b c             = x (y !a !b !c)
  let app4 x y a b c d           = x (y !a !b !c !d)
  let app5 x y a b c d e         = x (y !a !b !c !d !e)
  let app6 x y a b c d e f       = x (y !a !b !c !d !e !f)
  let app7 x y a b c d e f g     = x (y !a !b !c !d !e !f !g)
  let app8 x y a b c d e f g h   = x (y !a !b !c !d !e !f !g !h)
  let app9 x y a b c d e f g h i = x (y !a !b !c !d !e !f !g !h !i)

  module View (F: sig type 'a r val gfun: ('a, p) view -> 'a r end):
  Data with type 'a q := 'a q and type 'a r := 'a F.r = struct
    let data0 (d: _ data0) = app0 F.gfun d.view
    let data1 (d: _ data1) = app1 F.gfun d.view
    let data2 (d: _ data2) = app2 F.gfun d.view
    let data3 (d: _ data3) = app3 F.gfun d.view
    let data4 (d: _ data4) = app4 F.gfun d.view
    let data5 (d: _ data5) = app5 F.gfun d.view
    let data6 (d: _ data6) = app6 F.gfun d.view
    let data7 (d: _ data7) = app7 F.gfun d.view
    let data8 (d: _ data8) = app8 F.gfun d.view
    let data9 (d: _ data9) = app9 F.gfun d.view
  end

  module Schema (F: sig type 'a r val gfun: ('a, p) schema -> 'a r end):
  Data with type 'a q := 'a q and type 'a r := 'a F.r = struct
    let data0 (d: _ data0) = app0 F.gfun d.schema
    let data1 (d: _ data1) = app1 F.gfun d.schema
    let data2 (d: _ data2) = app2 F.gfun d.schema
    let data3 (d: _ data3) = app3 F.gfun d.schema
    let data4 (d: _ data4) = app4 F.gfun d.schema
    let data5 (d: _ data5) = app5 F.gfun d.schema
    let data6 (d: _ data6) = app6 F.gfun d.schema
    let data7 (d: _ data7) = app7 F.gfun d.schema
    let data8 (d: _ data8) = app8 F.gfun d.schema
    let data9 (d: _ data9) = app9 F.gfun d.schema
  end
end

(* pp *)

let invalid_arg fmt = Format.kasprintf invalid_arg fmt

let pf = Format.fprintf
let pp_string = Format.pp_print_string
let pp_iter ?(sep = fun ppf () -> pf ppf ";@ ") iter pp ppf v =
  let first = ref true in
  let f x = if !first then first := false else sep ppf (); pp ppf x in
  iter f v

let pp_meta ppf m =
  let pp_name ppf = function "" -> () | name -> pf ppf "%s " name
  and pp_body ppf = function
  | [||] -> pp_string ppf "(...)"
  | fs -> pf ppf "{%a}" (pp_iter Array.iter pp_string) fs in
  pf ppf "@[<1>%a%a@]" pp_name m.name pp_body m.labels

(* Metablock stuff. *)

let variant ?(labels = [||]) index name = { name; index; labels }
let record labels = { name = ""; index = 0; labels }

let name m = m.name
let index m = m.index
let labels m = Array.length m.labels
let has_label m x = Array.exists (String.equal x) m.labels

let err_label i m = invalid_arg "Tpf: invalid label #%d of %a" i pp_meta m
let label ({ labels; _ } as m) i =
  if 0 <= i && i < Array.length labels then
    Array.unsafe_get labels i
  else err_label i m
