(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Tpf

module G = Generic (struct type 'a q = 'a Fmt.t end)
open G
open V

let pp_pp ppf pp = pp ppf ()
let sum name ppf = function
| [] -> Fmt.string ppf name
| [x] -> Fmt.pf ppf "@[<1>%s@ %a@]" name x ()
| xs -> Fmt.(pf ppf "@[<1>%s@ (%a)@]" name (list ~sep:comma pp_pp) xs)

let record name ppf xs =
  let pp_kv ppf (field, pp) = Fmt.pf ppf "@[<1>%s =@ %a@]" field pp () in
  match name with
  | Some n -> Fmt.(pf ppf "@[<1>%s@ {%a}@]" n (list ~sep:semi pp_kv) xs)
  | None -> Fmt.(pf ppf "@[<1>{%a}@]" (list ~sep:semi pp_kv) xs)

let rec g_pp ?(sum = sum) ?(record = record) v ppf x =
  let goto10 = g_pp ~sum ~record v in
  let rec go_v: 'a. ('a, _, _) spine -> _ = fun s acc -> match s with
  | K _ -> acc
  | A (s, a, af) -> go_v s (Fmt.const !:af a :: acc)
  | R (s, a) -> go_v s (Fmt.const goto10 a :: acc) in
  let rec go_r: 'a. ('a, _, _) spine -> _ = fun s m i acc -> match s with
  | K _ -> acc
  | A (s, a, af) -> go_r s m (i - 1) ((field m i, Fmt.const !:af a) :: acc)
  | R (s, a) -> go_r s m (i - 1) ((field m i, Fmt.const goto10 a) :: acc) in
  let m = meta v x in
  match spine v x, fields m, name m with
  | K _, _, name -> sum name ppf []
  | s  , 0, name -> sum name ppf (go_v s [])
  | s  , _, ""   -> record None ppf (go_r s m (fields m - 1) [])
  | s  , _, name -> record (Some name) ppf (go_r s m (fields m - 1) [])

type sum = string -> unit Fmt.t list Fmt.t
type record = string option -> (string * unit Fmt.t) list Fmt.t

include P

let data0 ?sum ?record (d: _ data0) = app0 (g_pp ?sum ?record) d.view
let data1 ?sum ?record (d: _ data1) = app1 (g_pp ?sum ?record) d.view
let data2 ?sum ?record (d: _ data2) = app2 (g_pp ?sum ?record) d.view
let data3 ?sum ?record (d: _ data3) = app3 (g_pp ?sum ?record) d.view
let data4 ?sum ?record (d: _ data4) = app4 (g_pp ?sum ?record) d.view
let data5 ?sum ?record (d: _ data5) = app5 (g_pp ?sum ?record) d.view
let data6 ?sum ?record (d: _ data6) = app6 (g_pp ?sum ?record) d.view
let data7 ?sum ?record (d: _ data7) = app7 (g_pp ?sum ?record) d.view
let data8 ?sum ?record (d: _ data8) = app8 (g_pp ?sum ?record) d.view
let data9 ?sum ?record (d: _ data9) = app9 (g_pp ?sum ?record) d.view
