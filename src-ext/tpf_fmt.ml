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
  let pp_kv ppf (label, pp) = Fmt.pf ppf "@[<1>%s =@ %a@]" label pp () in
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
  | A (s, a, af) -> go_r s m (i - 1) ((label m i, Fmt.const !:af a) :: acc)
  | R (s, a) -> go_r s m (i - 1) ((label m i, Fmt.const goto10 a) :: acc) in
  let m = meta v x in
  match spine v x, labels m, name m with
  | K _, _, name -> sum name ppf []
  | s  , 0, name -> sum name ppf (go_v s [])
  | s  , _, ""   -> record None ppf (go_r s m (labels m - 1) [])
  | s  , _, name -> record (Some name) ppf (go_r s m (labels m - 1) [])

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


(* let sep_by pp = *)
(*   let first = ref true in *)
(*   fun ppf () -> if !first then first := false else pp ppf () *)

(* let parens pp ppf x = Fmt.(string ppf "("; pp ppf x; string ppf ")") *)

(* let rec g_pp: 'a. ('a, _) view -> 'a Fmt.t = fun v ppf x -> *)
(*   let variant ppf s = *)
(*     let sep = sep_by Fmt.comma in *)
(*     let rec go: 'a. ('a, _, _) spine Fmt.t = fun ppf -> function *)
(*     | K _ -> () *)
(*     | A (s, a, f) -> go ppf s; sep ppf (); !:f ppf a *)
(*     | R (s, a) -> go ppf s; sep ppf (); g_pp v ppf a in *)
(*     match s with *)
(*     | A (K _, a, f) -> !:f ppf a *)
(*     | R (K _, a) -> g_pp v ppf a *)
(*     | s -> parens go ppf s *)
(*   and record m ppf s = *)
(*     let sep = sep_by Fmt.semi in *)
(*     let field ppf i pp_x x = *)
(*       sep ppf (); Fmt.pf ppf "@[<1>%s =@ %a@]" (field m i) pp_x x in *)
(*     let rec go: 'a. _ -> _ -> ('a, _, _) spine -> _ = *)
(*       fun i ppf -> function *)
(*     | K _ -> () *)
(*     | A (s, a, f) -> go (i - 1) ppf s; field ppf i !:f a *)
(*     | R (s, a) -> go (i - 1) ppf s; field ppf i (g_pp v) a in *)
(*     let pp_s ppf s = go (fields m - 1) ppf s in *)
(*     pp_s ppf s in *)
(*   let m = meta v x in *)
(*   match spine v x, fields m, name m with *)
(*   | K _, _, name -> Fmt.string ppf name *)
(*   | s  , 0, name -> Fmt.pf ppf "@[<1>%s@ %a@]" name variant s *)
(*   | s  , _, ""   -> Fmt.pf ppf "@[<1>{%a}@]" (record m) s *)
(*   | s  , _, name -> Fmt.pf ppf "@[<1>%s@ %a@]" name (record m) s *)

(* include P *)
(* include View (struct type 'a r = 'a Fmt.t let gfun = g_pp end) *)
