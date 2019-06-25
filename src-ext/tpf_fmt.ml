(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Tpf

let sep_by pp =
  let first = ref true in
  fun ppf () -> if !first then first := false else pp ppf ()

module G = Generic (struct type 'a q = 'a Fmt.t end)
open G
open V

let parens pp ppf x = Fmt.(string ppf "("; pp ppf x; string ppf ")")

let rec g_pp: 'a. ('a, _) view -> 'a Fmt.t = fun v ppf x ->
  let variant ppf s =
    let sep = sep_by Fmt.comma in
    let rec go: 'a. ('a, _, _) spine Fmt.t = fun ppf -> function
    | K _ -> ()
    | A (s, a, f) -> go ppf s; sep ppf (); !:f ppf a
    | R (s, a) -> go ppf s; sep ppf (); g_pp v ppf a in
    match s with
    | A (K _, a, f) -> !:f ppf a
    | R (K _, a) -> g_pp v ppf a
    | s -> parens go ppf s
  and record m ppf s =
    let sep = sep_by Fmt.semi in
    let field ppf i pp_x x =
      sep ppf (); Fmt.pf ppf "@[<1>%s =@ %a@]" (field m i) pp_x x in
    let rec go: 'a. _ -> _ -> ('a, _, _) spine -> _ =
      fun i ppf -> function
    | K _ -> ()
    | A (s, a, f) -> go (i - 1) ppf s; field ppf i !:f a
    | R (s, a) -> go (i - 1) ppf s; field ppf i (g_pp v) a in
    let pp_s ppf s = go (fields m - 1) ppf s in
    pp_s ppf s in
  let m = meta v x in
  match spine v x, fields m, name m with
  | K _, _, name -> Fmt.string ppf name
  | s  , 0, name -> Fmt.pf ppf "@[<1>%s@ %a@]" name variant s
  | s  , _, ""   -> Fmt.pf ppf "@[<1>{%a}@]" (record m) s
  | s  , _, name -> Fmt.pf ppf "@[<1>%s@ %a@]" name (record m) s

include P
include View (struct type 'a r = 'a Fmt.t let gfun = g_pp end)
