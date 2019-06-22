open Tpf

let sep_by pp =
  let first = ref true in
  fun ppf () -> if !first then first := false else pp ppf ()

include Generic (struct type 'a r = 'a Fmt.t end)

open V

let parens pp ppf x = Fmt.(string ppf "("; pp ppf x; string ppf ")")
let braces pp ppf x = Fmt.(string ppf "{"; pp ppf x; string ppf "}")

let rec g_pp: 'a. ('a, _) view -> 'a Fmt.t = fun v ppf x ->
  let variant ppf s =
    let sep = sep_by Fmt.comma in
    let rec go: 'a. ('a, _, _) spine Fmt.t = fun ppf -> function
    | K _ -> ()
    | A (s, a, f) -> go ppf s; sep ppf (); !f ppf a
    | R (s, a) -> go ppf s; sep ppf (); g_pp v ppf a in
    match s with
    | A (K _, a, f) -> !f ppf a
    | R (K _, a) -> g_pp v ppf a
    | s -> parens go ppf s
  and record m ppf s =
    let sep = sep_by Fmt.semi in
    let field ppf i pp_x x =
      sep ppf (); Fmt.pf ppf "@[<1>%s =@ %a@]" (field m i) pp_x x in
    let rec go: 'a. _ -> _ -> ('a, _, _) spine -> _ =
      fun i ppf -> function
    | K _ -> ()
    | A (s, a, f) -> go (i - 1) ppf s; field ppf i !f a
    | R (s, a) -> go (i - 1) ppf s; field ppf i (g_pp v) a in
    let pp_s ppf s = go (fields m - 1) ppf s in
    braces pp_s ppf s in
  let m = meta v x in
  match spine v x, fields m, name m with
  | K _, _, name -> Fmt.string ppf name
  | s  , 0, name -> Fmt.pf ppf "@[<1>%s@ %a@]" name variant s
  | s  , _, ""   -> record m ppf s
  | s  , _, name -> Fmt.pf ppf "@[<1>%s@ %a@]" name (record m) s

(* Either *)
let g_pp0 g0 = v0 g_pp g0
let g_pp1 g1 = v1 g_pp g1
let g_pp2 g2 = v2 g_pp g2
let g_pp3 g3 = v3 g_pp g3
let g_pp4 g4 = v4 g_pp g4
let g_pp5 g5 = v5 g_pp g5
let g_pp6 g6 = v6 g_pp g6
let g_pp7 g7 = v7 g_pp g7
let g_pp8 g8 = v8 g_pp g8
let g_pp9 g9 = v9 g_pp g9

(* Or *)
include (View_f (struct let f = g_pp end): Gfun with type 'a r := 'a r)
