open Tpf

include Generic (struct type 'a r = 'a Fmt.t end)

let sep_by pp =
  let first = ref true in
  fun ppf () -> if !first then first := false else pp ppf ()

open V

let err_missing_fields s =
  Fmt.invalid_arg
  "Tpf_fmt.g_pp: `%s': some record elements are missing field labels"
  (v_name s)

let err_extra_fields s =
  Fmt.invalid_arg "Tpf_fmt.g_pp: `%s': extra field labels: %a"
  (v_name s) Fmt.(list ~sep:comma string)

let is_empty s = function [] -> () | fs -> err_extra_fields s fs

let parens pp ppf x = Fmt.(string ppf "("; pp ppf x; string ppf ")")
let braces pp ppf x = Fmt.(string ppf "{"; pp ppf x; string ppf "}")

let rec g_pp: type a. (a, _) view -> a Fmt.t = fun v ppf x ->
  let variant ppf s =
    let rec go: 'a. _ -> ('a, _, _) spine Fmt.t = fun sep ppf -> function
    | K _ -> ()
    | A (s, x, pp_x) -> go sep ppf s; sep ppf (); !!pp_x ppf x
    | R (s, x) -> go sep ppf s; sep ppf (); g_pp v ppf x in
    match s with
    | A (K _, x, pp_x) -> !!pp_x ppf x
    | R (K _, x) -> g_pp v ppf x
    | s -> parens (go (sep_by Fmt.comma)) ppf s
  and record fields ppf s =
    let field ppf s sep pp_x x = function
    | f::fs -> sep ppf (); Fmt.pf ppf "@[<1>%s =@ %a@]" f pp_x x; fs
    | [] -> err_missing_fields s in
    let rec go: 'a. _ -> _ -> _ -> ('a, _, _) spine -> _ =
      fun sep fields ppf -> function
    | K _ -> fields
    | A (s, x, pp_x) -> go sep fields ppf s |> field ppf s sep !!pp_x x
    | R (s, x) -> go sep fields ppf s |> field ppf s sep (g_pp v) x in
    let pp_s ppf s = go (sep_by Fmt.semi) fields ppf s |> is_empty s in
    braces pp_s ppf s in
  let s = v x in
  let m = v_meta s in
  match s, m.fields, m.name with
  | K _, _     , name -> Fmt.string ppf name
  | _  , []    , name -> Fmt.pf ppf "@[<1>%s@ %a@]" name variant s
  | _  , fields, ""   -> record fields ppf s
  | _  , fields, name -> Fmt.pf ppf "@[<1>%s@ %a@]" name (record fields) s

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
