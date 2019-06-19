open Tpf

include Generic (struct type 'a r = 'a Fmt.t end)

let intersperse pp =
  let first = ref true in
  fun ppf () -> if !first then first := false else pp ppf ()

open V

let err_missing_fields s =
  Fmt.invalid_arg
  "Tpf_fmt.g_pp: `%s': some record elements are missing field labels"
  (meta s).name
let err_extra_fields s =
  Fmt.invalid_arg "Tpf_fmt.g_pp: `%s': extra field labels: %a"
  (meta s).name Fmt.(list ~sep:comma string)

let named ppf = Fmt.pf ppf "@[<1>%s@ %a@]"
let field ppf = Fmt.pf ppf "@[<1>%s =@ %a@]"

let variant ppf pp_rec name = function
| K _ -> Fmt.string ppf name
| A (K _, x, pp_x) -> named ppf name !!pp_x x
| R (K _, x) -> named ppf name pp_rec x
| s ->
    let rec go: type a. _ -> (a, _, _) spine Fmt.t = fun sep ppf -> function
    | K _ -> ()
    | A (s, x, pp_x) -> go sep ppf s; sep ppf (); !!pp_x ppf x
    | R (s, x) -> go sep ppf s; sep ppf (); pp_rec ppf x in
    named ppf name (Fmt.parens (go (intersperse Fmt.comma))) s

let record ppf pp_rec fields =
  let rec go: type a. _ -> _ -> _ -> (a, _, _) spine -> _ =
    fun sep fields ppf -> function
    | K _ -> fields
    | A (s, x, pp_x) ->
      ( match go sep fields ppf s with
        | f::fs -> sep ppf (); field ppf f !!pp_x x; fs
        | [] -> err_missing_fields s )
    | R (s, x) ->
      ( match go sep fields ppf s with
        | f::fs -> sep ppf (); field ppf f pp_rec x; fs
        | [] -> err_missing_fields s ) in
  let pp ppf s = match go (intersperse Fmt.semi) fields ppf s with
  | [] -> ()
  | fs -> err_extra_fields s fs in
  Fmt.braces pp ppf

let rec g_pp: type a. (a, _) view -> a Fmt.t = fun v ppf x ->
  let s = v x in
  let meta = meta s in
  match meta.fields with
  | [] -> variant ppf (g_pp v) meta.name s
  | fields -> match meta.name with
      | "" -> record ppf (g_pp v) fields s
      | name -> named ppf name (fun ppf -> record ppf (g_pp v) fields) s

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
