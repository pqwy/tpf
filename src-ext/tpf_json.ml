(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Tpf

type json =
  [ `Null | `Bool of bool | `Int of int | `Float of float| `String of string
  | `List of json list | `Assoc of (string * json) list ]

type error = [`Msg of string]
type 'a e = 'a -> json
type 'a d = json -> ('a, error) result

let pf ppf fmt = Format.fprintf ppf fmt
let kstrf = Format.kasprintf
let pp_error ppf (`Msg err) = pf ppf "%s" err

module Enc = struct

  module G = Generic (struct type 'a q = 'a e end)
  open G
  open V

  type sum = string -> json list -> json
  let arr name xs = `List (`String name :: xs)
  let obj ?(tag = "tag") ?(values = "values") name xs =
    `Assoc [tag, `String name; values, `List xs]

  let rec g_to_json ?(sum = arr) (spine, meta as v) x =
    let m = meta x in
    let rec go_v: 'a. ('a, _, _) spine -> _ = fun s acc -> match s with
    | K _ -> acc
    | A (s, a, af) -> go_v s (!:af a :: acc)
    | R (s, a) -> go_v s (g_to_json ~sum v a :: acc) in
    let rec go_r: 'a. ('a, _, _) spine -> _ = fun s i acc -> match s with
    | K _ -> acc
    | A (s, a, af) -> go_r s (i - 1) ((label m i, !:af a) :: acc)
    | R (s, a) -> go_r s (i - 1) ((label m i, g_to_json ~sum v a) :: acc) in
    match spine x, labels m, name m with
    | K _, _, name -> `String name
    | s  , n, ""   -> `Assoc (go_r s (n - 1) [])
    | s  , 0, name -> sum name (go_v s [])
    | s  , n, name -> sum name [`Assoc (go_r s (n - 1) [])]

  include P

  let data0 ?sum (d: _ data0) = app0 (g_to_json ?sum) d.view
  let data1 ?sum (d: _ data1) = app1 (g_to_json ?sum) d.view
  let data2 ?sum (d: _ data2) = app2 (g_to_json ?sum) d.view
  let data3 ?sum (d: _ data3) = app3 (g_to_json ?sum) d.view
  let data4 ?sum (d: _ data4) = app4 (g_to_json ?sum) d.view
  let data5 ?sum (d: _ data5) = app5 (g_to_json ?sum) d.view
  let data6 ?sum (d: _ data6) = app6 (g_to_json ?sum) d.view
  let data7 ?sum (d: _ data7) = app7 (g_to_json ?sum) d.view
  let data8 ?sum (d: _ data8) = app8 (g_to_json ?sum) d.view
  let data9 ?sum (d: _ data9) = app9 (g_to_json ?sum) d.view
end

module Smap = Map.Make (struct
  type t = string
  let compare (a: string) b = compare a b
end)
let of_list xs = List.fold_left (fun m (k, v) -> Smap.add k v m) Smap.empty xs

module Dec = struct

  type sum = (string * json list) d
  module G = Generic (struct type 'a q = 'a d end)
  open G
  open S

  let error_msg fmt = kstrf (fun err -> Error (`Msg err)) fmt

  exception Whoops of error
  let to_exn = function Ok x -> x | Error e -> raise (Whoops e)

  let raise_msg fmt = kstrf (fun err -> raise (Whoops (`Msg err))) fmt
  let err_arity () = raise_msg "variant arity mismatch"
  let err_missing_label x = raise_msg "missing record element: %s" x
  let err_extra_labels meta map =
    let pp ppf f _ = if not (has_label meta f) then pf ppf " %s" f in
    raise_msg "extra fields:%a" (fun ppf -> Smap.iter (pp ppf)) map
  let err_expect_obj () = raise_msg "expecting object"
  let err_expect_single_obj () = raise_msg "expecting single-object array"

  let err_expect_arr = error_msg "expecting array"
  let err_expect_sum_obj = error_msg "expecting {%S: \"<CTOR>\", %S: [<ARGS>]}"
  let err_tag = error_msg "unexpected variant"

  let refl f json = !:f json |> to_exn

  let of_variant goto10 s =
    let rec go: 'a. ('a, _, _) spine -> ('a -> _) -> _ = fun s k -> match s with
    | K c -> fun json -> k c json
    | A (A (A (s, af), bf), cf) ->
        go s (fun f -> function
          | x0::x1::x2::xs -> k (f (refl af x0) (refl bf x1) (refl cf x2)) xs
          | _ -> err_arity ())
    | A (A (s, af), bf) ->
        go s (fun f -> function
          | x0::x1::xs -> k (f (refl af x0) (refl bf x1)) xs
          | _ -> err_arity ())
    | A (s, af) ->
        go s (fun f -> function
          | x::xs -> k (refl af x |> f) xs
          | _ -> err_arity ())
    | R s ->
        go s (fun f -> function
          | [] -> err_arity ()
          | x::xs -> k (goto10 x |> f) xs) in
    go s (fun x -> function [] -> x | _ -> err_arity ())

  let get label map = match Smap.find_opt label map with
  | Some json -> json | _ -> err_missing_label label
  let refl f label map = match Smap.find_opt label map with
  | None -> err_missing_label label
  | Some json -> !:f json |> to_exn

  let of_record goto10 s m =
    let rec go: 'a. _ -> ('a, _, _) spine -> _ -> 'a = fun i -> function
    | K c -> fun _ -> c
    | A (A (A (s, af), bf), cf) ->
        let f = go (i - 3) s in fun map ->
          let k0 = label m (i - 2) and k1 = label m (i - 1) and k2 = label m i in
          f map (refl af k0 map) (refl bf k1 map) (refl cf k2 map)
    | A (A (s, af), bf) ->
        let f = go (i - 2) s
        and k0 = label m (i - 1) and k1 = label m i in
        fun map -> f map (refl af k0 map) (refl bf k1 map)
    | A (s, af) ->
        let f = go (i - 1) s and k = label m i in
        fun map -> f map (refl af k map)
    | R s ->
        let f = go (i - 1) s and k = label m i in
        fun map -> f map (goto10 (get k map)) in
    let n = labels m in
    let f = go (n - 1) s in function
    | `Assoc xs ->
        let map = of_list xs in
        if Smap.cardinal map <= n then f map else err_extra_labels m map
    | _ -> err_expect_obj ()

  let arr = function
  | `List (`String name::jsons) -> Ok (name, jsons)
  | _ -> err_expect_arr
  let obj ?(tag = "tag") ?(values = "values") = function
  | `Assoc [f0, `String name; f1, `List jsons]
  | `Assoc [f1, `List jsons; f0, `String name]
      when f0 = tag && f1 = values -> Ok (name, jsons)
  | _ -> err_expect_sum_obj tag values

  let g_of_json ?(sum = arr) (type a) (schema: (a, p) schema) =
    let of_json = Tpf_std.fix @@ fun goto10 -> match schema with
    | [s, m] when name m = "" -> of_record goto10 s m
    | ss ->
        let to_f s m = match labels m with
        | 0 -> of_variant goto10 s
        | _ ->
            let f = of_record goto10 s m in
            function [xs] -> f xs | _ -> err_expect_single_obj () in
        let map =
          of_list @@ List.map (fun (s, m) -> name m, lazy (to_f s m)) ss in
        function
        | `String name -> Lazy.force (Smap.find name map) []
        | json ->
            let (name, jsons) = sum json |> to_exn in
            Lazy.force (Smap.find name map) jsons in
    fun json -> try Ok (of_json json) with
    | Whoops err -> Error err
    | Not_found -> err_tag

  include P

  let data0 ?sum (d: _ data0) = app0 (g_of_json ?sum) d.schema
  let data1 ?sum (d: _ data1) = app1 (g_of_json ?sum) d.schema
  let data2 ?sum (d: _ data2) = app2 (g_of_json ?sum) d.schema
  let data3 ?sum (d: _ data3) = app3 (g_of_json ?sum) d.schema
  let data4 ?sum (d: _ data4) = app4 (g_of_json ?sum) d.schema
  let data5 ?sum (d: _ data5) = app5 (g_of_json ?sum) d.schema
  let data6 ?sum (d: _ data6) = app6 (g_of_json ?sum) d.schema
  let data7 ?sum (d: _ data7) = app7 (g_of_json ?sum) d.schema
  let data8 ?sum (d: _ data8) = app8 (g_of_json ?sum) d.schema
  let data9 ?sum (d: _ data9) = app9 (g_of_json ?sum) d.schema
end
