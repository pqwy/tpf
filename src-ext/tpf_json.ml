(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Tpf

type json =
  [ `Null | `Bool of bool | `Float of float| `String of string
  | `A of json list | `O of (string * json) list ]

type 'a e = 'a -> json

let of_unit () = `Null
let of_string s = `String s
let of_bool b = `Bool b
let of_float x = `Float x
let of_list f xs = `A (List.map f xs)

module To = struct
  module G = Generic (struct type 'a q = 'a e end)
  open G
  open V
  let arr n xs = `A (`String n :: xs)
  let arr_no_ctor _ xs = `A xs
  let obj ?(tag = "tag") ?(contents = "contents") n xs =
    `O [tag, `String n; contents, `A xs]
  type 'a entry = ?sum:(string -> json list -> json) -> 'a -> json
  let rec g_to_json v ?(sum = arr) x =
    let m = meta v x in
    let rec go_v: 'a. _ -> ('a, _, _) spine -> _ = fun acc -> function
    | K _ -> acc
    | A (s, a, af) -> go_v (!:af a :: acc) s
    | R (s, a) -> go_v (g_to_json v ~sum a::acc) s in
    let rec go_r: 'a. _ -> _ -> ('a, _, _) spine -> _ = fun i acc -> function
    | K _ -> `O acc
    | A (s, a, af) -> go_r (i - 1) ((field m i, !:af a)::acc) s
    | R (s, a) -> go_r (i - 1) ((field m i, g_to_json v ~sum a) :: acc) s in
    match spine v x, fields m, name m with
    | K _, _, name -> `String name
    | s  , _, ""   -> go_r (fields m - 1) [] s
    | s  , 0, name -> sum name (go_v [] s)
    | s  , _, name -> sum name [go_r (fields m - 1) [] s]
  include P
  include View (struct type nonrec 'a r = 'a entry let gfun = g_to_json end)
end

(* type err = json * string *)
(* exception Err of err *)
(* let run f json = try Ok (f json) with Err e -> Error e *)
(* let err json fmt = Format.kasprintf (fun msg -> raise (Err (json, msg))) fmt *)

(* type 'a d = 'a -> json *)

(* let of_unit = function `Null -> () | j -> err j "expected null" *)
(* let of_string = function `String s -> s | j -> err j "expected string" *)
(* let of_bool = function `Bool b -> b | j -> err j "expected bool" *)
(* let of_float = function `Float x -> x | j -> err j "expected float" *)
(* let of_list f = function `A xs -> List.map f xs | j -> err j "expected array" *)

(* module Of = struct *)
(*   module G = Generic (struct type 'a q = 'a d end) *)
(* end *)
