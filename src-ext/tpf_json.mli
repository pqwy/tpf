(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Tpf

type json =
  [ `Null | `Bool of bool | `Float of float| `String of string
  | `A of json list | `O of (string * json) list ]

type 'a e = 'a -> json

val of_unit : unit e
val of_string : string e
val of_bool : bool e
val of_float : float e
val of_list : 'a e -> 'a list e

module To : sig
  val arr : string -> json list -> json
  val arr_no_ctor : string -> json list -> json
  val obj : ?tag:string -> ?contents:string -> string -> json list -> json
  type 'a entry = ?sum:(string -> json list -> json) -> 'a -> json
  include P with type 'a q := 'a e
  include Data with type 'a q := 'a e and type 'a r := 'a entry
  val g_to_json : ('a, p) view -> 'a entry
end

