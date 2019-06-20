open Tpf
open Sexplib

let pf = Format.fprintf
let invalid_arg fmt = Format.kasprintf invalid_arg fmt
let failwith fmt = Format.kasprintf failwith fmt
let err_spine_fields _ = invalid_arg "Tpf: bad generic .... who what fields"

type 'a e = 'a -> Sexp.t

module Enc = struct

  include Generic (struct type 'a r = 'a e end)

  open V
  open Sexplib.Sexp

  let field f x = List [Atom f; x]

  let rec g_to_sexp: 'a. ('a, _) view -> 'a e = fun v x ->
    let rec variant: 'a. _ -> _ -> ('a, _, _) spine -> _ = fun v0 acc -> function
    | K _ -> acc
    | R (s, x) -> variant v0 (g_to_sexp v0 x :: acc) s
    | A (s, x, f) -> variant v0 (!!f x :: acc) s in
    let rec record: 'a. _ -> _ -> _ -> ('a, _, _) spine -> _ =
      fun v0 acc fields s -> match s, fields with
    | K _, [] -> acc
    | R (s, x), f::fs -> record v0 (field f (g_to_sexp v0 x) :: acc) fs s
    | A (s, x, f_x), f::fs -> record v0 (field f (!!f_x x) :: acc) fs s
    | _ -> err_spine_fields s in
    let s = v x in
    let m = v_meta s in
    match s, m.fields, m.name with
    | K _, _     , name -> Atom name
    | _  , []    , name -> List (Atom name :: variant v [] s)
    | _  , fields, ""   -> List (record v [] (List.rev fields) s)
    | _  , fields, name -> List (Atom name :: record v [] (List.rev fields) s)
end

module Smap = Map.Make (struct
  type t = string
  let compare (a: string) b = compare a b
end)

type 'a d = Sexp.t -> 'a

module Dec = struct

  include Generic (struct type 'a r = 'a d end)

  open S
  open Sexplib.Sexp

  let err_tagged_form _ = failwith "expecting atom or list with a head atom"
  let err_arity () = failwith "variant arity mismatch"
  let err_record_form _ = failwith "expecting a list of record components"
  let err_missing_field = failwith "missing record element: %s"
  let err_duplicate_field = failwith "duplicate field: %s"
  let err_extra_fields fields map =
    let pp ppf =
      Smap.iter (fun f _ -> if not (List.mem f fields) then pf ppf " %s" f) in
    failwith "extra fields:%a" pp map
  let of_sexp_error exn sexp = match exn with
  | Failure err ->
      raise (Sexplib.Conv.Of_sexp_error
              (Failure ("Tpf_sexplib.g_of_sexp: " ^ err), sexp))
  | _ -> raise (Sexplib.Conv.Of_sexp_error (exn, sexp))
  let err_tag sexp = of_sexp_error (Failure "unexpected variant") sexp
  let err_record_component =
    of_sexp_error (Failure "bad record element: expecting (<name> <value>)")

  let variant goto10 s =
    let rec go: 'a. ('a, _, _) spine -> ('a -> _) -> _ =
      fun s k -> match s with
    | K (f, _) -> fun xs -> k f xs
    | R s -> go s (fun f -> function
      | [] -> err_arity () | x::xs -> k (f (goto10 x)) xs)
    | A (s, f_a) -> go s (fun f -> function
      | [] -> err_arity () | x::xs -> k (f (!!f_a x)) xs) in
    go s (fun x -> function [] -> x | _ -> err_arity ())

  let field_map_of_sexp xs =
    let f map = function
    | List [Atom field; _] when Smap.mem field map -> err_duplicate_field field
    | List [Atom field; xs] -> Smap.add field xs map
    | sexp -> err_record_component sexp in
    List.fold_left f Smap.empty xs

  let get_field field map =
    match Smap.find_opt field map with
      Some x -> x | _ -> err_missing_field field

  let record goto10 s =
    let rec go: 'a. ('a, _, _) spine -> _ -> ('a -> _) -> _ =
      fun s rfields k -> match s, rfields with
    | K (f, _), [] -> (fun map -> k f map)
    | R s, field::fields ->
        go s fields (fun f map -> k (f (goto10 (get_field field map))) map)
    | A (s, f_a), field::fields ->
        go s fields (fun f map -> k (f (!!f_a (get_field field map))) map)
    | _ -> err_spine_fields s in
    let fields = s_fields s in
    let arity = List.length fields in
    let extract = go s (List.rev fields) (fun x _ -> x) in
    (fun xs ->
      let map = field_map_of_sexp xs in
      if Smap.cardinal map <= arity then extract map
      else err_extra_fields fields map)

  let g_of_sexp = function
  | [s] when (s_meta s).name == "" ->
      let rec goto10 sexp =
        try match sexp with
        | List sexps -> Lazy.force f sexps
        | _ -> err_record_form ()
        with Failure _ as exn -> of_sexp_error exn sexp
      and f = lazy (record goto10 s) in
      goto10
  | ss ->
      let rec map =
        let f map s =
          let m = s_meta s in
          let fields = m.fields and name = m.name in
          match fields with
          | [] -> Smap.add name (lazy (variant goto10 s)) map
          | _ -> Smap.add name (lazy (record goto10 s)) map in
        lazy (List.fold_left f Smap.empty ss)
      and goto10 sexp =
        try match sexp with
        | Atom name ->
            Lazy.(force (Smap.find name (force map))) []
        | List (Atom name :: sexp) ->
            Lazy.(force (Smap.find name (force map))) sexp
        | _ -> err_tagged_form ()
        with | Failure _ as exn -> of_sexp_error exn sexp
             | Not_found -> err_tag sexp in
      goto10
end

let g_to_sexp0 g = Enc.(v0 g_to_sexp g)
let g_to_sexp1 g = Enc.(v1 g_to_sexp g)
let g_to_sexp2 g = Enc.(v2 g_to_sexp g)
let g_to_sexp3 g = Enc.(v3 g_to_sexp g)
let g_to_sexp4 g = Enc.(v4 g_to_sexp g)
let g_to_sexp5 g = Enc.(v5 g_to_sexp g)
let g_to_sexp6 g = Enc.(v6 g_to_sexp g)
let g_to_sexp7 g = Enc.(v7 g_to_sexp g)
let g_to_sexp8 g = Enc.(v8 g_to_sexp g)
let g_to_sexp9 g = Enc.(v9 g_to_sexp g)

let g_of_sexp0 g = Dec.(s0 g_of_sexp g)
let g_of_sexp1 g = Dec.(s1 g_of_sexp g)
let g_of_sexp2 g = Dec.(s2 g_of_sexp g)
let g_of_sexp3 g = Dec.(s3 g_of_sexp g)
let g_of_sexp4 g = Dec.(s4 g_of_sexp g)
let g_of_sexp5 g = Dec.(s5 g_of_sexp g)
let g_of_sexp6 g = Dec.(s6 g_of_sexp g)
let g_of_sexp7 g = Dec.(s7 g_of_sexp g)
let g_of_sexp8 g = Dec.(s8 g_of_sexp g)
let g_of_sexp9 g = Dec.(s9 g_of_sexp g)