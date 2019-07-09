(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Tpf

let pf = Format.fprintf
let failwith fmt = Format.kasprintf failwith fmt

let sexp: _ data2 =
  let open Sexplib0.Sexp in
  let atom x = Atom x and list xs = List xs in
  let m0 = variant 0 "Atom"
  and m1 = variant 0 "List"
  and k0 = V.K atom and k1 = V.K list in
  { view = (fun s sx ->
      V.(function Atom a -> A (k0, a, s) | List xs -> A (k1, xs, sx)),
      (function Atom _ -> m0 | List _ -> m1))
  ; schema = S.(fun s sx -> [A (K atom, s), m0; A (K list, sx), m1]) }

module To = struct

  type 'a e = 'a -> Sexplib0.Sexp.t
  module G = Generic (struct type 'a q = 'a e end)

  open G
  open V
  open Sexplib0.Sexp

  let field m i x = List [Atom (label m i); x]

  let rec g_to_sexp: 'a. ('a, _) view -> 'a e = fun v x ->
    let rec variant: 'a. _ -> _ -> ('a, _, _) spine -> _ = fun v0 acc -> function
    | K _ -> acc
    | A (s, a, f) -> variant v0 (!:f a :: acc) s
    | R (s, a) -> variant v0 (g_to_sexp v0 a :: acc) s in
    let rec record: 'a. _ -> _ -> _ -> _ -> ('a, _, _) spine -> _ =
      fun v0 acc m i -> function
    | K _ -> acc
    | A (s, a, f) -> record v0 (field m i (!:f a) :: acc) m (i - i) s
    | R (s, a) -> record v0 (field m i (g_to_sexp v0 a) :: acc) m (i - 1) s in
    let m = meta v x in
    match spine v x, labels m, name m with
    | K _, _, name -> Atom name
    | s  , 0, name -> List (Atom name :: variant v [] s)
    | s  , _, ""   -> List (record v [] m (labels m - 1) s)
    | s  , _, name -> List (Atom name :: record v [] m (labels m - 1) s)

  include G.P
  include G.View (struct type 'a r = 'a e let gfun = g_to_sexp end)
end

module Smap = Map.Make (struct
  type t = string
  let compare (a: string) b = compare a b
end)

module Of = struct

  type 'a d = Sexplib0.Sexp.t -> 'a
  module G = Generic (struct type 'a q = 'a d end)

  open G
  open S
  open Sexplib0.Sexp

  let err_tagged_form () = failwith "expecting atom or list with a head atom"
  let err_arity () = failwith "variant arity mismatch"
  let err_record_form () = failwith "expecting a list of record components"
  let err_missing_field = failwith "missing record element: %s"
  let err_duplicate_field = failwith "duplicate field: %s"
  let err_extra_fields m map =
    let pp ppf =
      Smap.iter (fun f _ -> if not (has_label m f) then pf ppf " %s" f) in
    failwith "extra fields:%a" pp map
  let of_sexp_error exn sexp = match exn with
  | Failure err ->
      raise (Of_sexp_error (Failure ("Tpf_sexplib.g_of_sexp: " ^ err), sexp))
  | _ -> raise (Of_sexp_error (exn, sexp))
  let err_tag sexp = of_sexp_error (Failure "unexpected variant") sexp
  let err_record_component =
    of_sexp_error (Failure "bad record element: expecting (<name> <value>)")

  (* Anamorphisms have a couple of unrolled steps to shrink the closure
     chains. But adding too much costs time. *)

  let variant goto10 s =
    let rec go: 'a. ('a, _, _) spine -> ('a -> _) -> _ =
      fun s k -> match s with
    | K v -> fun xs -> k v xs
    | A (A (A (s, a), b), c) ->
        go s (fun f -> function
          | x0::x1::x2::xs -> k (f (!:a x0) (!:b x1) (!:c x2)) xs
          | _ -> err_arity ())
    | A (A (s, a), b) ->
        go s (fun f -> function
          | x0::x1::xs -> k (f (!:a x0) (!:b x1)) xs
          | _ -> err_arity ())
    | A (s, a) ->
        go s (fun f -> function
          | x::xs -> k (f (!:a x)) xs
          | _ -> err_arity ())
    | R s ->
        go s (fun f -> function
          | x::xs -> k (f (goto10 x)) xs
          | _ -> err_arity ()) in
    go s (fun x -> function [] -> x | _ -> err_arity ())

  let field_map_of_sexp xs =
    let f map = function
    | List [Atom field; xs] ->
        if Smap.mem field map then err_duplicate_field field
        else Smap.add field xs map
    | sexp -> err_record_component sexp in
    List.fold_left f Smap.empty xs

  let get_field m i map =
    let f = label m i in
    match Smap.find_opt f map with
      Some x -> x | _ -> err_missing_field f

  let record goto10 m s =
    let rec go: 'a. ('a, _, _) spine -> _ -> ('a -> _) -> _ =
      fun s i k -> match s with
    | K c -> fun map -> k c map
    | A (A (A (s, a), b), c) ->
        go s (i - 3) (fun f map ->
          k (f (!:a (get_field m (i - 2) map))
               (!:b (get_field m (i - 1) map))
               (!:c (get_field m i map)))
            map)
    | A (A (s, a), b) ->
        go s (i - 2) (fun f map ->
          k (f (!:a (get_field m (i - 1) map))
               (!:b (get_field m i map)))
            map)
    | A (s, a) -> go s (i - 1) (fun f map -> k (f (!:a (get_field m i map))) map)
    | R s -> go s (i - 1) (fun f map -> k (f (goto10 (get_field m i map))) map)
    in
    let extract = go s (labels m - 1) (fun x _ -> x) in
    fun xs ->
      let map = field_map_of_sexp xs in
      if Smap.cardinal map <= labels m then extract map
      else err_extra_fields m map

  let g_of_sexp = function
  | [s, m] when name m = "" ->
      let rec goto10 sexp =
        try match sexp with
        | List sexps -> Lazy.force f sexps
        | _ -> err_record_form ()
        with Failure _ as exn -> of_sexp_error exn sexp
      and f = lazy (record goto10 m s) in
      goto10
  | ss ->
      let rec map =
        let f map (s, m) =
          Smap.add (name m)
          (lazy (if labels m = 0 then variant goto10 s else record goto10 m s))
          map in
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

  include G.P
  include Schema (struct type 'a r = 'a d let gfun = g_of_sexp end)
end
