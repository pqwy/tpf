(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Tpf

(* Misc *)

let fix f =
  let rec g = lazy (f (fun x -> Lazy.force g x)) in
  Lazy.force g

(* Stdlib generics *)

let unit: _ data0 =
  let m0 = variant 0 "()" in
  let k0 = V.K () in
  { view = (fun () -> k0), (fun () -> m0)
  ; schema = S.[ K (), m0 ] }

let m0 = variant 0 "(,)"
let pair: _ data2 =
  let cons a b = a, b in
  let k0 = V.K cons in
  { view = (fun a b -> V.(fun (x, y) -> A (A (k0, x, a), y, b)),
           (fun _ -> m0))
  ; schema = fun a b -> S.[ A (A (K cons, a), b), m0 ] }

let m0 = variant 0 "(,,)"
let triple: _ data3 =
  let cons a b c = a, b, c in
  let k0 = V.K cons in
  { view = (fun a b c ->
      V.(fun (x, y, z) -> A (A (A (k0, x, a), y, b), z, c)),
      (fun _ -> m0))
  ; schema = S.(fun a b c -> [A (A (A (K cons, a), b), c), m0]) }

let m0 = variant 0 "(,,,)"
let quadruple: _ data4 =
  let cons a b c d = a, b, c, d in
  let k0 = V.K cons in
  { view = (fun a b c d ->
      V.(fun (x, y, z, w) -> A (A (A (A (k0, x, a), y, b), z, c), w, d)),
      (fun _ -> m0))
  ; schema = S.(fun a b c d -> [A (A (A (A (K cons, a), b), c), d), m0]) }

let m0 = variant 0 "[]" and m1 = variant 1 "(::)"
let list: _ data1 =
  let k0 = V.K [] and k1 = V.K List.cons in
  { view = (fun a ->
      V.(function [] -> k0 | x::xs -> R (A (k1, x, a), xs)),
      (function [] -> m0 | _ -> m1))
  ; schema = S.(fun a -> [K [], m0; R (A (K List.cons, a)), m1]) }

let m0 = variant 0 "Nil" and m1 = variant 1 "Cons"
let seq: _ data1 =
  let scons x xs () = Seq.Cons (x, xs) in
  let k0 = V.K Seq.empty and k1 = V.K scons in
  { view = Seq.(fun a ->
      V.(fun s -> match s () with
        | Cons (x, s) -> R (A (k1, x, a), s)
        | _ -> k0),
      (fun s -> match s () with Nil -> m0 | _ -> m1))
  ; schema = S.(fun a -> [K Seq.empty, m0; R (A (K scons, a)), m1]) }

let m0 = variant 0 "None" and m1 = variant 1 "Some"
let option: _ data1 =
  let some x = Some x in
  let k0 = V.K None and k1 = V.K some in
  { view = (fun a -> V.(function Some x -> A (k1, x, a) | _ -> k0),
           (function None -> m0 | _ -> m1))
  ; schema = S.(fun a -> [K None, m0; A (K some, a), m1]) }

let m0 = variant 0 "Ok" and m1 = variant 1 "Error"
let result: _ data2 =
  let ok x = Ok x and error x = Error x in
  let k0 = V.K ok and k1 = V.K error in
  { view = (fun a b ->
      V.(function Ok x -> A (k0, x, a) | Error y -> A (k1, y, b)),
      (function Ok _ -> m0 | _ -> m1))
  ; schema = S.(fun a b -> [A (K ok, a), m0; A (K error, b), m1]) }

(* Natural transformations. *)

type ('p, 'q) nat = { nat: 'a. ('a, 'p) app -> ('a, 'q) app }

let vmap {nat} =
  let open V in
  let rec go: 'a. ('a, _, _) spine -> ('a, _, _) spine = function
  | K k -> K k
  | A (s, a, f) -> A (go s, a, nat f)
  | R (s, a) -> R (go s, a) in
  (fun (spine, meta) -> (fun x -> go (spine x)), meta)

let smap {nat} =
  let open S in
  let rec go: 'a. ('a, _, _) spine -> ('a, _, _) spine = function
  | K k -> K k
  | A (s, a) -> A (go s, nat a)
  | R s -> R (go s) in
  List.map (fun (spine, meta) -> go spine, meta)


(* The Upside-down *)

module type AppV = sig
  type 'a t
  val pure : 'a -> 'a t
  val app : ('a -> 'b) t -> 'a t -> 'b t
  val gfun: meta -> 'a t -> 'a t
end

module AppV (A: AppV) = struct
  module G = Generic (struct type 'a q = 'a -> 'a A.t end)
  open G
  open V
  let gfun (spine, meta) = fix @@ fun goto10 ->
    let rec go: 'a. ('a, _, _) spine -> 'a A.t = function
    | K k -> A.pure k
    | A (s, a, f) -> let k = go s in A.app k (!:f a)
    | R (s, a) -> let k = go s in A.app k (goto10 a) in
    (fun x -> A.gfun (meta x) (go (spine x)))
  include P
  include View (struct type 'a r = 'a -> 'a A.t let gfun = gfun end)
end

module type AppS = sig
  type 'a t
  val pure : 'a -> 'a t
  val app : ('a -> 'b) t -> 'a t -> 'b t
  val retract : 'a t Lazy.t -> 'a t
  val gfun : ('a t Lazy.t * meta) list -> 'a t
end

module AppS (A: AppS) = struct
  module G = Generic (struct type 'a q = 'a A.t end)
  open G
  open S
  let gfun sch =
    let rec go: 'a. ('a, _, _) spine -> 'a A.t = function
    | K k -> A.pure k
    | A (s, a) -> let k = go s in A.app k !:a
    | R s -> let k = go s in A.app k (A.retract goto10)
    and goto10 = lazy (
      A.gfun (List.map (fun (spine, meta) -> lazy (go spine), meta) sch)) in
    Lazy.force goto10
  include P
  include Schema (struct type 'a r = 'a A.t let gfun = gfun end)
end

(* Type tags can be a private matter! *)

type _ ttag = ..

module Eq = struct
  type 'a eq = 'a -> 'a -> bool
  type 'a jmeq = { eq: 'b. 'a -> 'b ttag -> 'b -> bool }

  let jmeq (type a) (eq: a -> a -> bool) =
    let module M = struct type _ ttag += K : a ttag end in
    let eq (type b) a (t: b ttag) (b: b) =
      match t with M.K -> eq a b | _ -> false in
    { eq }, M.K

  module Eq1 = Generic (struct type 'a q = 'a jmeq end)
  module Eq2 = Generic (struct type 'a q = 'a ttag end)

  let g_eq ((spine1, meta1), (spine2, meta2)) =
    let open V in
    let rec eq a b =
      index (meta1 a) = index (meta2 b) && go (spine1 a) (spine2 b)
    and go: 'a 'b. ('a, _, _) spine -> ('b, _, _) spine -> bool =
      fun s1 s2 -> match s1, s2 with
    | K _          , K _           -> true
    | A (s1, a, af), A (s2, b, ag) -> go s1 s2 && Eq1.(!:af).eq a Eq2.(!:ag) b
    | R (s1, a)    , R (s2, b)     -> go s1 s2 && eq a b
    | _            , _             -> false in
    eq

  type p = Eq1.p
  type q = Eq2.p

  let ($$) (view1, view2) f =
    let (f1, f2) = jmeq f in view1 Eq1.(!f1), view2 Eq2.(!f2)

  let data0 (d: _ data0) =
    (d.view, d.view) |> g_eq
  let data1 (d: _ data1) eq1 =
    (d.view, d.view) $$ eq1 |> g_eq
  let data2 (d: _ data2) eq1 eq2 =
    (d.view, d.view) $$ eq1 $$ eq2 |> g_eq
  let data3 (d: _ data3) eq1 eq2 eq3 =
    (d.view, d.view) $$ eq1 $$ eq2 $$ eq3 |> g_eq
  let data4 (d: _ data4) eq1 eq2 eq3 eq4 =
    (d.view, d.view) $$ eq1 $$ eq2 $$ eq3 $$ eq4 |> g_eq
  let data5 (d: _ data5) eq1 eq2 eq3 eq4 eq5 =
    (d.view, d.view) $$ eq1 $$ eq2 $$ eq3 $$ eq4 $$ eq5 |> g_eq
  let data6 (d: _ data6) eq1 eq2 eq3 eq4 eq5 eq6 =
    (d.view, d.view) $$ eq1 $$ eq2 $$ eq3 $$ eq4 $$ eq5 $$ eq6 |> g_eq
  let data7 (d: _ data7) eq1 eq2 eq3 eq4 eq5 eq6 eq7 =
    (d.view, d.view) $$ eq1 $$ eq2 $$ eq3 $$ eq4 $$ eq5 $$ eq6 $$
    eq7 |> g_eq
  let data8 (d: _ data8) eq1 eq2 eq3 eq4 eq5 eq6 eq7 eq8 =
    (d.view, d.view) $$ eq1 $$ eq2 $$ eq3 $$ eq4 $$ eq5 $$ eq6 $$
    eq7 $$ eq8 |> g_eq
  let data9 (d: _ data9) eq1 eq2 eq3 eq4 eq5 eq6 eq7 eq8 eq9 =
    (d.view, d.view) $$ eq1 $$ eq2 $$ eq3 $$ eq4 $$ eq5 $$ eq6 $$
    eq7 $$ eq8 $$ eq9 |> g_eq
end

module Cmp = struct

  type 'a cmp = 'a -> 'a -> int
  type 'a jmcmp = { cmp: 'b. 'a -> 'b ttag -> 'b -> int }

  let lift (type a) (compare: a -> a -> int) =
    let module M = struct type _ ttag += K : a ttag end in
    let cmp (type b): _ -> b ttag -> b -> _ =
      fun a r b -> match r with M.K -> compare a b | _ -> 0 in
    { cmp }, M.K

  module Cmp1 = Generic (struct type 'a q = 'a jmcmp end)
  module Cmp2 = Generic (struct type 'a q = 'a ttag end)

  let err_spine () = invalid_arg "Tpf_std.compare: incoherent spine"

  let g_cmp ((spine1, meta1), (spine2, meta2)) =
    let open V in
    let rec cmp a b =
      let c = index (meta1 a) - index (meta2 b) in
      if c = 0 then go (spine1 a) (spine2 b) else if c < 0 then -1 else 1
    and go: 'a 'b. ('a, _, _) spine -> ('b, _, _) spine -> int =
      fun s1 s2 -> match s1, s2 with
    | A (s1, a, af), A (s2, b, ag) ->
      ( match go s1 s2 with 0 -> Cmp1.(!:af).cmp a Cmp2.(!:ag) b | c -> c )
    | R (s1, a), R (s2, b) ->
      ( match go s1 s2 with 0 -> cmp a b | c -> c )
    | K _, K _-> 0
    | _ -> err_spine () in
    cmp

  type p = Cmp1.p
  type q = Cmp2.p

  let ($$) (view1, view2) f =
    let (f1, f2) = lift f in view1 Cmp1.(!f1), view2 Cmp2.(!f2)

  let data0 (d: _ data0) =
    (d.view, d.view) |> g_cmp
  let data1 (d: _ data1) cmp1 =
    (d.view, d.view) $$ cmp1 |> g_cmp
  let data2 (d: _ data2) cmp1 cmp2 =
    (d.view, d.view) $$ cmp1 $$ cmp2 |> g_cmp
  let data3 (d: _ data3) cmp1 cmp2 cmp3 =
    (d.view, d.view) $$ cmp1 $$ cmp2 $$ cmp3 |> g_cmp
  let data4 (d: _ data4) cmp1 cmp2 cmp3 cmp4 =
    (d.view, d.view) $$ cmp1 $$ cmp2 $$ cmp3 $$ cmp4 |> g_cmp
  let data5 (d: _ data5) cmp1 cmp2 cmp3 cmp4 cmp5 =
    (d.view, d.view) $$ cmp1 $$ cmp2 $$ cmp3 $$ cmp4 $$ cmp5 |> g_cmp
  let data6 (d: _ data6) cmp1 cmp2 cmp3 cmp4 cmp5 cmp6 =
    (d.view, d.view) $$ cmp1 $$ cmp2 $$ cmp3 $$ cmp4 $$ cmp5 $$ cmp6 |> g_cmp
  let data7 (d: _ data7) cmp1 cmp2 cmp3 cmp4 cmp5 cmp6 cmp7 =
    (d.view, d.view) $$ cmp1 $$ cmp2 $$ cmp3 $$ cmp4 $$ cmp5 $$ cmp6 $$
    cmp7 |> g_cmp
  let data8 (d: _ data8) cmp1 cmp2 cmp3 cmp4 cmp5 cmp6 cmp7 cmp8 =
    (d.view, d.view) $$ cmp1 $$ cmp2 $$ cmp3 $$ cmp4 $$ cmp5 $$ cmp6 $$
    cmp7 $$ cmp8 |> g_cmp
  let data9 (d: _ data9) cmp1 cmp2 cmp3 cmp4 cmp5 cmp6 cmp7 cmp8 cmp9 =
    (d.view, d.view) $$ cmp1 $$ cmp2 $$ cmp3 $$ cmp4 $$ cmp5 $$ cmp6 $$
    cmp7 $$ cmp8 $$ cmp9 |> g_cmp
end

module Iter = struct
  module G = Generic (struct type 'a q = 'a -> unit end)
  open G
  open V
  let g_iter (spine, _) x =
    let rec go: 'a. ('a, _, _) spine -> _ = fun s v -> match s with
    | K _ -> ()
    | A (s, a, af) -> go s v; !:af a
    | R (s, a) -> go s v; go (v a) v in
    go (spine x) spine
  include P
  include View (struct type 'a r = 'a -> unit let gfun = g_iter end)
end

module Random = struct
  open Random
  module G = Generic (struct type 'a q = State.t -> 'a end)
  open S
  open G
  let err_base () = invalid_arg "Tpf_std.Random.g_gen: size limit exceeded"
  let rec null: 'a. ('a, _) schema -> 'a = function
  | [] -> err_base ()
  | (K x, _)::_ -> x
  | _::xs -> null xs
  let rec g_gen sch ?base size rng =
    let rec go: 'a. ('a, _, _) spine -> _ -> _ -> 'a =
      fun s size rng -> match s with
    | K k -> k
    | A (A (A (s, a), b), c) -> go s size rng (!:a rng) (!:b rng) (!:c rng)
    | A (A (s, a), b) -> go s size rng (!:a rng) (!:b rng)
    | A (s, a) -> go s size rng (!:a rng)
    | R s -> go s size rng (g_gen sch ?base size rng) in
    if size < 1 then match base with Some x -> x | _ -> null sch else
      let spine, _ = List.(nth sch (State.int rng (length sch))) in
      go spine (size - 1) rng
  include P
  include Schema (struct
    type 'a r = ?base:'a -> int -> State.t -> 'a
    let gfun = g_gen
  end)
end
