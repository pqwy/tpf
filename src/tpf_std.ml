(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Tpf

(* Stdlib generics *)

let m0 = variant "()" 0
let k0 = V.K ()
let unit: _ data0 =
  { view = (fun () -> k0), (fun () -> m0)
  ; schema = S.[ K (), m0 ] }

let cons a b = a, b
let m0 = variant "(,)" 0
let k0 = V.K cons
let pair: _ data2 =
  { view = (fun a b -> V.(fun (x, y) -> A (A (k0, x, a), y, b)),
           (fun _ -> m0))
  ; schema = fun a b -> S.[ A (A (K cons, a), b), m0 ] }

let cons a b c = a, b, c
let m0 = variant "(,,)" 0
let k0 = V.K cons
let triple: _ data3 =
  { view = (fun a b c ->
      V.(fun (x, y, z) -> A (A (A (k0, x, a), y, b), z, c)),
      (fun _ -> m0))
  ; schema = S.(fun a b c -> [A (A (A (K cons, a), b), c), m0]) }

let cons a b c d = a, b, c, d
let m0 = variant "(,,,)" 0
let k0 = V.K cons
let quadruple: _ data4 =
  { view = (fun a b c d ->
      V.(fun (x, y, z, w) -> A (A (A (A (k0, x, a), y, b), z, c), w, d)),
      (fun _ -> m0))
  ; schema = S.(fun a b c d -> [A (A (A (A (K cons, a), b), c), d), m0]) }

let m0 = variant "[]" 0 and m1 = variant "(::)" 1
let k0 = V.K [] and k1 = V.K List.cons
let list: _ data1 =
  { view = (fun a ->
      V.(function [] -> k0 | x::xs -> R (A (k1, x, a), xs)),
      (function [] -> m0 | _ -> m1))
  ; schema = S.(fun a -> [K [], m0; R (A (K List.cons, a)), m1]) }

let scons x xs () = Seq.Cons (x, xs)
let m0 = variant "Nil" 0 and m1 = variant "Cons" 1
let k0 = V.K Seq.empty and k1 = V.K scons
let seq: _ data1 =
  { view = Seq.(fun a ->
      V.(fun s -> match s () with
        | Cons (x, s) -> R (A (k1, x, a), s)
        | _ -> k0),
      (fun s -> match s () with Nil -> m0 | _ -> m1))
  ; schema = S.(fun a -> [K Seq.empty, m0; R (A (K scons, a)), m1]) }

let some x = Some x
let m0 = variant "None" 0 and m1 = variant "Some" 1
let k0 = V.K None and k1 = V.K some
let option: _ data1 =
  { view = (fun a -> V.(function Some x -> A (k1, x, a) | _ -> k0),
           (function None -> m0 | _ -> m1))
  ; schema = S.(fun a -> [K None, m0; A (K some, a), m1]) }

let ok x = Ok x and error x = Error x
let m0 = variant "Ok" 0 and m1 = variant "Error" 1
let k0 = V.K ok and k1 = V.K error
let result: _ data2 =
  { view = (fun a b ->
      V.(function Ok x -> A (k0, x, a) | Error y -> A (k1, y, b)),
      (function Ok _ -> m0 | _ -> m1))
  ; schema = S.(fun a b -> [A (K ok, a), m0; A (K error, b), m1]) }


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

  let g_eq (vs1, vm1) (vs2, vm2) =
    let open V in
    let rec eq a b = index (vm1 a) = index (vm2 b) && go (vs1 a) (vs2 b)
    and go: 'a 'b. ('a, _, _) spine -> ('b, _, _) spine -> bool =
      fun s1 s2 -> match s1, s2 with
    | K _         , K _          -> true
    | A (s1, a, f), A (s2, b, g) -> go s1 s2 && Eq1.(!:f).eq a Eq2.(!:g) b
    | R (s1, x1)  , R (s2, x2)   -> go s1 s2 && eq x1 x2
    | _           , _            -> false in
    eq

  type p = Eq1.p
  type q = Eq2.p

  let ($$) (view1, view2) f =
    let (f1, f2) = jmeq f in view1 Eq1.(!f1), view2 Eq2.(!f2)
  let go (v1, v2) = g_eq v1 v2

  let data0 (g: _ data0) =
    (g.view, g.view) |> go
  let data1 (g: _ data1) eq1 =
    (g.view, g.view) $$ eq1 |> go
  let data2 (g: _ data2) eq1 eq2 =
    (g.view, g.view) $$ eq1 $$ eq2 |> go
  let data3 (g: _ data3) eq1 eq2 eq3 =
    (g.view, g.view) $$ eq1 $$ eq2 $$ eq3 |> go
  let data4 (g: _ data4) eq1 eq2 eq3 eq4 =
    (g.view, g.view) $$ eq1 $$ eq2 $$ eq3 $$ eq4 |> go
  let data5 (g: _ data5) eq1 eq2 eq3 eq4 eq5 =
    (g.view, g.view) $$ eq1 $$ eq2 $$ eq3 $$ eq4 $$ eq5 |> go
  let data6 (g: _ data6) eq1 eq2 eq3 eq4 eq5 eq6 =
    (g.view, g.view) $$ eq1 $$ eq2 $$ eq3 $$ eq4 $$ eq5 $$ eq6 |> go
  let data7 (g: _ data7) eq1 eq2 eq3 eq4 eq5 eq6 eq7 =
    (g.view, g.view) $$ eq1 $$ eq2 $$ eq3 $$ eq4 $$ eq5 $$ eq6 $$ eq7 |> go
  let data8 (g: _ data8) eq1 eq2 eq3 eq4 eq5 eq6 eq7 eq8 =
    (g.view, g.view) $$ eq1 $$ eq2 $$ eq3 $$ eq4 $$ eq5 $$ eq6 $$ eq7 $$
    eq8 |> go
  let data9 (g: _ data9) eq1 eq2 eq3 eq4 eq5 eq6 eq7 eq8 eq9 =
    (g.view, g.view) $$ eq1 $$ eq2 $$ eq3 $$ eq4 $$ eq5 $$ eq6 $$ eq7 $$
    eq8 $$ eq9 |> go
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

  let g_cmp (vs1, vm1) (vs2, vm2) =
    let open V in
    let rec cmp a b =
      let c = index (vm1 a) - index (vm2 b) in
      if c = 0 then go (vs1 a) (vs2 b) else if c < 0 then -1 else 1
    and go: 'a 'b. ('a, _, _) spine -> ('b, _, _) spine -> int =
      fun s1 s2 -> match s1, s2 with
    | A (s1, a, f), A (s2, b, r) ->
      ( match go s1 s2 with 0 -> Cmp1.(!:f).cmp a Cmp2.(!:r) b | c -> c )
    | R (s1, x1), R (s2, x2) ->
      ( match go s1 s2 with 0 -> cmp x1 x2 | c -> c )
    | K _, K _-> 0
    | _ -> err_spine () in
    cmp

  type p = Cmp1.p
  type q = Cmp2.p

  let ($$) (view1, view2) f =
    let (f1, f2) = lift f in view1 Cmp1.(!f1), view2 Cmp2.(!f2)
  let go (v1, v2) = g_cmp v1 v2

  let data0 (g: _ data0) =
    (g.view, g.view) |> go
  let data1 (g: _ data1) cmp1 =
    (g.view, g.view) $$ cmp1 |> go
  let data2 (g: _ data2) cmp1 cmp2 =
    (g.view, g.view) $$ cmp1 $$ cmp2 |> go
  let data3 (g: _ data3) cmp1 cmp2 cmp3 =
    (g.view, g.view) $$ cmp1 $$ cmp2 $$ cmp3 |> go
  let data4 (g: _ data4) cmp1 cmp2 cmp3 cmp4 =
    (g.view, g.view) $$ cmp1 $$ cmp2 $$ cmp3 $$ cmp4 |> go
  let data5 (g: _ data5) cmp1 cmp2 cmp3 cmp4 cmp5 =
    (g.view, g.view) $$ cmp1 $$ cmp2 $$ cmp3 $$ cmp4 $$ cmp5 |> go
  let data6 (g: _ data6) cmp1 cmp2 cmp3 cmp4 cmp5 cmp6 =
    (g.view, g.view) $$ cmp1 $$ cmp2 $$ cmp3 $$ cmp4 $$ cmp5 $$ cmp6 |> go
  let data7 (g: _ data7) cmp1 cmp2 cmp3 cmp4 cmp5 cmp6 cmp7 =
    (g.view, g.view) $$ cmp1 $$ cmp2 $$ cmp3 $$ cmp4 $$ cmp5 $$ cmp6 $$ cmp7 |> go
  let data8 (g: _ data8) cmp1 cmp2 cmp3 cmp4 cmp5 cmp6 cmp7 cmp8 =
    (g.view, g.view) $$ cmp1 $$ cmp2 $$ cmp3 $$ cmp4 $$ cmp5 $$ cmp6 $$ cmp7 $$
    cmp8 |> go
  let data9 (g: _ data9) cmp1 cmp2 cmp3 cmp4 cmp5 cmp6 cmp7 cmp8 cmp9 =
    (g.view, g.view) $$ cmp1 $$ cmp2 $$ cmp3 $$ cmp4 $$ cmp5 $$ cmp6 $$ cmp7 $$
    cmp8 $$ cmp9 |> go
end

module Iter = struct
  module G = Generic (struct type 'a q = 'a -> unit end)
  open G
  open V
  let g_iter (vf, _) x =
    let rec go: 'a. ('a, _, _) spine -> _ = fun s v -> match s with
    | K _ -> ()
    | A (s, a, f) -> go s v; !:f a
    | R (s, a) -> go s v; go (v a) v in
    go (vf x) vf
  include P
  include View (struct type 'a r = 'a -> unit let gfun = g_iter end)
end

module Endo = struct
  module G = Generic (struct type 'a q = 'a -> 'a end)
  open G
  open V
  let g_endo (vf, _) x =
    let rec go: 'a. ('a, _, _) spine -> _ -> 'a = fun s v -> match s with
    | K f -> f
    | A (A (A (s, a, f), b, g), c, h) -> go s v (!:f a) (!:g b) (!:h c)
    | A (A (s, a, f), b, g) -> go s v (!:f a) (!:g b)
    | A (s, a, f) -> go s v (!:f a)
    | R (s, a) -> go s v (go (v a) v) in
    go (vf x) vf
  include P
  include View (struct type 'a r = 'a -> 'a let gfun = g_endo end)
end

module Fold = struct
  open V
  module Make (T: sig type t end) = struct
    module G = Generic (struct type 'a q = 'a -> T.t -> T.t end)
    open G
    let g_fold (vf, _) x t =
      let rec go: 'a. ('a, _, _) spine -> _ = fun s t v -> match s with
      | K _ -> t
      | A (s, a, f) -> go s (!:f a t) v
      | R (s, a) -> go s (go (v a) t v) v in
      go (vf x) t vf
    include P
  end
  type ('a, 'x) t = 'a -> 'x -> 'x
  let data0 (type a) (g: _ data0) =
    let module M = Make (struct type t = a end) in M.(G.app0 g_fold g.view)
  let data1 (type a) (g: _ data1) =
    let module M = Make (struct type t = a end) in M.(G.app1 g_fold g.view)
  let data2 (type a) (g: _ data2) =
    let module M = Make (struct type t = a end) in M.(G.app2 g_fold g.view)
  let data3 (type a) (g: _ data3) =
    let module M = Make (struct type t = a end) in M.(G.app3 g_fold g.view)
  let data4 (type a) (g: _ data4) =
    let module M = Make (struct type t = a end) in M.(G.app4 g_fold g.view)
  let data5 (type a) (g: _ data5) =
    let module M = Make (struct type t = a end) in M.(G.app5 g_fold g.view)
  let data6 (type a) (g: _ data6) =
    let module M = Make (struct type t = a end) in M.(G.app6 g_fold g.view)
  (* let data7 (type xxx) (omg: _ data7) = *)
  (*   let module M = Make (struct type t = xxx end) in M.(G.app7 g_fold omg.view) *)
  (* let data8 (type a) (g: _ data8) = *)
  (*   let module M = Make (struct type t = a end) in M.(G.app8 g_fold g.view) *)
  (* let data9 (type a) (g: _ data9) = *)
  (*   let module M = Make (struct type t = a end) in M.(G.app9 g_fold g.view) *)
end

module Random = struct
  type 'a gen = unit -> 'a
  module G = Generic (struct type 'a q = 'a gen end)
  open S
  open G
  let g_random sc =
    let rec go: 'a. ('a -> _) -> ('a, _, _) spine -> _ = fun k -> function
    | K f -> fun () -> k f
    | A (s, a) -> go (fun f -> k (f (!:a ()))) s
    | R s -> go (fun f -> k (f (gen ()))) s
    and fs = lazy (
      Array.of_list (List.map (fun (s, _) -> go (fun x -> x) s) sc))
    and gen () =
      let fs = Lazy.force fs in
      fs.(Random.int (Array.length fs)) () in
    gen
  include P
  include Schema (struct type 'a r = 'a q let gfun = g_random end)
end
