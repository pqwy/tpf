(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Tpf
open QCheck

module G = Generic (struct type 'a q = 'a arbitrary end)
open G

let small (vf, _) x =
  let small arb a = match arb.small with Some f -> f a | _ -> 1 in
  let rec go: 'a. ('a, _, _) V.spine -> _ = function
  | K _ -> 1
  | A (s, a, f) -> go s + small !:f a
  | R (s, a) -> go s + go (vf a) in
  go (vf x)

let g_shrink (vf, _) x i =
  let shrink arb a i = match arb.shrink with Some f -> f a i | _ -> i a in
  let rec go: 'a. ('a, _, _) V.spine -> 'a Iter.t = fun s i -> match s with
  | V.K x -> i x
  | V.A (s, a, f) -> go s (fun x -> shrink !:f a (fun a -> i (x a)))
  | R (s, a) -> go s (fun x -> go (vf a) (fun a -> i (x a))) in
  go (vf x) i

let gen s =
  Tpf_std.(Random.(g_gen (smap { nat = fun arb -> !(!:arb.gen) } s)))

let pp v =
  let to_string arb a = match arb.print with Some f -> f a | _ -> "<?>" in
  let nat arb = Tpf_fmt.(!(fun ppf v -> Fmt.string ppf (to_string !:arb v))) in
  Tpf_fmt.g_pp (Tpf_std.vmap { nat } v)

let g_arb v schema ?base size =
  make (gen schema ?base size)
  ~small:(small v) ~shrink:(g_shrink v)
  ~print:(pp v |> Fmt.to_to_string)

include P

let data0 (data: _ data0) =
  g_arb data.view data.schema
let data1 (data: _ data1) a =
  g_arb (data.view !a) (data.schema !a)
let data2 (data: _ data2) a b =
  g_arb (data.view !a !b) (data.schema !a !b)
let data3 (data: _ data3) a b c =
  g_arb (data.view !a !b !c) (data.schema !a !b !c)
let data4 (data: _ data4) a b c d =
  g_arb (data.view !a !b !c !d) (data.schema !a !b !c !d)
let data5 (data: _ data5) a b c d e =
  g_arb (data.view !a !b !c !d !e) (data.schema !a !b !c !d !e)
let data6 (data: _ data6) a b c d e f =
  g_arb (data.view !a !b !c !d !e !f) (data.schema !a !b !c !d !e !f)
let data7 (data: _ data7) a b c d e f g =
  g_arb (data.view !a !b !c !d !e !f !g) (data.schema !a !b !c !d !e !f !g)
let data8 (data: _ data8) a b c d e f g h =
  g_arb (data.view !a !b !c !d !e !f !g !h) (data.schema !a !b !c !d !e !f !g !h)
let data9 (data: _ data9) a b c d e f g h i =
  g_arb (data.view !a !b !c !d !e !f !g !h !i) (data.schema !a !b !c !d !e !f !g !h !i)
