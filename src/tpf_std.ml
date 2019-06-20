open Tpf

(* Stdlib generics *)

let m0 = variant "()" 0
let k0 = V.K ((), m0)
let unit: _ g0 = { view = (fun () -> k0); schema = S.[ K ((), m0) ] }

let pair a b = a, b
let m0 = variant "(,)" 0
let k0 = V.K (pair, m0)
let pair: _ g2 =
  { view = V.(fun a b (x, y) -> A (A (k0, x, a), y, b))
  ; schema = fun a b -> S.[ A (A (K (pair, m0), a), b) ] }

let triple a b c = a, b, c
let m0 = variant "(,,)" 0
let k0 = V.K (triple, m0)
let triple: _ g3 =
  { view = V.(fun a b c (x, y, z) -> A (A (A (k0, x, a), y, b), z, c))
  ; schema = fun a b c -> S.[ A (A (A (K (triple, m0), a), b), c) ] }

let quadruple a b c d = a, b, c, d
let m0 = variant "(,,,)" 0
let k0 = V.K (quadruple, m0)
let quadruple: _ g4 =
  { view = V.(fun a b c d (x, y, z, w) ->
      A (A (A (A (k0, x, a), y, b), z, c), w, d))
  ; schema = fun a b c d ->
      S.[ A (A (A (A (K (quadruple, m0), a), b), c), d) ] }

let cons x xs = x :: xs
let m0 = variant "[]" 0 and m1 = variant "(::)" 1
let k0 = V.K ([], m0) and k1 = V.K (cons, m1)
let view a = V.(function
| [] -> k0
| x::xs -> R (A (k1, x, a), xs))
let schema a = S.[ K ([], m0); R (A (K (cons, m1), a)) ]
let list: _ g1 = { view; schema }

let scons x xs () = Seq.Cons (x, xs)
let m0 = variant "Nil" 0 and m1 = variant "Cons" 1
let k0 = V.K (Seq.empty, m0) and k1 = V.K (scons, m1)
let view a s = V.(match s () with
| Seq.Nil -> k0
| Seq.Cons (x, s) -> R (A (k1, x, a), s))
let schema a = S.[ K (Seq.empty, m0); R (A (K (scons, m1), a)) ]
let seq: _ g1 = { view; schema }

let some x = Some x
let m0 = variant "None" 0 and m1 = variant "Some" 1
let k0 = V.K (None, m0) and k1 = V.K (some, m1)
let view a = V.(function Some x -> A (k1, x, a) | _ -> k0)
let schema a = S.[ K (None, m0); A (K (some, m1), a) ]
let option: _ g1 = { view; schema }

let ok x = Ok x and error x = Error x
let m0 = variant "Ok" 0 and m1 = variant "Error" 1
let k0 = V.K (ok, m0) and k1 = V.K (error, m1)
let view a b = V.(function Ok x -> A (k0, x, a) | Error y -> A (k1, y, b))
let schema a b = S.[ A (K (ok, m0), a); A (K (error, m1), b) ]
let result: _ g2 = { view; schema }

(* Equality *)

type _ ttag = ..

type 'a eq = 'a -> 'a -> bool
type 'a jmeq = { eq: 'b. 'a -> 'b ttag -> 'b -> bool }

let jmeq (type a) (eq: a -> a -> bool) =
  let module M = struct type _ ttag += K : a ttag end in
  let eq (type b) a (t: b ttag) (b: b) =
    match t with M.K -> eq a b | _ -> false in
  { eq }, M.K

module Eq1 = Generic (struct type 'a r = 'a jmeq end)
module Eq2 = Generic (struct type 'a r = 'a ttag end)

let geq v1 v2 a b =
  let rec go: 'a 'b. ('a, _, _) V.spine -> ('b, _, _) V.spine -> bool =
  V.( fun s1 s2 -> match s1, s2 with
    | K (_, m1)   , K (_, m2)    -> m1.index = m2.index
    | A (s1, a, f), A (s2, b, g) -> go s1 s2 && Eq1.(!!f).eq a Eq2.(!!g) b
    | R (s1, x1)  , R (s2, x2)   -> go s1 s2 && go (v1 x1) (v2 x2)
    | _           , _            -> false ) in
  go (v1 a) (v2 b)

let ($$) (view1, view2) f =
  let (f1, f2) = jmeq f in Eq1.(view1 $ f1), Eq2.(view2 $ f2)
let go (v1, v2) = geq v1 v2

let eq0 (g: _ g0) =
  (g.view, g.view) |> go
let eq1 (g: _ g1) eq1 =
  (g.view, g.view) $$ eq1 |> go
let eq2 (g: _ g2) eq1 eq2 =
  (g.view, g.view) $$ eq1 $$ eq2 |> go
let eq3 (g: _ g3) eq1 eq2 eq3 =
  (g.view, g.view) $$ eq1 $$ eq2 $$ eq3 |> go
let eq4 (g: _ g4) eq1 eq2 eq3 eq4 =
  (g.view, g.view) $$ eq1 $$ eq2 $$ eq3 $$ eq4 |> go
let eq5 (g: _ g5) eq1 eq2 eq3 eq4 eq5 =
  (g.view, g.view) $$ eq1 $$ eq2 $$ eq3 $$ eq4 $$ eq5 |> go
let eq6 (g: _ g6) eq1 eq2 eq3 eq4 eq5 eq6 =
  (g.view, g.view) $$ eq1 $$ eq2 $$ eq3 $$ eq4 $$ eq5 $$ eq6 |> go
let eq7 (g: _ g7) eq1 eq2 eq3 eq4 eq5 eq6 eq7 =
  (g.view, g.view) $$ eq1 $$ eq2 $$ eq3 $$ eq4 $$ eq5 $$ eq6 $$ eq7 |> go
let eq8 (g: _ g8) eq1 eq2 eq3 eq4 eq5 eq6 eq7 eq8 =
  (g.view, g.view) $$ eq1 $$ eq2 $$ eq3 $$ eq4 $$ eq5 $$ eq6 $$ eq7 $$
  eq8 |> go
let eq9 (g: _ g9) eq1 eq2 eq3 eq4 eq5 eq6 eq7 eq8 eq9 =
  (g.view, g.view) $$ eq1 $$ eq2 $$ eq3 $$ eq4 $$ eq5 $$ eq6 $$ eq7 $$
  eq8 $$ eq9 |> go

(* Comparison *)

type 'a cmp = 'a -> 'a -> int
type 'a jmcmp = { cmp: 'b. 'a -> 'b ttag -> 'b -> int }

let lift (type a) (compare: a -> a -> int) =
  let module M = struct type _ ttag += K : a ttag end in
  let cmp (type b): _ -> b ttag -> b -> _ =
    fun a r b -> match r with M.K -> compare a b | _ -> 0 in
  { cmp }, M.K

module Cmp1 = Generic (struct type 'a r = 'a jmcmp end)
module Cmp2 = Generic (struct type 'a r = 'a ttag end)

let gcompare v1 v2 a b =
  let rec go: 'a 'b. ('a, _, _) V.spine -> ('b, _, _) V.spine -> int =
  V.( fun s1 s2 -> match s1, s2 with
    | K (_, m1)   , K (_, m2)    -> m1.index - m2.index
    | A (s1, a, f), A (s2, b, r) ->
      ( match go s1 s2 with 0 -> Cmp1.(!!f).cmp a Cmp2.(!!r) b | c -> c )
    | R (s1, x1)  , R (s2, x2)   ->
      ( match go s1 s2 with 0 -> go (v1 x1) (v2 x2) | c -> c )
    | A (s1, _, _), _            -> go s1 s2
    | R (s1, _)   , _            -> go s1 s2
    | _           , A (s2, _, _) -> go s1 s2
    | _           , R (s2, _)    -> go s1 s2 ) in
  match go (v1 a) (v2 b) with 0 -> 0 | c when c < 0 -> -1 | _ -> 1

let ($$) (view1, view2) f =
  let (f1, f2) = lift f in Cmp1.(view1 $ f1), Cmp2.(view2 $ f2)
let go (v1, v2) = gcompare v1 v2

let cmp0 (g: _ g0) =
  (g.view, g.view) |> go
let cmp1 (g: _ g1) cmp1 =
  (g.view, g.view) $$ cmp1 |> go
let cmp2 (g: _ g2) cmp1 cmp2 =
  (g.view, g.view) $$ cmp1 $$ cmp2 |> go
let cmp3 (g: _ g3) cmp1 cmp2 cmp3 =
  (g.view, g.view) $$ cmp1 $$ cmp2 $$ cmp3 |> go
let cmp4 (g: _ g4) cmp1 cmp2 cmp3 cmp4 =
  (g.view, g.view) $$ cmp1 $$ cmp2 $$ cmp3 $$ cmp4 |> go
let cmp5 (g: _ g5) cmp1 cmp2 cmp3 cmp4 cmp5 =
  (g.view, g.view) $$ cmp1 $$ cmp2 $$ cmp3 $$ cmp4 $$ cmp5 |> go
let cmp6 (g: _ g6) cmp1 cmp2 cmp3 cmp4 cmp5 cmp6 =
  (g.view, g.view) $$ cmp1 $$ cmp2 $$ cmp3 $$ cmp4 $$ cmp5 $$ cmp6 |> go
let cmp7 (g: _ g7) cmp1 cmp2 cmp3 cmp4 cmp5 cmp6 cmp7 =
  (g.view, g.view) $$ cmp1 $$ cmp2 $$ cmp3 $$ cmp4 $$ cmp5 $$ cmp6 $$ cmp7 |> go
let cmp8 (g: _ g8) cmp1 cmp2 cmp3 cmp4 cmp5 cmp6 cmp7 cmp8 =
  (g.view, g.view) $$ cmp1 $$ cmp2 $$ cmp3 $$ cmp4 $$ cmp5 $$ cmp6 $$ cmp7 $$
  cmp8 |> go
let cmp9 (g: _ g9) cmp1 cmp2 cmp3 cmp4 cmp5 cmp6 cmp7 cmp8 cmp9 =
  (g.view, g.view) $$ cmp1 $$ cmp2 $$ cmp3 $$ cmp4 $$ cmp5 $$ cmp6 $$ cmp7 $$
  cmp8 $$ cmp9 |> go
