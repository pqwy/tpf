open Tpf

let c0 = variant "()" 0
let k0 = V.K ((), c0)
let unit : _ g0 = { view = (fun () -> k0); schema = S.[ K ((), c0) ] }

let pair a b = a, b
let c0 = variant "(,)" 0
let k0 = V.K (pair, c0)
let view a b (x, y) = V.(A (A (k0, x, a), y, b))
let schema a b = S.[ A (A (K (pair, c0), a), b) ]
let pair : _ g2 = { view; schema }

let cons x xs = x :: xs
let c0 = variant "[]" 0 and c1 = variant "(::)" 1
let k0 = V.K ([], c0) and k1 = V.K (cons, c1)
let view a = V.(function
| [] -> k0
| x::xs -> R (A (k1, x, a), xs))
let schema a = S.[ K ([], c0); R (A (K (cons, c1), a)) ]
let list : _ g1 = { view; schema }

let scons x xs () = Seq.Cons (x, xs)
let c0 = variant "Nil" 0 and c1 = variant "Cons" 1
let k0 = V.K (Seq.empty, c0) and k1 = V.K (scons, c1)
let view a s = V.(match s () with
| Seq.Nil -> k0
| Seq.Cons (x, s) -> R (A (k1, x, a), s))
let schema a = S.[ K (Seq.empty, c0); R (A (K (scons, c1), a)) ]
let seq: _ g1 = { view; schema }

let some x = Some x
let c0 = variant "None" 0 and c1 = variant "Some" 1
let k0 = V.K (None, c0) and k1 = V.K (some, c1)
let view a = V.(function Some x -> A (k1, x, a) | _ -> k0)
let schema a = S.[ K (None, c0); A (K (some, c1), a) ]
let option: _ g1 = { view; schema }

let ok x = Ok x and error x = Error x
let c0 = variant "Ok" 0 and c1 = variant "Error" 1
let k0 = V.K (ok, c0) and k1 = V.K (error, c1)
let view a b = V.(function Ok x -> A (k0, x, a) | Error y -> A (k1, y, b))
let schema a b = S.[ A (K (ok, c0), a); A (K (error, c1), b) ]
let result: _ g2 = { view; schema }
