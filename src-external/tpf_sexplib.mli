open Tpf

type 'a e = 'a -> Sexplib.Sexp.t

val g_to_sexp0: 'x g0 -> 'x e
val g_to_sexp1: ('a, 'x) g1 -> 'a e -> 'x e
val g_to_sexp2: ('a, 'b, 'x) g2 -> 'a e -> 'b e -> 'x e
val g_to_sexp3: ('a, 'b, 'c, 'x) g3 -> 'a e -> 'b e -> 'c e -> 'x e
val g_to_sexp4: ('a, 'b, 'c, 'd, 'x) g4 -> 'a e -> 'b e -> 'c e -> 'd e -> 'x e
val g_to_sexp5: ('a, 'b, 'c, 'd, 'e, 'x) g5 -> 'a e -> 'b e -> 'c e -> 'd e ->
                'e e -> 'x e
val g_to_sexp6: ('a, 'b, 'c, 'd, 'e, 'f, 'x) g6 -> 'a e -> 'b e -> 'c e ->
                'd e -> 'e e -> 'f e ->'x e
val g_to_sexp7: ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'x) g7 -> 'a e -> 'b e -> 'c e ->
                'd e -> 'e e -> 'f e -> 'g e -> 'x e
val g_to_sexp8: ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'x) g8 -> 'a e -> 'b e ->
                'c e -> 'd e -> 'e e -> 'f e -> 'g e -> 'h e -> 'x e
val g_to_sexp9: ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'x) g9 -> 'a e -> 'b e ->
                'c e -> 'd e -> 'e e -> 'f e -> 'g e -> 'h e -> 'i e -> 'x e

type 'a d = Sexplib.Sexp.t -> 'a

val g_of_sexp0: 'x g0 -> 'x d
val g_of_sexp1: ('a, 'x) g1 -> 'a d -> 'x d
val g_of_sexp2: ('a, 'b, 'x) g2 -> 'a d -> 'b d -> 'x d
val g_of_sexp3: ('a, 'b, 'c, 'x) g3 -> 'a d -> 'b d -> 'c d -> 'x d
val g_of_sexp4: ('a, 'b, 'c, 'd, 'x) g4 -> 'a d -> 'b d -> 'c d -> 'd d -> 'x d
val g_of_sexp5: ('a, 'b, 'c, 'd, 'e, 'x) g5 -> 'a d -> 'b d -> 'c d -> 'd d ->
                'e d -> 'x d
val g_of_sexp6: ('a, 'b, 'c, 'd, 'e, 'f, 'x) g6 -> 'a d -> 'b d -> 'c d ->
                'd d -> 'e d -> 'f d ->'x d
val g_of_sexp7: ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'x) g7 -> 'a d -> 'b d -> 'c d ->
                'd d -> 'e d -> 'f d -> 'g d -> 'x d
val g_of_sexp8: ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'x) g8 -> 'a d -> 'b d ->
                'c d -> 'd d -> 'e d -> 'f d -> 'g d -> 'h d -> 'x d
val g_of_sexp9: ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'x) g9 -> 'a d -> 'b d ->
                'c d -> 'd d -> 'e d -> 'f d -> 'g d -> 'h d -> 'i d -> 'x d
