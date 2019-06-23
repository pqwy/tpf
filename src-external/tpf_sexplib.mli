open Tpf

module Sexp_to : sig
  type 'a e = 'a -> Sexplib.Sexp.t
  type p
  val (!:) : 'a e -> ('a, p) app
  val gfun : ('a, p) view -> 'a e
  include (S with type 'a q := 'a e and type 'a r := 'a e)
end

module Sexp_of : sig
  type 'a d = Sexplib.Sexp.t -> 'a
  type p
  val (!:) : 'a d -> ('a, p) app
  val gfun : ('a, p) schema -> 'a d
  include (S with type 'a q := 'a d and type 'a r := 'a d)
end
