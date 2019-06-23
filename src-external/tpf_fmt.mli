open Tpf

type p
val (!:) : 'a Fmt.t -> ('a, p) app
val g_pp: ('a, p) Tpf.view -> 'a Fmt.t
include S with type 'a q := 'a Fmt.t and type 'a r := 'a Fmt.t
