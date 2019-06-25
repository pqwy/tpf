(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(** Generic printer *)

open Tpf

include P with type 'a q := 'a Fmt.t
val g_pp: ('a, p) Tpf.view -> 'a Fmt.t
(** [g_pp v ppf x] pretty-prints [x], viewed by [v], on [ppf], mimicking the
    toplevel printer. *)

include Data with type 'a q := 'a Fmt.t and type 'a r := 'a Fmt.t
