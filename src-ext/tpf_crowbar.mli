(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(** Generic Crowbar.

    This module provides generic {!Crowbar.gen} and depends on [crowbar].

    It combines nicely with with {{!Tpf_fmt}generic printing} and
    {{!Tpf_std.Eq}generic equality}. *)

open Tpf

(** {1 Generic [gen]} *)
include P with type 'a q := 'a Crowbar.gen
val g_gen : ('a, p) schema -> 'a Crowbar.gen

(** {1 [data] interface} *)
include Data with type 'a q := 'a Crowbar.gen and type 'a r := 'a Crowbar.gen
