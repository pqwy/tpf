(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(** Generic printers.

    This modules depends on [fmt]. *)

open Tpf

(** {1 Generic printer} *)

type sum = string -> unit Fmt.t list Fmt.t
(** [sum name args] formats a variant with constructor [name] and
    argument formatters [args]. *)

type record = string option -> (string * unit Fmt.t) list Fmt.t
(** [record None fields] formats a plain constructorless record;
    [record (Some name) fields] formats an inline record with constructor
    [name]. [args] are argument formatters paired with field names. *)

include P with type 'a q := 'a Fmt.t

val g_pp : ?sum:sum -> ?record:record -> ('a, p) view -> 'a Fmt.t
(** [g_pp ~sum ~record v ppf x] uses [v] to view [x] and pretty-print it on
    [ppf].

    [sum] controls the formatting of plain variants, [record] of records.
    Defaults match the way toplevel prints values. *)

(** {1 [data] interface} *)

(* Note - this blaaargh can be replaced by including
   [Data with type 'a r := ?sum -> ?record -> 'a Fmt.t] but we pull the optional
   arguments to the beginning for a nicer API. *)

val data0 : ?sum:sum -> ?record:record ->
            'a data0 ->
            'a Fmt.t
val data1 : ?sum:sum -> ?record:record ->
            ('a, 'b) data1 ->
            'a Fmt.t -> 'b Fmt.t
val data2 : ?sum:sum -> ?record:record ->
            ('a, 'b, 'c) data2 ->
            'a Fmt.t -> 'b Fmt.t -> 'c Fmt.t
val data3 : ?sum:sum -> ?record:record ->
            ('a, 'b, 'c, 'd) data3 ->
            'a Fmt.t -> 'b Fmt.t -> 'c Fmt.t -> 'd Fmt.t
val data4 : ?sum:sum -> ?record:record ->
            ('a, 'b, 'c, 'd, 'e) data4 ->
            'a Fmt.t -> 'b Fmt.t -> 'c Fmt.t -> 'd Fmt.t -> 'e Fmt.t
val data5 : ?sum:sum -> ?record:record ->
            ('a, 'b, 'c, 'd, 'e, 'f) data5 ->
            'a Fmt.t -> 'b Fmt.t -> 'c Fmt.t -> 'd Fmt.t -> 'e Fmt.t -> 'f Fmt.t
val data6 : ?sum:sum -> ?record:record ->
            ('a, 'b, 'c, 'd, 'e, 'f, 'g) data6 ->
            'a Fmt.t -> 'b Fmt.t -> 'c Fmt.t -> 'd Fmt.t -> 'e Fmt.t ->
            'f Fmt.t -> 'g Fmt.t
val data7 : ?sum:sum -> ?record:record ->
            ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) data7 ->
            'a Fmt.t -> 'b Fmt.t -> 'c Fmt.t -> 'd Fmt.t -> 'e Fmt.t ->
            'f Fmt.t -> 'g Fmt.t -> 'h Fmt.t
val data8 : ?sum:sum -> ?record:record ->
            ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i) data8 ->
            'a Fmt.t -> 'b Fmt.t -> 'c Fmt.t -> 'd Fmt.t -> 'e Fmt.t ->
            'f Fmt.t -> 'g Fmt.t -> 'h Fmt.t -> 'i Fmt.t
val data9 : ?sum:sum -> ?record:record ->
            ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j) data9 ->
            'a Fmt.t -> 'b Fmt.t -> 'c Fmt.t -> 'd Fmt.t -> 'e Fmt.t ->
            'f Fmt.t -> 'g Fmt.t -> 'h Fmt.t -> 'i Fmt.t -> 'j Fmt.t
