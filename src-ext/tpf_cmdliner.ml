(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Tpf
open Cmdliner

let invalid_arg fmt = Format.kasprintf invalid_arg fmt
let err_recursive () =
  invalid_arg "Tpf_cmdliner: recursive types are not supported"
let err_no_labels =
  invalid_arg "Tpf_cmdliner: `%a': not a record" pp_meta
let err_not_singleton () =
  invalid_arg "Tpf_cmdliner: expecting a type with one constructor."

module G = Generic (struct type 'a q = 'a Arg.conv end)
open G
include G.P

module Opt = struct
  open S
  let g = function
  | [s, m] ->
      let rec term: 'a. _ -> ('a, _, _) spine -> 'a Term.t = fun i -> function
      | K k -> Term.const k
      | R _ -> err_recursive ()
      | A (s, a) ->
          let nfo = Arg.info [label m i] in
          let arg = Arg.(required @@ opt (some !:a) None nfo) in
          Term.(term (i - 1) s $ arg) in
      term (labels m - 1) s
  | _ -> err_not_singleton ()
  include Schema (struct type 'a r = 'a Term.t let gfun = g end)
end

module Opt_def = struct
  open V
  let g v x =
    let m = meta v x in
    match labels m with
    | 0 -> err_no_labels m
    | _ ->
        let rec term: 'a. _ -> ('a, _, _) spine -> 'a Term.t = fun i -> function
        | K k -> Term.const k
        | R _ -> err_recursive ()
        | A (s, a, f) ->
            let nfo = Arg.info [label m i] in
            Term.(term (i - 1) s $ Arg.(value @@ opt !:f a nfo)) in
        term (labels m - 1) (spine v x)
  include View (struct type 'a r = 'a -> 'a Term.t let gfun = g end)
end

module Pos = struct
  open S
  let g = function
  | [s, _] ->
      fun i ->
        let rec term: 'a. ('a, _, _) spine -> 'a Term.t * _ = function
        | K k -> Term.const k, i
        | R _ -> err_recursive ()
        | A (s, a) ->
            let t, i = term s in
            Term.(t $ Arg.(required @@ pos i (some !:a) None @@ info [])), i + 1
        in
        term s
  | _ -> err_not_singleton ()
  include Schema (struct type 'a r = int -> 'a Term.t * int let gfun = g end)
end

module Pos_def = struct
  open V
  let g v x i =
    let rec term: 'a. ('a, _, _) spine -> 'a Term.t * _ = function
    | K k -> Term.const k, i
    | R _ -> err_recursive ()
    | A (s, a, f) ->
        let t, i = term s in
        Term.(t $ Arg.(value @@ pos i !:f a @@ info [])), i + 1 in
    term (spine v x)
  include View (struct
    type 'a r = 'a -> int -> 'a Term.t * int
    let gfun = g
  end)
end
