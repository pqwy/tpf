open Tpf
open Cmdliner

let invalid_arg fmt = Format.kasprintf invalid_arg fmt
let err_recursive () =
  invalid_arg "Tpf_cmdliner: recursive types are not supported"
let err_no_fields m =
  invalid_arg "Tpf_cmdliner: `%s': not a record" m.name
let err_fields () =
  invalid_arg "Tpf_cmdliner: `%s': fields mismatch" "XXX"
let err_not_singleton () =
  invalid_arg "Tpf_cmdliner: expecting a type with one constructor."

module Opt = struct
  include Generic (struct type 'a r = 'a Arg.conv end)
  open S
  let rec term: 'a. ('a, _, _) spine -> 'a Term.t * _ = function
  | K (f, m) ->
    ( match m.fields with
      | [] -> err_no_fields m
      | fields -> Term.const f, fields )
  | R _ -> err_recursive ()
  | A (s, a) ->
      match term s with
      | _, [] -> err_fields ()
      | t, f::fs ->
          let nfo = Arg.info [f] in
          Term.(t $ Arg.(required (opt (some !a) None nfo))), fs
  let f = function
  | [s] -> ( match term s with t, [] -> t | _ -> err_fields () )
  | _ -> err_not_singleton ()
  let g0 g = s0 f g
  let g1 g = s1 f g
  let g2 g = s2 f g
  let g3 g = s3 f g
  let g4 g = s4 f g
  let g5 g = s5 f g
  let g6 g = s6 f g
  let g7 g = s7 f g
  let g8 g = s8 f g
  let g9 g = s9 f g
end

module Opt2 = struct
  include Generic (struct type 'a r = 'a Arg.conv end)
  open V
  let rec term: 'a. ('a, _, _) spine -> _ -> 'a Term.t =
    fun s fs -> match s, fs with
  | K (f, _), [] -> Term.const f
  | A (s, a, f), f0::fs ->
      Term.(term s fs $ Arg.(value @@ opt !f a @@ info [f0]))
  | R _, _ -> err_recursive ()
  | _ -> err_fields ()
  let f v x =
    let s = v x in
    let m = v_meta s in
    match m.fields with
    | [] -> err_no_fields m
    | fs -> term s (List.rev fs)
  let g0 g = v0 f g
  let g1 g = v1 f g
  let g2 g = v2 f g
  let g3 g = v3 f g
  let g4 g = v4 f g
  let g5 g = v5 f g
  let g6 g = v6 f g
  let g7 g = v7 f g
  let g8 g = v8 f g
  let g9 g = v9 f g
end

module Pos = struct
  include Generic (struct type 'a r = 'a Arg.conv end)
  open S
  let rec term: 'a. _ -> ('a, _, _) spine -> 'a Term.t * _ =
  fun i -> function
  | K (f, _) -> Term.const f, i
  | R _ -> err_recursive ()
  | A (s, a) ->
      let t, i = term i s in
      let nfo = Arg.info [] in
      Term.(t $ Arg.(required (pos i (some !a) None nfo))), i + 1
  let f i = function
  | [s] -> term i s
  | _ -> err_not_singleton ()
  let g0 g i = s0 (f i) g
  let g1 g i = s1 (f i) g
  let g2 g i = s2 (f i) g
  let g3 g i = s3 (f i) g
  let g4 g i = s4 (f i) g
  let g5 g i = s5 (f i) g
  let g6 g i = s6 (f i) g
  let g7 g i = s7 (f i) g
  let g8 g i = s8 (f i) g
  let g9 g i = s9 (f i) g
end
