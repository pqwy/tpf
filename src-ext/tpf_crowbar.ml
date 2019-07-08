(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Crowbar

include Tpf_std.AppS (struct
  type 'a t = 'a gen
  let pure = const
  let app f a = map [f; a] (@@)
  let retract = unlazy
  let gfun gens = choose (List.map (fun (g, _) -> unlazy g) gens)
end)
let g_gen = gfun
