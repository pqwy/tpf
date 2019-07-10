(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Longident
open Asttypes
open Parsetree
open Ast_helper

[@@@ocaml.warning "-9"]

let raise_errorf = Ppx_deriving.raise_errorf
let (%) f g x = f (g x)

module Cmp = struct
  let lex cmp v1 v2 = function 0 -> cmp v1 v2 | r -> r
  let rec list cmp xs ys = match xs, ys with
  | x::xs, y::ys -> cmp x y |> lex (list cmp) xs ys
  | [], [] -> 0 | [], _ -> -1 | _, [] -> 1
  let rec core_type t1 t2 =
    let fv1 = Ppx_deriving.free_vars_in_core_type t1
    and fv2 = Ppx_deriving.free_vars_in_core_type t2 in
    let c = compare (List.length fv1) (List.length fv2) in
    if c <> 0 then c else
    match t1.ptyp_desc, t2.ptyp_desc with
    | Ptyp_object _, _    | _, Ptyp_object _
    | Ptyp_class _, _     | _, Ptyp_class _
    | Ptyp_variant _, _   | _, Ptyp_variant _
    | Ptyp_poly _, _      | _, Ptyp_poly _
    | Ptyp_package _, _   | _, Ptyp_package _
    | Ptyp_extension _, _ | _, Ptyp_extension _ ->
        invalid_arg "FIXME: add support"
    | Ptyp_any, Ptyp_any -> 0
    | Ptyp_any, _ -> -1
    | Ptyp_var v1, Ptyp_var v2 -> compare v1 v2
    | Ptyp_var _, Ptyp_any -> 1
    | Ptyp_var _, _ -> -1
    | Ptyp_arrow (l1, a1, b1), Ptyp_arrow (l2, a2, b2) ->
        compare l1 l2 |> lex core_type a1 a2 |> lex core_type b1 b2
    | Ptyp_arrow _, (Ptyp_any | Ptyp_var _) -> 1
    | Ptyp_arrow _, _ -> -1
    | Ptyp_tuple t1, Ptyp_tuple t2 -> list core_type t1 t2
    | Ptyp_tuple _, (Ptyp_any | Ptyp_var _ | Ptyp_arrow _) -> 1
    | Ptyp_tuple _, _ -> -1
    | Ptyp_constr (id1, xs), Ptyp_constr (id2, ys) ->
        compare id1.txt id2.txt |> lex (list core_type) xs ys
    | Ptyp_constr _, (Ptyp_any | Ptyp_var _ | Ptyp_arrow _ | Ptyp_tuple _) -> 1
    | Ptyp_constr _, _ -> -1
    | Ptyp_alias (t1, _), Ptyp_alias (t2, _) -> core_type t1 t2
    | Ptyp_alias _,
      (Ptyp_any | Ptyp_var _ | Ptyp_arrow _ | Ptyp_tuple _ | Ptyp_constr _) -> 1
end

let lid ?(path = []) str =
  let txt = match path @ [str] with
  | x::xs -> List.fold_left (fun lid x -> Ldot (lid, x)) (Lident x) xs
  | [] -> assert false in
  { loc = !Ast_helper.default_loc; txt }
let sid str = { txt = str; loc = !Ast_helper.default_loc }
let to_lid sid = { txt = Lident sid.txt; loc = !Ast_helper.default_loc }

module Smap = Map.Make (struct type t = string let compare = String.compare end)
let sym_cnt = ref Smap.empty
let sym ?(root = "x") () =
  let i = match Smap.find_opt root !sym_cnt with Some i -> i | _ -> 0 in
  sym_cnt := Smap.add root (i + 1) !sym_cnt;
  root ^ "_" ^ string_of_int i |> sid

let cons fcons ftup ident = function
| [] -> fcons ident None
| [x] -> fcons ident (Some x)
| xs -> fcons ident (Some (ftup xs))
let construct = cons Exp.construct Exp.tuple
let pconstruct = cons Pat.construct Pat.tuple
let const_s = Exp.constant % Const.string
let const_i = Exp.constant % Const.int
let rec fun_ vars expr = match vars with
| [] -> expr
| v::vs -> Exp.fun_ Nolabel None (Pat.var v) (fun_ vs expr)

let const_list xs =
  let nil = lid "[]" and cons = lid "::" in
  List.fold_right (fun x xs -> construct cons [x; xs]) xs (construct nil [])

let structure f =
  let defs = ref [] in
  let g ?(gen = true) var expr =
    let id = if gen then sym ~root:var () else sid var in
    defs := Str.value Nonrecursive [Vb.mk (Pat.var id) expr] :: !defs;
    Exp.ident (to_lid id) in
  let () = f g in
  List.rev !defs

let iter_core_types f = function
| { ptype_kind = Ptype_variant consn } ->
    consn |> List.iter (function
    | { pcd_args = Pcstr_tuple cdecls } ->
        List.iter f cdecls
    | { pcd_args = Pcstr_record ldecls } ->
        List.iter (fun l -> f l.pld_type) ldecls)
| { ptype_kind = Ptype_record ldecls } ->
    List.iter (fun l -> f l.pld_type) ldecls
| { ptype_kind = (Ptype_abstract | Ptype_open) } -> ()

let troot = function
| { ptyp_desc = Ptyp_var v } -> v
| { ptyp_desc = Ptyp_constr ({ txt = (Lident v|Ldot (_, v)) }, _) } -> v
| _ -> "type"

module Tmap = Map.Make (struct type t = core_type let compare = Cmp.core_type end)

let core_types_in tdecl =
  let name = tdecl.ptype_name.txt
  and args = List.map fst tdecl.ptype_params in
  let recurs = function
  | { ptyp_desc = Ptyp_constr ({ txt = Lident name1 }, args1) } ->
      name = name1 && Cmp.(list core_type) args args1 = 0
  | _ -> false in
  let map = ref Tmap.empty in
  iter_core_types (fun t ->
    if not (recurs t || Tmap.mem t !map) then
      map := Tmap.add t (sym ~root:(troot t) ()) !map
  ) tdecl;
  let pick t =
    if recurs t then `Rec else
      `Id (Exp.ident (Tmap.find t !map |> to_lid)) in
  let tvs = Tmap.bindings !map in
  List.map fst tvs, List.map snd tvs, pick

let path = ["Tpf"; "V"]
let tpf_v_k = let id = lid ~path "K" in fun x -> construct id [x]
let tpf_v_r = let id = lid ~path "R" in fun s a -> construct id [s; a]
let tpf_v_a = let id = lid ~path "A" in fun s a f -> construct id [s; a; f]

let path = ["Tpf"; "S"]
let tpf_s_k = let id = lid ~path "K" in fun x -> construct id [x]
let tpf_s_r = let id = lid ~path "R" in fun s -> construct id [s]
let tpf_s_a = let id = lid ~path "A" in fun s f -> construct id [s; f]

let tpf_variant =
  let id = lid ~path:["Tpf"] "variant" in
  fun ?labels name i ->
  let labels = match labels with
  | Some fs -> [Labelled "labels", Exp.array (List.map const_s fs)]
  | None -> [] in
  Exp.(apply (ident id) ([Nolabel, i; Nolabel, name] @ labels))

let id_data n = lid ~path:["Tpf"] ("data" ^ string_of_int n)

let t_name t = t.ptype_name

let raise_error_t t fmt =
  raise_errorf ~loc:t.ptype_loc
  ("Cannot derive Tpf for %s: " ^^ fmt) t.ptype_name.txt
let assert_arity t types =
  if List.length types > 9 then
    raise_error_t t "it mentions more than 9 other types"

let data_defn_name type_name =
  Ppx_deriving.mangle_type_decl (`Prefix "data") type_name

let data_defn_type tdecl other_types =
  let arity = List.length other_types in
  let self = Typ.constr (t_name tdecl |> to_lid)
              (List.map fst tdecl.ptype_params) in
  Typ.constr (id_data arity) (other_types @ [self])

let labelled lbls xs =
  List.(combine (map (fun l -> to_lid l.pld_name) lbls) xs)

(* NB -- We don't even bother to reject GADTs as that goes well into semantic
   territory. Instead we let the typer bomb out later. *)

let str_of_type ~options:_ ~path:_ tdecl =
  (* parse/abort-on options *)
  let types, tyvars, tyref = core_types_in tdecl in
  assert_arity tdecl types;
  structure @@ fun defn ->
    let case ?labels ~pat ~ctor name i args =
      let argvars = List.map (fun _ -> sym ()) args in
      let argrefs = List.map (Exp.ident % to_lid) argvars in
      let tyrefs = List.map tyref args in
      let cons = defn "cons" @@
        fun_ argvars (ctor argrefs) in
      let vk = defn "vk" @@ tpf_v_k cons
      and sk = defn "sk" @@ tpf_s_k cons in
      let meta = defn "meta" @@
        tpf_variant ?labels (const_s name) (const_i i) in
      let mcase = Exp.case (pat (List.map (fun _ -> Pat.any ()) args)) meta
      and vcase =
        Exp.case (pat (List.map Pat.var argvars))
        (List.fold_left
          (fun e -> function v, `Rec -> tpf_v_r e v | v, `Id x -> tpf_v_a e v x)
          vk (List.combine argrefs tyrefs))
      and scase =
        Exp.tuple [
          List.fold_left
            (fun e -> function `Rec -> tpf_s_r e | `Id x -> tpf_s_a e x)
            sk tyrefs
        ; meta ] in
      vcase, mcase, scase
    in
    let cases = match tdecl.ptype_kind with
    | Ptype_abstract -> raise_error_t tdecl "is abstract"
    | Ptype_open -> raise_error_t tdecl "is open"
    | Ptype_variant cdecls ->
        cdecls |> List.mapi (fun i { pcd_name; pcd_args } ->
          let ctor_id = to_lid pcd_name in
          match pcd_args with
          | Pcstr_tuple args ->
              case pcd_name.txt i args
              ~pat:(pconstruct ctor_id) ~ctor:(construct ctor_id)
          | Pcstr_record lbls ->
              let pat vars =
                pconstruct ctor_id [Pat.record (labelled lbls vars) Closed]
              and ctor vars =
                construct ctor_id [Exp.record (labelled lbls vars) None] in
              case pcd_name.txt i ~pat ~ctor
              ~labels:(List.map (fun l -> l.pld_name.txt) lbls)
              (List.map (fun l -> l.pld_type) lbls))
    | Ptype_record lbls ->
        let pat vars = Pat.record (labelled lbls vars) Closed
        and ctor vars = Exp.record (labelled lbls vars) None in
        [ case "" 0 ~pat ~ctor
          (List.map (fun l -> l.pld_type) lbls)
          ~labels:(List.map (fun l -> l.pld_name.txt) lbls) ]
    in
    let vcases = List.map (fun (v, _, _) -> v) cases
    and mcases = List.map (fun (_, m, _) -> m) cases
    and scases = List.map (fun (_, _, s) -> s) cases in
    let view = defn "view" @@
      fun_ tyvars Exp.(tuple [function_ vcases; function_ mcases]) in
    let schema = defn "schema" @@
      fun_ tyvars (const_list scases) in
    let _ = defn ~gen:false (data_defn_name tdecl)
      Exp.(constraint_
            (record [lid "view", view; lid "schema", schema] None)
            (data_defn_type tdecl types)) in
    ()

let sig_of_type ~options:_ ~path:_ tdecl =
  let types, _, _ = core_types_in tdecl in
  assert_arity tdecl types;
  match tdecl.ptype_kind with
  | Ptype_abstract -> raise_error_t tdecl "is abstract"
  | Ptype_open -> raise_error_t tdecl "is open"
  | Ptype_variant _ | Ptype_record _ ->
      Sig.value @@
        Val.mk (data_defn_name tdecl |> sid) (data_defn_type tdecl types)

let () =
  Ppx_deriving.(register @@ create "tpf"
    ~type_decl_str: (fun ~options ~path tdecls ->
      List.(map (str_of_type ~options ~path) tdecls |> concat))
    ~type_decl_sig: (fun ~options ~path tdecls ->
      List.map (sig_of_type ~options ~path) tdecls)
    ())
