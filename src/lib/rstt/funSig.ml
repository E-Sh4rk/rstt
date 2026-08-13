open Sstt

type label =
| LConst of string
| LVar of string

type ('v,'r,'i) ty =
| FLVar of string
| FRegular of ('v,'r,'i) Builder.t
| FList of ('v,'r,'i) lst
| FAttr of (('v,'r,'i) ty, 'r Builder.classes) Attr.atom

and ('v,'r,'i) lst = {
    bindings: (label * ('v,'r,'i) ty) list ;
    tl: ('v,'r,'i) ty
}

and ('v,'r,'i) arg = {
    pos_named : (label * ('v,'r,'i) ty) list ;
    pos_tl: ('v,'r,'i) ty ;
    named_tl : ('v,'r,'i) ty ;
    named : (label * ('v,'r,'i) ty) list
}

type ('v,'r,'i) t = { dom: ('v,'r,'i) arg ; ret: ('v,'r,'i) ty }

module StrMap = Map.Make(String)
module StrSet = Set.Make(String)

(* === Generic traversal === *)

(* [map f fl fc t] rebuilds [t] by applying [fl] to every label, [fc] to every
   classes component, and [f] to every (already rebuilt) type node. *)
let map f fl fc t =
  let rec aux t =
    let t = match t with
    | FLVar _ | FRegular _ -> t
    | FList { bindings ; tl } ->
      let bindings = bindings |> List.map (fun (l,t) -> fl l, aux t) in
      FList { bindings ; tl=aux tl }
    | FAttr a -> FAttr (Attr.map_atom aux fc a)
    in
    f t
  in
  aux t

let map_arg f fl fc { pos_named ; pos_tl ; named_tl ; named } =
  let aux = map f fl fc in
  let aux_bindings b = b |> List.map (fun (l,t) -> fl l, aux t) in
  let pos_named = aux_bindings pos_named in
  let pos_tl, named_tl = aux pos_tl, aux named_tl in
  let named = aux_bindings named in
  { pos_named ; pos_tl ; named_tl ; named }

let map_sig f fl fc { dom ; ret } =
  let dom = map_arg f fl fc dom in
  { dom ; ret=map f fl fc ret }

(* === Resolution of identifiers === *)

let resolve env t =
  let env = ref env in
  let regular t =
    let env', t = Builder.resolve !env t in
    env := env' ; t
  in
  let classes c =
    let env', c = Builder.resolve_classes !env c in
    env := env' ; c
  in
  let rec aux t =
    match t with
    | FLVar x -> FLVar x
    | FRegular t -> FRegular (regular t)
    | FList { bindings ; tl } ->
      let bindings = bindings |> List.map (fun (l,t) -> l, aux t) in
      FList { bindings ; tl=aux tl }
    | FAttr { Attr.content ; classes=cs ; attrs } ->
      let content = aux content in
      let cs = classes cs in
      let attrs = aux attrs in
      FAttr { Attr.content ; classes=cs ; attrs }
  in
  let aux_arg { pos_named ; pos_tl ; named_tl ; named } =
    let aux_bindings b = b |> List.map (fun (l,t) -> l, aux t) in
    let pos_named = aux_bindings pos_named in
    let pos_tl = aux pos_tl in
    let named_tl = aux named_tl in
    let named = aux_bindings named in
    { pos_named ; pos_tl ; named_tl ; named }
  in
  let { dom ; ret } = t in
  let dom = aux_arg dom in
  let ret = aux ret in
  !env, { dom ; ret }

(* === Conversion to a regular type === *)

exception Not_regular of string

let regular_label l =
  match l with
  | LConst str -> str
  | LVar x -> raise (Not_regular ("label variable "^x^" is unresolved"))

let rec regular_ty t =
  match t with
  | FLVar x -> raise (Not_regular ("label variable "^x^" is unresolved"))
  | FRegular t -> t
  | FList { bindings ; tl } ->
    let bindings = bindings |> List.map (fun (l,t) -> regular_label l, regular_ty t) in
    Builder.TList { Lst.bindings ; sym=[] ; tl=regular_ty tl }
  | FAttr a -> Builder.TAttr (Attr.map_atom regular_ty Fun.id a)

let regular_arg { pos_named ; pos_tl ; named_tl ; named } =
  let aux_bindings b = b |> List.map (fun (l,t) -> regular_label l, regular_ty t) in
  let pos_named = aux_bindings pos_named in
  let pos_tl, named_tl = regular_ty pos_tl, regular_ty named_tl in
  let named = aux_bindings named in
  { Arg.pos_named ; pos_tl ; named_tl ; named }

let regular_sig { dom ; ret } =
  Builder.TArrow (Builder.TArg (regular_arg dom), regular_ty ret)

let is_regular_ty t =
  match regular_sig t with
  | _ -> true
  | exception (Not_regular _) -> false

let to_regular t =
  try regular_sig t
  with Not_regular msg -> invalid_arg ("Not a regular signature: "^msg^".")

(* === Specialization === *)

(* Types can appear either as a R value (they are then wrapped in a Attr
   container), or as a struct (inside a Attr container). *)
type pos = Value | Struct

(* [str_ty pos str] is the type of the string [str], in the position [pos]. *)
let str_ty =
  let tbl = Hashtbl.create 16 in
  fun pos str ->
    match Hashtbl.find_opt tbl (pos, str) with
    | Some ty -> ty
    | None ->
      let t = Builder.TVec (Vec.Scalar (Builder.PChr' str)) in
      let ty = match pos with
        | Value -> Builder.build Builder.TIdMap.empty t
        | Struct -> Builder.build_struct Builder.TIdMap.empty t
      in
      Hashtbl.add tbl (pos, str) ty ; ty

(* [strings_of_ty pos ty] returns a set of strings [strs] such that [ty]
   (in the position [pos]) is a subtype of the union of the singleton string
   types associated to [strs]. It returns [None] if it cannot compute such a
   (finite and non-empty) set of strings. *)
let strings_of_ty pos ty =
  let exception Unknown in
  let extract_prim p =
    let na, lines = Chr.destruct (Prim.destruct p) in
    if na then raise Unknown ;
    (* Type variables can safely be ignored: dropping them can only
       over-approximate the set of strings. *)
    lines |> List.concat_map (fun { Utils.pos ; prim ; _ } ->
      if pos then prim else raise Unknown)
  in
  let extract_vec a =
    match a with
    | Vec.Scalar p -> extract_prim p
    | Vec.Vector _ -> raise Unknown (* Not a scalar: cannot denote a label *)
  in
  let content = match pos with Struct -> ty | Value -> Attr.proj_content ty in
  match
    (* Negative atoms are ignored: dropping them can only
       over-approximate the set of strings. *)
    content |> Vec.destruct |> List.concat_map (fun (p,_) -> extract_vec p)
  with
  | exception Unknown -> None
  | exception (Invalid_argument _) -> None
  | [] -> None
  | strs ->
    let strs = List.sort_uniq String.compare strs in
    let union = strs |> List.map (str_ty pos) |> Ty.disj in
    if Ty.leq ty union then Some (StrSet.of_list strs) else None

(* [field_of_param elt ~idx ~name] returns the type of the parameter of index
   [idx] and name [name] in the argument atom [elt], if any. *)
let field_of_param elt ~idx ~name =
  let by_name bindings = Option.bind name (fun name -> List.assoc_opt name bindings) in
  let by_idx bindings =
    match idx with
    | Some i when i < List.length bindings -> Some (List.nth bindings i)
    | _ -> None
  in
  match elt with
  | Arg.CallSite { pos' ; named' ; _ } ->
    begin match by_idx pos' with
    | Some fty -> Some fty
    | None -> by_name named'
    end
  | Arg.DefSite { pos_named ; named ; _ } ->
    begin match by_name (pos_named@named) with
    | Some fty -> Some fty
    | None -> by_idx pos_named |> Option.map snd
    end

(* [ty_of_field fty] returns the type of the field [fty],
   provided it is mandatorily present. *)
let ty_of_field fty =
  let oty = fty |> Ty.F.get_descr |> Ty.O.get in
  if Ty.O.Atom.is_required oty then Some (Ty.O.Atom.get oty) else None

(* [occurrences t] returns, for each label variable of [t], the number of times
   it occurs in [t] and the number of times it occurs as a [FLVar] in the
   domain of [t]. *)
let occurrences { dom ; ret } =
  let res = ref StrMap.empty in
  let add x dom_flvar =
    let n, ndom = StrMap.find_opt x !res |> Option.value ~default:(0,0) in
    res := StrMap.add x (n+1, if dom_flvar then ndom+1 else ndom) !res
  in
  let f dom t = (match t with FLVar x -> add x dom | _ -> ()) ; t in
  let fl l = (match l with LVar x -> add x false | LConst _ -> ()) ; l in
  ignore (map_arg (f true) fl Fun.id dom) ;
  ignore (map (f false) fl Fun.id ret) ;
  !res

let specialize t arg =
  let fail x msg =
    invalid_arg ("Cannot specialize the label variable "^x^": "^msg^".")
  in
  (* Constraints inferred from one atom [elt] of the argument.
     Matching is best-effort: positions of the signature that cannot be
     matched with the argument are simply ignored (they may prevent some
     label variables from being resolved, but they never make the
     specialization unsound). *)
  let constraints_of_elt elt =
    let res = ref StrMap.empty in
    let add x strs =
      let strs = match StrMap.find_opt x !res with
        | None -> strs
        | Some strs' -> StrSet.inter strs strs'
      in
      res := StrMap.add x strs !res
    in
    let rec match_ty pos pat ty =
      match pat with
      | FRegular _ -> ()
      | FLVar x -> strings_of_ty pos ty |> Option.iter (add x)
      | FAttr { Attr.content ; attrs ; _ } ->
        match_ty Struct content (Attr.proj_content ty) ;
        match_ty Struct attrs (Attr.proj_attrs ty)
      | FList { bindings ; _ } ->
        let ty = match pos with Struct -> ty | Value -> Attr.proj_content ty in
        bindings |> List.iter (fun (l,pat) ->
          match l with
          (* The label is unknown: the associated field cannot be selected. *)
          | LVar _ -> ()
          | LConst lbl -> match_field pat (Lst.proj lbl ty))
    and match_field pat fty =
      ty_of_field fty |> Option.iter (match_ty Value pat)
    in
    let match_param ~idx ~name pat =
      field_of_param elt ~idx ~name |> Option.iter (match_field pat)
    in
    let name l = match l with LConst str -> Some str | LVar _ -> None in
    t.dom.pos_named |> List.iteri (fun i (l,pat) ->
      match_param ~idx:(Some i) ~name:(name l) pat) ;
    t.dom.named |> List.iter (fun (l,pat) ->
      match_param ~idx:None ~name:(name l) pat) ;
    !res
  in
  (* Each atom of the argument is matched independently, and the resulting
     constraints are joined (the argument may have any of these shapes). *)
  let constraints = Arg.destruct arg |> List.map constraints_of_elt
    |> List.fold_left (StrMap.union (fun _ s1 s2 -> Some (StrSet.union s1 s2)))
      StrMap.empty
  in
  let vars = occurrences t |> StrMap.bindings |> List.filter_map (fun (x,(n,ndom)) ->
    (* A label variable that occurs only once, as a parameter of the domain,
       does not need to be resolved: the associated parameter simply accepts
       any string of length 1. *)
    if n = 1 && ndom = 1 then None
    else match StrMap.find_opt x constraints with
    | None when ndom = 0 ->
      fail x "it is not matched with any parameter of the domain"
    | None -> fail x "it could not be resolved from the given argument"
    | Some strs when StrSet.is_empty strs ->
      fail x "no string can be matched with it"
    | Some strs -> Some (x, StrSet.elements strs))
  in
  (* Build one instance of the signature for each possible assignment
     of the label variables. *)
  let rec assignments vars =
    match vars with
    | [] -> [StrMap.empty]
    | (x,strs)::vars ->
      let assignments = assignments vars in
      strs |> List.concat_map (fun str ->
        assignments |> List.map (StrMap.add x str))
  in
  let instantiate assign =
    let fl l =
      match l with
      | LConst _ -> l
      | LVar x -> begin match StrMap.find_opt x assign with
        | Some str -> LConst str
        | None -> l
        end
    in
    let f t =
      match t with
      | FLVar x -> begin match StrMap.find_opt x assign with
        | Some str -> FRegular (Builder.TVec (Vec.Scalar (Builder.PChr' str)))
        (* [x] does not need to be resolved: the parameter
           accepts any (non-NA) string of length 1. *)
        | None -> FRegular (Builder.TVec (Vec.Scalar (Builder.PHat Builder.PChr)))
        end
      | t -> t
    in
    map_sig f fl Fun.id t |> to_regular
  in
  match assignments vars |> List.map instantiate with
  | [] -> assert false
  | t::ts -> List.fold_left (fun acc t -> Builder.TCap (acc, t)) t ts
