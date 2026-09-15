open Sstt

type gvar = { group: string ; column: string }

type label =
| LConst of string
| LVar of string
| LGroup of gvar

type ('v,'r,'i) ty =
| FLVar of string
| FGVar of gvar
| FRegular of ('v,'r,'i) Builder.t
| FList of (label, ('v,'r,'i) ty) Lst.atom
| FAttr of (('v,'r,'i) ty, 'r Builder.classes) Attr.atom

type ('v,'r,'i) arg = (label, ('v,'r,'i) ty) Arg.atom

type ('v,'r,'i) t = { dom: ('v,'r,'i) arg ; ret: ('v,'r,'i) ty }

module StrMap = Map.Make(String)
module StrSet = Set.Make(String)

(* === Generic traversal === *)

(* [map f fl fc t] rebuilds [t] by applying [fl] to every label, [fc] to every
   classes component, and [f] to every (already rebuilt) type node. *)
let map f fl fc t =
  let rec aux t =
    let t = match t with
    | FLVar _ | FGVar _ | FRegular _ -> t
    | FList a -> FList (Lst.map_atom fl aux a)
    | FAttr a -> FAttr (Attr.map_atom aux fc a)
    in
    f t
  in
  aux t

let map_arg f fl fc a = Arg.map_atom fl (map f fl fc) a

let map_sig f fl fc { dom ; ret } =
  let dom = map_arg f fl fc dom in
  { dom ; ret=map f fl fc ret }

(* === Groups === *)

(* The variables of a group [g] are those whose name ends with ["_g"]. *)
let group_column g str =
  let suffix = "_"^g in
  if String.ends_with ~suffix str
  then Some (String.sub str 0 (String.length str - String.length suffix))
  else None

let repetition g key body =
  let column str = match group_column g str with
    | Some column -> { group=g ; column }
    | None -> invalid_arg (str^" is not a column of the group "^g)
  in
  let f t = match t with
    | FLVar str when group_column g str <> None -> FGVar (column str)
    | t -> t
  in
  LGroup (column key), map f Fun.id Fun.id body

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
    | FGVar gv -> FGVar gv
    | FRegular t -> FRegular (regular t)
    | FList a -> FList (Lst.map_atom Fun.id aux a)
    | FAttr { Attr.content ; classes=cs ; attrs } ->
      let content = aux content in
      let cs = classes cs in
      let attrs = aux attrs in
      FAttr { Attr.content ; classes=cs ; attrs }
  in
  let { dom ; ret } = t in
  let dom = Arg.map_atom Fun.id aux dom in
  let ret = aux ret in
  !env, { dom ; ret }

(* === Conversion to a regular type === *)

exception Not_regular of string

let unresolved_group { group ; column } =
  raise (Not_regular ("column "^column^" of the group "^group^" is unresolved"))

let regular_label l =
  match l with
  | LConst str -> str
  | LVar x -> raise (Not_regular ("label variable "^x^" is unresolved"))
  | LGroup gv -> unresolved_group gv

let rec regular_ty t =
  match t with
  | FLVar x -> raise (Not_regular ("label variable "^x^" is unresolved"))
  | FGVar gv -> unresolved_group gv
  | FRegular t -> t
  | FList a -> Builder.TList (Lst.map_atom regular_label regular_ty a)
  | FAttr a -> Builder.TAttr (Attr.map_atom regular_ty Fun.id a)

let regular_arg polymorphic a =
  let a = Arg.map_atom regular_label regular_ty a in
  if polymorphic then Builder.TPolyArg a else Builder.TArg a

let regular_sig polymorphic { dom ; ret } =
  Builder.TArrow (regular_arg polymorphic dom, regular_ty ret)

let is_regular_ty t =
  match regular_sig false t with
  | _ -> true
  | exception (Not_regular _) -> false

let fail_not_regular kind f t =
  try f t with Not_regular msg -> invalid_arg ("Not a regular "^kind^": "^msg^".")

let to_regular ?(polymorphic=false) t = fail_not_regular "signature" (regular_sig polymorphic) t
let to_regular_ty t = fail_not_regular "type" regular_ty t
let to_regular_arg ?(polymorphic=false) t = fail_not_regular "argument" (regular_arg polymorphic) t

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


(* The state of the algorithm: candidate labels for each column (a column
   absent from the map is unconstrained), and the entities of each occurrence. *)

type col = CVar of string | CCol of gvar
type ent = EConst of string | EVar of string | ECol of gvar | EBot

module ColMap = Map.Make(struct type t = col let compare = compare end)

type ('v,'r,'i) occ = {
  fields : (string * Ty.t) list ;
  (* Whether [fields] is all the record can have. When it is not, a label that
     is not observed may still be present, so the observation says nothing
     about the columns it does not mention. *)
  closed : bool ;
  ents : ent list ;
  (* Those entries of [ents] whose binding admits absence: the argument need
     not have a field at their label. *)
  opt : ent list ;
  reps : (gvar * ('v,'r,'i) ty) list ;
}

let cand_inter = ColMap.union (fun _ s1 s2 -> Some (StrSet.inter s1 s2))
let cand_join = ColMap.merge (fun _ s1 s2 ->
  match s1, s2 with Some s1, Some s2 -> Some (StrSet.union s1 s2) | _ -> None)
(* A column absent from the map is unconstrained, and stays so: what is being
   removed is finite, and the admissible set it is taken from is not. *)
let cand_remove v c strs =
  match ColMap.find_opt c v with
  | None -> v
  | Some s -> ColMap.add c (StrSet.diff s strs) v

let may v c lbl =
  match ColMap.find_opt c v with None -> true | Some s -> StrSet.mem lbl s
let pinned v c =
  match ColMap.find_opt c v with
  | Some s when StrSet.cardinal s = 1 -> Some (StrSet.choose s)
  | _ -> None
let cand_nb v c =
  match ColMap.find_opt c v with None -> max_int | Some s -> StrSet.cardinal s

let ent_of_label l =
  match l with
  | LConst str -> EConst str
  | LVar x -> EVar x
  | LGroup gv -> ECol gv
let col_of_ent e =
  match e with
  | EConst _ | EBot -> None
  | EVar x -> Some (CVar x)
  | ECol gv -> Some (CCol gv)

(* A record is open when its tail is not [absent]: the tail then claims the
   labels the scheme does not name. *)
let is_open tl =
  match tl with FRegular (Builder.TOption Builder.TEmpty) -> false | _ -> true

(* Whether a binding admits the absence of its field, as [#k: any?] does. *)
let is_optional b =
  match b with FRegular (Builder.TOption _) -> true | _ -> false

(* Whether a record tail admits no further field. *)
let tail_closed fty =
  fty |> Ty.F.get_descr |> Ty.O.get |> Ty.O.Atom.get |> Ty.is_empty

(* [fields_of_lst ty] enumerates the fields of [ty] when it is a single list
   atom, and returns None when it cannot be observed. *)
let fields_of_lst ty =
  let fields bindings =
    bindings |> List.filter_map (fun (lbl,fty) ->
      ty_of_field fty |> Option.map (fun ty -> lbl, ty))
  in
  match Lst.destruct ty with
  | [ ([ { Lst.bindings ; tl } ], []) ] -> Some (fields bindings, tail_closed tl)
  | _ | exception (Invalid_argument _) -> None

let fields_of_arg elt =
  let bindings, tl = match elt with
    | Arg.CallSite { named' ; named_tl' ; _ } -> named', named_tl'
    | Arg.DefSite { pos_named ; named ; named_tl ; _ } -> pos_named@named, named_tl
  in
  bindings |> List.filter_map (fun (lbl,fty) ->
    ty_of_field fty |> Option.map (fun ty -> lbl, ty)),
  tail_closed tl

(* === Narrowing (the traversal) === *)

(* [walk v occs pos pat ty] matches the scheme node [pat] against the concrete
   type [ty], returning the candidates it narrows and appending to [occs] every
   record occurrence it goes through. *)
let rec walk v occs pos pat ty =
  let constr c strs =
    match strs with None -> ColMap.empty | Some s -> ColMap.singleton c s
  in
  match pat with
  | FRegular _ -> ColMap.empty
  | FLVar x -> constr (CVar x) (strings_of_ty pos ty)
  | FGVar gv -> constr (CCol gv) (strings_of_ty pos ty)
  | FAttr { Attr.content ; attrs ; _ } ->
    cand_inter
      (walk v occs Struct content (Attr.proj_content ty))
      (walk v occs Struct attrs (Attr.proj_attrs ty))
  | FList { bindings ; tl } ->
    let ty = match pos with Struct -> ty | Value -> Attr.proj_content ty in
    let lookup lbl = Lst.proj lbl ty |> ty_of_field in
    record v occs ~opened:(is_open tl)
      (bindings |> List.map (fun (l,b) -> l, b, lookup)) (fields_of_lst ty)

(* [record v occs bindings fields] narrows the candidates from one record
   occurrence, [fields] being its observed fields (None when it cannot be
   observed, in which case it is not an occurrence). *)
and record v occs ~opened bindings obs =
  let fields = Option.map fst obs in
  let ents = bindings |> List.map (fun (l,_,_) -> ent_of_label l) in
  let ents = if opened then EBot::ents else ents in
  let opt = bindings |> List.filter_map (fun (l,b,_) ->
    if is_optional b then Some (ent_of_label l) else None) in
  let reps = bindings |> List.filter_map (fun (l,b,_) ->
    match l with LGroup gv -> Some (gv,b) | _ -> None) in
  obs |> Option.iter (fun (fields, closed) ->
    occs := { fields ; closed ; ents ; opt ; reps } :: !occs) ;
  let body b ty = match ty with None -> ColMap.empty | Some ty -> walk v occs Value b ty in
  bindings |> List.fold_left (fun acc (l,b,lookup) ->
    match l with
    | LConst str -> cand_inter acc (body b (lookup str))
    | LVar x ->
      (* A key whose label is still unknown cannot be used to select a field. *)
      begin match pinned v (CVar x) with
      | Some str -> cand_inter acc (body b (lookup str))
      | None -> acc
      end
    | LGroup gv ->
      (* Which candidates are selected is not known yet, so the constraints
         produced by the body are joined instead of being intersected. *)
      begin match fields with
      | None -> acc
      | Some fields ->
        match fields |> List.filter_map (fun (lbl,ty) ->
          if may v (CCol gv) lbl then Some (walk v occs Value b ty) else None)
        with
        | [] -> acc
        | c::cs -> cand_inter acc (List.fold_left cand_join c cs)
      end)
    ColMap.empty

let walk_sig v occs t elt =
  let param idx (l,b) =
    let idx = match l with LGroup _ -> None | _ -> idx in
    l, b, (fun str -> Option.bind (field_of_param elt ~idx ~name:(Some str)) ty_of_field)
  in
  let bindings =
    (t.dom.Arg.pos_named |> List.mapi (fun i b -> param (Some i) b))
    @ (t.dom.Arg.named |> List.map (param None))
  in
  (* The result is matched against no concrete type: it constrains nothing. *)
  record v occs ~opened:(is_open t.dom.Arg.pos_tl || is_open t.dom.Arg.named_tl)
    bindings (Some (fields_of_arg elt))

(* === Choosing === *)

(* The entities that may still claim [lbl] at the occurrence [o]. *)
let claimants v o lbl =
  if List.mem (EConst lbl) o.ents then [EConst lbl]
  else match o.ents |> List.find_opt (fun e ->
    match e with EVar x -> pinned v (CVar x) = Some lbl | _ -> false)
  with
  | Some e -> [e]
  | None -> o.ents |> List.filter (fun e ->
    match e with
    | EBot -> true (* the tail may claim any label *)
    | EConst _ -> false
    | EVar x -> may v (CVar x) lbl
    | ECol gv -> may v (CCol gv) lbl)

let nkeys occs c =
  occs |> List.filter (fun o -> o.ents |> List.exists (fun e -> col_of_ent e = Some c))
       |> List.length

(* The entity claiming [lbl], when the rules single out one. *)
let claim v occs o lbl =
  let rank e = match col_of_ent e with
    | None -> (0,0)
    | Some c -> (cand_nb v c, - (nkeys occs c))
  in
  match claimants v o lbl |> List.filter (fun e -> e <> EBot) with
  | [e] -> Some e
  | es ->
    (* The most constrained claimant wins; a tie resolves nothing. *)
    match List.sort (fun e1 e2 -> compare (rank e1) (rank e2)) es with
    | e1::e2::_ -> if rank e1 < rank e2 then Some e1 else None
    | _ -> None

(* A column keys possibly several occurrences, and claims the same labels at
   each of them. *)
let agreement v occs =
  occs |> List.fold_left (fun v o ->
    o.ents |> List.fold_left (fun v e ->
      match col_of_ent e with
      | None -> v
      | Some c ->
        let claimed, rest = o.fields |> List.map fst
          |> List.partition (fun lbl -> List.mem e (claimants v o lbl)) in
        if not o.closed || List.mem e o.opt
        then
          (* A label the record does not have is admissible too, either
             because the binding may be absent or because the record may have
             fields beyond the observed ones; only a label it *does* have, and
             that this entry cannot claim, is ruled out. Intersecting with
             [claimed] instead would force the field to be present, which is
             what an optional binding says it need not be -- and a signature
             that adds a field, such as
             [(x: {#k: any?, `r}, k: #k, v: 'b) -> {#k: 'b, `r}], could then
             never be specialized. *)
          cand_remove v c (StrSet.of_list rest)
        else cand_inter v (ColMap.singleton c (StrSet.of_list claimed))) v) v

let solve t elt =
  let rec loop v n =
    let occs = ref [] in
    let v' = cand_inter v (walk_sig v occs t elt) in
    let v' = agreement v' !occs in
    if n = 0 || ColMap.equal StrSet.equal v v' then v, !occs else loop v' (n-1)
  in
  loop ColMap.empty 8

(* === Instantiation === *)

(* The columns of each group of [t]. *)
let columns t =
  let add { group ; column } m =
    StrMap.update group (function
      | None -> Some (StrSet.singleton column)
      | Some s -> Some (StrSet.add column s)) m
  in
  let m = ref StrMap.empty in
  let fl l = (match l with LGroup gv -> m := add gv !m | _ -> ()) ; l in
  let f ty = (match ty with FGVar gv -> m := add gv !m | _ -> ()) ; ty in
  ignore (map_sig f fl Fun.id t) ; !m

(* The instances of each determined group, as rows over its columns. *)
let instances t v occs =
  let contribs g =
    occs |> List.concat_map (fun o ->
      o.reps |> List.filter_map (fun (gv,body) ->
        if gv.group <> g then None else
        Some (gv, o.fields |> List.filter_map (fun (lbl,ty) ->
          if claim v occs o lbl <> Some (ECol gv) then None else
          (* The other columns of the row are read off the body. *)
          let c = walk v (ref []) Value body ty in
          Some (ColMap.fold (fun col strs row ->
            match col, StrSet.elements strs with
            | CCol gv', [str] when gv'.group = g -> StrMap.add gv'.column str row
            | _ -> row) c (StrMap.singleton gv.column lbl))))))
  in
  columns t |> StrMap.filter_map (fun g cols ->
    let contribs = contribs g in
    let complete rows = rows |> List.for_all (fun row ->
      cols |> StrSet.for_all (fun col -> StrMap.mem col row)) in
    match contribs |> List.find_opt (fun (_,rows) -> complete rows) with
    | None -> None
    | Some (_,rows) ->
      let image gv rows = rows |> List.filter_map (StrMap.find_opt gv.column)
        |> StrSet.of_list in
      if contribs |> List.for_all (fun (gv,rows') ->
           StrSet.equal (image gv rows) (image gv rows'))
      then Some rows else None)

exception Clash of string list

let fresh_col =
  let tbl = Hashtbl.create 16 in
  fun group i v ->
    let key = group, i, Var.name v in
    match Hashtbl.find_opt tbl key with
    | Some v -> v
    | None -> let v' = Var.mk (Var.name v) in Hashtbl.add tbl key v' ; v'

let is_col group v = String.ends_with ~suffix:("_"^group) (Var.name v)

let str_pat str = FRegular (Builder.TVec (Vec.Scalar (Builder.PChr' str)))

(* [instantiate t assign inst] replaces the resolved label variables and
   expands the determined groups of [t]. *)
let instantiate t assign inst =
  (* Inside a repetition, the columns of the group are replaced by their value
     at the instance, and its type columns by a variable fresh for it. *)
  let subst group i row b =
    let var v = if is_col group v then fresh_col group i v else v in
    let f ty = match ty with
      | FGVar gv when gv.group = group -> str_pat (StrMap.find gv.column row)
      | FRegular bty ->
        FRegular (Builder.map (function Builder.TVar v -> Builder.TVar (var v) | t -> t)
          Fun.id Fun.id bty)
      | ty -> ty
    in
    map f Fun.id Fun.id b
  in
  let rec inst_ty ty =
    match ty with
    | FLVar x -> (match assign x with Some str -> str_pat str | None -> ty)
    | FGVar _ | FRegular _ -> ty
    | FList { bindings ; tl } -> FList { bindings = record bindings ; tl = inst_ty tl }
    | FAttr a -> FAttr (Attr.map_atom inst_ty Fun.id a)
  (* Each binding is tagged with the group that produced it, if any, so that a
     clash can name the groups to give up on. *)
  and inst_bindings bindings =
    bindings |> List.concat_map (fun (l,b) ->
      match l with
      | LConst _ -> [None, (l, inst_ty b)]
      | LVar x -> (match assign x with
        | Some str -> [None, (LConst str, inst_ty b)]
        | None -> [None, (l, inst_ty b)])
      | LGroup gv -> (match inst gv.group with
        | None -> [None, (l, inst_ty b)]
        | Some rows -> rows |> List.mapi (fun i row ->
          Some gv.group,
          (LConst (StrMap.find gv.column row), inst_ty (subst gv.group i row b)))))
  (* Expanding a record must not produce the same label twice. *)
  and checked bindings =
    let labels = bindings |> List.filter_map (fun (_,(l,_)) ->
      match l with LConst str -> Some str | _ -> None) in
    let groups = bindings |> List.filter_map fst in
    if List.length (List.sort_uniq String.compare labels) < List.length labels
       && groups <> [] then raise (Clash groups) ;
    List.map snd bindings
  and record bindings = checked (inst_bindings bindings) in
  let { Arg.pos_named ; pos_tl ; named ; named_tl } = t.dom in
  let pos_named, named = inst_bindings pos_named, inst_bindings named in
  ignore (checked (pos_named@named)) ;
  let dom = { Arg.pos_named = List.map snd pos_named ; pos_tl = inst_ty pos_tl ;
              named = List.map snd named ; named_tl = inst_ty named_tl } in
  { dom ; ret = inst_ty t.ret }

let specialize t arg =
  (* Each atom of the argument is solved independently: only what all of them
     agree on is committed. *)
  let sols = Arg.destruct arg |> List.map (fun elt ->
    let v, occs = solve t elt in
    let assign = ColMap.fold (fun c strs assign ->
      match c, StrSet.elements strs with
      | CVar x, [str] -> StrMap.add x str assign
      | _ -> assign) v StrMap.empty in
    assign, instances t v occs)
  in
  let agree eq sols = match sols with
    | [] -> None
    | s::ss -> if List.for_all (eq s) ss then Some s else None
  in
  let assign x = sols |> List.map (fun (a,_) -> StrMap.find_opt x a) |> agree (=)
    |> Option.join in
  let rec build dropped =
    let inst g =
      if List.mem g dropped then None
      else sols |> List.map (fun (_,i) -> StrMap.find_opt g i)
        |> agree (=) |> Option.join
    in
    match instantiate t assign inst with
    | t -> t
    | exception (Clash groups) ->
      match groups |> List.filter (fun g -> not (List.mem g dropped)) with
      | [] -> instantiate t assign (fun _ -> None)
      | groups -> build (groups@dropped)
  in
  build []
