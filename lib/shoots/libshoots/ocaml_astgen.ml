open Parsetree
open Asttypes
open Longident

module Ah = Ast_helper
module L = Location

let collect_defs tree = 
    let defs = ref [] in begin
        Ast.trasnform (fun loc node -> 
            match node with 
            | Def bind -> (defs := (loc, bind) :: defs; (loc, Def bind))
            | v -> (loc, v)) tree;
        !defs
    end

let rec compile_expr (loc, v) =
    let module E = Ah.Exp in 
    match v with 
    | Call (op, args, kws) -> E.apply ~loc:loc 
        (compile_expr op)
        (List.map (fun e -> (Nolabel, (compile_expr e))) args)
        (List.map (fun (k, e) -> (Labelled k, (compile_expr e))) kws)
    | Field_lookup (k, e) -> E.field ~loc:loc
        (compile_expr e)
        (L.mkloc (Lident k) loc)
    | Cons (op, elems) -> construct loc (L.mkloc (Lident op) loc) elems
    | Let (binds, body) -> E.let_ ~loc:loc Nonrecursive
        (compile_binds binds)
        (compile_expr body)
    | Def (k, _) -> 
        (* Def's get hoisted before expr compilation, so what remains should be stubs *)
        E.ident ~loc:loc (L.mkloc (Lident k) loc)
    | Fun0 e -> E.fun_ ~loc:loc Nolabel None (Ah.Pat.any ()) (compile_expr e)
    | Fun1 (label, args, default, body) -> E.fun_ ~loc:loc label
        (match default with
        | None -> None
        | Some v -> (compile_expr v))
        (compile_pat args)
        (compile_expr body)
    | String s -> E.constant ~loc:loc (Ah.Const.string ~loc:loc s)
    | Int i -> E.constant ~loc:loc (Ah.Const.int ~loc:loc i)
    | Float f -> E.constant ~loc:loc (Ah.Const.float ~loc:loc f)
    | Tuple vs -> E.tuple ~loc:loc (List.map compile_expr vs)
    | Record items -> E.record ~loc:loc 
        (List.map (fun (k, e) -> ((L.mkloc (Lident k) loc), (compile_expr e))) items)
    | If (c, t, f) -> E.ifthenelse ~loc:loc 
        (compile_expr c)
        (compile_expr t)
        (match f with
        | None -> None
        | Some f -> compile_expr f)
    | Do e :: es -> List.fold_left
        (fun res e -> E.sequence ~loc:loc (compile_expr e) res)
        (compile_expr e) es
    | Ident s -> E.ident ~loc:loc (L.mkloc (Lident s) loc)
    | Variant s -> E.variant ~loc:loc s None
    | Assert (t, m) -> E.assert_ ~loc:loc 
        (compile_expr t)
        (match m with
        | Some m -> compile_expr m
        | None -> E.construct ~loc:loc (E.ident (L.mkloc (Lident "()"))))
    | Widget _ -> raise (Failure "Widgets should be removed by a separate compiler pass")


and compile_binds = List.map compile_bind

and compile_bind (pat, e) = 
    Ah.Vb.mk (compile_pat pat) (compile_expr e)

and compile_pat pat = 
    let open Ah.Pat in
    match pat with
    | Name_pat n -> var n
    | Tuple_pat vs -> tuple (List.map compile_pat vs)
    | Attr_lookup_pat (p, k) -> record Open [((Lident k), (compile_pat p))]

and construct loc op elems = 
    match elems with 
    | [] -> E.construct ~loc:loc op None
    | h :: t -> E.construct ~loc:loc op 
        (E.tuple ~loc:loc [(compile_expr h); (construct loc op t)])

let structure_of_expr (loc, (k, e)) = Ah.Str.value Nonrecursive 
    [(Ah.Vb.mk ~loc:loc (Ah.Pat.var k) e)]

let compile_module module_name tree = 
    let defs = collect_defs tree in 
    let defs = List.map (fun (loc, (k, v)) -> (loc, k, (compile_expr v))) defs in 
    List.map structure_of_expr defs

