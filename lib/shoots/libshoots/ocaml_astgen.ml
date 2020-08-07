open Ppxlib.Ast_builder.Default
open Parsetree

let widget_ctr = ref 0

(* we could use Parsing.Parse.longident here but it's declared unstable *)
let make_ident s =
    match String.split_on_char '.' s with
    | [] -> (Lident "()")
    | [a] -> (Lident a)
    | frst :: rst -> List.fold_left (fun res s -> (Ldot res s)) (Lident frst) rst

let int_const i = 
    (Pconst_integer ((int_to_string i), None))

let rec compile_expr ctx (loc, expr) = 
    match expr with
    | Do [expr] -> compile_expr ctx expr
    | Do expr :: exprs -> List.fold_left
        (fun res x -> pexp_sequence ~loc res (compile_expr ctx x))
        (compile_expr ctx expr) exprs
    | Do [] -> pexp_construct ~loc (Lident "()")
    | Int v -> pexp_constant ~loc (int_const v)
    | Float v -> pexp_constant ~loc (Pconst_float ((float_to_string v), None))
    | String v -> pexp_constant ~loc (Pconst_string v)
    | Ident s -> pexp_ident ~loc (make_ident s)
    | Call (op, args, kwargs) ->
            let args = List.map (fun v -> Nolabel (compile_expr ctx v)) args in 
            let op = compile_expr ctx op in
            let kwargs = List.map 
                (fun (k, v) -> Labelled (k, (compile_expr ctx v))) kwargs in
            pexp_apply ~loc op (args @ kwargs)
    | Field_lookup (k, target) -> pexp_field ~loc (compile_expr ctx target) (make_ident k)
    | Cons (op, []) -> pexp_construct ~loc (make_ident op) None
    | Cons (op, [v]) -> pexp_construct ~loc (make_ident op) (Some (compile_expr ctx v))
    | Cons (op, h :: vs) -> let op = make_ident op in 
        List.fold_left
            (fun res v -> pexp_construct ~loc op Some (compile_expr ctx v, res))
            (compile_expr ctx h) vs
    | Fun0 body -> pexp_fun ~loc Nolabel None (ppat_any ~loc) (compile_expr ctx body)
    | Tuple items -> pexp_tuple ~loc (List.map (compile_expr ctx) items)
    | Record pairs -> pexp_record ~loc 
        (List.map (fun (k, v) -> ((make_ident k), (compile_expr ctx v))) pairs)
        None
    | If (cond, t, f) -> pexp_ifthenelse ~loc 
        (compile_expr ctx cond)
        (compile_expr ctx t)
        match f with
        | None -> None
        | Some v -> Some (compile_expr ctx v)
    | Widget (_, _, v) -> [%expr Libshoots.Prelude.make_widget 
        [%e let v = !widget_ctr in incr widget_ctr; v]
        (fun () -> [%e (compile_expr loc v) ])  ]
    | Let (binds, body) -> pexp_let ~loc Nonrecursive 
        (List.map 
            (fun (pat, expr) -> value_binding ~loc
                (compile_pat loc pat)
                (compile_expr loc expr))
            binds)
        body

    | Fun1 (arg_label, pat, default, body) -> pexp_fun ~loc 
        arg_label
        (match default with
        | None -> None
        | Some v -> (compile_expr loc v))
        (compile_pat loc pat)
        (compile_expr loc body)

and compile_type loc typ = 
    match typ with
    | Meta s -> ptyp_var ~loc s
    | Const (s, vs) -> ptyp_constr ~loc (make_ident s) (List.map (compile_type loc) vs)
    | Tuple vs -> ptyp_tuple ~loc (List.map (compile_type loc) vs)
    | List vs -> ptyp_construct ~loc (Lident "::") (List.map (compile_type loc) vs)

and compile_pat loc (typ, pat) = 
    let untyped = match pat with
    | Name s -> ppat_var ~loc s
    | Tuple vs -> ppat_tuple ~loc (List.map (compile_ppat loc) vs)
    | Attr_lookup (target, attr) -> ppat_record ~loc 
        [((make_ident attr), (compile_pat loc target))]
    | Slice (target, ((Int start), (Int stop), None)) -> ppat_interval ~loc
        (int_const start) (int_const stop)
    | Int of i -> ppat_constant ~loc (int_const i)
    | Float of f -> ppat_constant ~loc (Pconst_float ((float_to_string f), None))
    | String of s -> ppat_constant ~loc (Pconst_string s) in
    match typ with 
    | None -> untyped
    | Some t -> ppat_constraint ~loc untyped (compile_type loc typ)
