open Ast
open Cst

(*outer wrapper: pstr_module (module_binding (pmod_unpack expr)) *)

type compile_context = {
    val imports : string list ref;
    val requires : string list ref;
    val filepath : string;
}

let create_ctx path = {
    imports=ref [];
    requires=ref [];
    filepath=path;
}

let resolve_import filepath (aliases, from, level) = 


let add_require ctx s =
    ctx.requires <- s :: !(ctx.requires)

let add_import ctx i = 
    ctx.imports <- (resolve_import ctx.filepath i) :: !(ctx.imports)

let boolop_ident = function
    | And -> "&&"
    | Or -> "||"

let op_ident = function
    | Add -> "+"
    | Sub -> "-"
    | StringFmt -> "Printf.sprintf"
    | Mult -> "*"
    | MatMult -> "*"
    | Div -> "/"
    | Mod -> "mod"
    | Pow -> "**"
    | LShift -> "lsl"
    | RShift -> "lsr"
    | BitOr -> "lor"
    | BitXor -> "lxor"
    | BitAnd -> "land"

let unary_op_ident = function
    | Invert -> "lnot"
    | Not -> "not"
    | UAdd -> "+"
    | USub -> "-"

let cmp_op_ident = function
    | Eq -> "=="
    | NotEq -> "!="
    | Lt -> "<"
    | LtE -> "<="
    | Gt -> ">"
    | GtE -> ">="
    | Is -> "="
    | IsNot -> "!="
    | In -> "Libshoots.Prelude.list_contains"
    | NotIn -> "Libshoots.Prelude.list_not_contains"

let rec zip_calls op vals = 
    match vals with
    | [] -> (Ident "()")
    | [a] -> a
    | h :: t -> Call ((Ident op), [h; (zip_calls op t)], [])

let get_string = function
    | Str s -> s
    | _ -> raise (Failure "expected string, got something else")

let rec zip_compare loc l ops comps = 
    match (ops, comps) with
    | [], [] -> l
    | _, [] -> fail loc "unbalanced comparators"
    | [], _ -> fail loc "unbalanced comparators"
    | oh :: ot, ch :: ct -> Call (
        (loc, (Ident (cmp_op_ident l))),
        [ch; (zip_compare loc oh ot ct)],
        [])

let rec compile_expr loc expr =
    (loc, match expr with
    | BoolOp (op, vals) -> zip_calls (boolop_ident op) (List.map (compile_expr loc) vals)
    | BinOp (l, op, r) -> Call (
        (loc, (Ident (op_ident op))),
        [(compile_expr loc l); (compile_expr loc r)],
        [])
    | UnaryOp (op, v) -> Call((loc, (Ident (op_ident op))), [(compile_expr loc v)], [])
    | IfExp (test, t, f) -> If ((compile_expr loc test),
                                (compile_expr loc t),
                                (Some (compile_expr loc f)))
    | Dict (ks, vs) -> Record
        (List.map2 (fun k v -> ((get_string k), (compile_expr loc v))) ks vs)
    | Set elts -> Cons ("::", (List.map (compile_expr loc) elts))
    | Compare (l, ops, comparators) -> zip_compare loc l ops (List.map (compile_expr loc) comparators)
    | Call (func, args, kwargs) -> Call (
        (compile_expr loc func),
        (List.map (compile_expr loc) args),
        (List.map (compile_expr loc) kwargs))
    | Num (Float n) -> Float n
    | Num (Int n) -> Int n
    | Str s -> String s
    | FormattedValue (v, _conversion, args) -> Call ((loc, (Ident "Printf.sprintf")),
        ((compile_expr loc v) :: (List.map (compile_expr loc) args)), [])
    | JoinedStr strs -> Cons ("^", (List.map (compile_expr loc) strs))
    | Bytes s -> String s
    | NameConstnt v -> match v with 
        | True -> (Ident "true")
        | False -> (Ident "false")
        | SNone -> (Ident "()")
    | Attribute (v, attr, Load) -> Field_lookup (attr, (compile_expr loc v))
    | Subscript (v, (lower, upper, step), Load) ->
        let lower = match lower with
        | Some v -> (Cons "Some" [(compile_expr loc v)])
        | None -> (Ident "None") in
        let upper = match upper with
        | Some v -> (Cons "Some" [(compile_expr loc v)])
        | None -> (Ident "None") in 
        let step = match step with
        | Some v -> (Cons "Some" [(compile_expr loc v)])
        | None -> (Ident "None") in
        Call ((Ident "Libshoots.Prelude.slice"),
            [(compile_expr loc v); lower; upper; step], [])
    | Name (id, Load) -> (Ident id)
    | Tuple (items, Load) -> Tuple (List.map (compile_expr loc) items)
    | Null -> fail loc "hit Null expr"
    )

and maybe_compile_expr loc v = 
    match v with
    | None -> None
    | Some v -> compile_expr loc v

and compile_pat loc v = 
    match v with
    | Attribute (value, id, Store) -> Attr_lookup (compile_pat loc value) id
    | Name (id, Store) -> Name id
    | Tuple (elts, Store) -> Tuple (List.map (compile_pat loc) elts)
    | Subscript (v, (lower, upper, None), Load) -> Slice ((compile_pat loc v), (
        (maybe_compile_pat loc lower),
        (maybe_compile_pat loc upper),
        None))
    | Num (Int v) -> Int v
    | Num (Float v) -> Float v
    | String v -> String v
    | _ -> fail loc "Non-assignment expression in assignment context"

and maybe_compile_pat loc v =
    match v with
    | None -> None
    | Some v -> (compile_pat loc v)

and with_decorators loc decorators body = 
    match decorators with
    | [] -> body
    | h :: t -> Call (compile_expr loc h) [(with_decorators loc t)] []

and compile_type loc expr = 
    | Name n -> Ident n
    | Call (Name n), args, _ -> (Parameterized n (List.map (compile_type loc) args))
    | _ -> fail loc "not a valid type declaration"

and arg_pats loc (args, vargs, kwargs, kw_defaults, vkwarg, defaults) =
    let mapper tag = 
        (fun (name, typ) default -> 
            match default with 
            | Null -> (compile_type typ, tag name)
            | default -> (compile_type typ, Default ((tag name), (compile_expr loc default)))) in
    let args_pats = List.map2 (mapper Name) args defaults in 
    let kwarg_pats = List.map2 (mapper Label) kwargs kw_defaults in 
    args_pats @ kwarg_pats

and fun_tree loc args body = 
    match args with
    | [] -> (compile_expr loc body)
    | h :: t -> (Fun1 h (fun_tree loc t body))

and compile_fun loc ctx args body = 
    match args with 
    | [] -> (loc, (Do (List.map (compile_stmt ctx body))))
    | arg :: args -> (loc, 
        match arg with
        | Name (s, _) -> (Fun1 (Nolabel, (Name s), None, (compile_fun loc ctx args body)))
        | Attribute (value, attr, _) -> Fun1 (
            (Labelled attr), (Name s), (compile_expr ctx value), (compile_fun loc ctx args body))
        | Tuple (vals, _) -> Fun1 (Nolabel, (Tuple (List.map (compile_pat loc) vals)) 
            None, (compile_fun loc ctx args body))

and compile_stmt ctx statements = 
    match statements with 
    | [] -> (Ident "()")
    | (loc, v) :: rst -> 
    (loc, match v with 
    | FunctionDef (name, [], body, decorators, _return_typ) ->
            (Let ((Name name), 
                  (with_decorators loc decorators (Fun0 (compile_expr loc body))))
                (compile_stmt ctx rst))
    | FunctionDef (name, args, body, decorators, _return_typ) ->
            (Let ((Name name)
                  (with_decorators loc decorators (compile_fun loc ctx args body))))
    | Widget (spec, _args, params, body) ->
            Widget spec
                (List.map (fun (id, v) -> (id, compile_pat loc v)) params) 
                (compile_stmt ctx body)
    | For (target, iter, body) -> 
            (Call (Ident "Libshoots.Prelude._for") [
                (Cons ("[]", []))
                (fun_tree loc (arg_pats loc [iter]) body);
                (compile_expr loc target)] [])
    | Break -> (Ident "Break")
    | Continue -> (Ident "Continue")
    | If (test, body, els) -> 
            (If
                (compile_expr loc test)
                (compile_stmt ctx body)
                (compile_stmt ctx els))
    | Let (items, body) ->
            let bindings = List.map 
                (fun (target, src) -> (compile_pat loc pat, compile_expr loc src))
                items in 
            (Let (bindings, (compile_stmt ctx body)))
    | Require s -> add_require ctx s
    | Import aliases -> add_import ctx aliases None None; (Ident "()")
    | ImportFrom (from, aliases, level) -> add_import ctx from aliases level; (Ident "()")
    | Assert (test, msg) -> Assert (compile_expr loc test,
                                    match msg with 
                                    | None -> None
                                    | Some msg -> Some (compile_expr loc msg))
    | Expr expr -> compile_expr loc expr
    )

and compile_cst path tree = 
    let ctx = create_ctx path in 
    (ctx, (Do (List.map (compile_stmt ctx) tree)))

