open Ast
open Cst

exception Compile_error of (Ppxlib.Location.t option * string)

let try_finalize f x finally y =
   let res = try f x with exn -> finally y; raise exn in
   finally y;
       res

module AC = struct 

    let cur_loc : Ppxlib.Location.t option ref = ref None

    let with_loc (loc : Ppxlib.Location.t) f = 
        let prev_loc = !cur_loc in begin
            cur_loc := Some loc;
            try_finalize f () (fun () -> cur_loc := prev_loc) ()
        end

    let construct v = 
        match !cur_loc with
        | None -> raise (Failure "tried to construct ast node from outside with_loc block")
        | Some loc -> (loc, v)

    let call op args kwargs = construct (Ast.Call (op, args, kwargs))
    let field_lookup field v = construct (Ast.Field_lookup (field, v))
    let cons h t = construct (Ast.Cons (h, t))
    let let_ binds body = construct (Ast.Let (binds, body))
    let def k v = construct (Ast.Def (k, v))
    let fun0 body = construct (Ast.Fun0 body)
    let fun1 func = construct (Ast.Fun1 func)
    let string_ s = construct (Ast.String s)
    let int_ i = construct (Ast.Int i)
    let float_ f = construct (Ast.Float f)
    let widget spec params children = construct (Ast.Widget (spec, params, children))
    let tuple vs = construct (Ast.Tuple vs)
    let record items = construct (Ast.Record items)
    let if_ cond t f = construct (Ast.If (cond, t, f))
    let do_ vs = match vs with 
    | [v] -> v
    | vs -> construct (Ast.Do vs)
    let ident k = construct (Ast.Ident k)
    let variant k = construct (Ast.Variant k)
    let assert_ t k = construct (Ast.Assert (t, k))

    let fail msg = raise (Compile_error (!cur_loc, msg))

end

let fail_noassign () = AC.fail "variable assignment is not supported (by design)"

(*outer wrapper: pstr_module (module_binding (pmod_unpack expr)) *)


type 'a stmt_ctx = {
    imports: 'a list ref;
    requires: string list ref;
    filepath: string;
}

let create_ctx path = {
    imports=ref [];
    requires=ref [];
    filepath=path;
}

let call_unit fname = AC.(call fname [(ident "()")] [])

let tuple_to_list (v : Cst.expr option) = 
    match v with
    | Some (Tuple (elts, _ctx)) -> elts
    | None -> []
    | Some v -> [v]

let add_require ctx s =
    ctx.requires := (s :: !(ctx.requires))

let add_import ctx i = 
    ctx.imports := ((ctx.filepath, i) :: !(ctx.imports))

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

let rec zip_calls op (vals : Ast.expr list) = 
    match vals with
    | [] -> (AC.ident "()")
    | [a] -> a
    | h :: t -> AC.(call op [h; (zip_calls op t)] [])

let get_string = function
    | Str s -> s
    | Name (s, _) -> s
    | _ -> AC.fail "expected string or identifier, got something else"



let rec compile_expr expr : Ast.expr =
    match expr with
    | BoolOp (op, vals) -> zip_calls
        (boolop_ident op)
        (List.map compile_expr vals)
    | BinOp (l, op, r) -> AC.(call
        (op_ident op)
        [(compile_expr l); (compile_expr r)]
        [])
    | UnaryOp (op, v) -> AC.(call (unary_op_ident op) [(compile_expr v)] [])
    | IfExp (test, t, f) -> AC.if_ (compile_expr test)
                                (compile_expr t)
                                (Some (compile_expr f))
    | Dict (ks, vs) -> AC.record
        (List.map2 (fun k v -> ((get_string k), (compile_expr v))) ks vs)
    | Set elts -> AC.(cons "::" (List.map compile_expr elts))
    | Compare (l, (ops : Cst.cmpop list), comparators) -> zip_compare l ops comparators
    | Call (func, args, kwargs) -> AC.call 
        (get_name func)
        (List.map compile_expr args)
        (List.map compile_keyword kwargs)
    | Num (Float n) -> AC.float_ n
    | Num (Int n) -> AC.int_ n
    | Str s -> AC.string_ s
    | FormattedValue (v, _conv, args) -> AC.(call
        "Printf.sprintf" 
        ((compile_expr v) :: (List.map compile_expr (tuple_to_list args))) [])
    | JoinedStr strs -> AC.cons "^" (List.map compile_expr strs)
    | Bytes s -> AC.string_ s
    | NameConstant v -> begin match v with 
        | True -> AC.ident "true"
        | False -> AC.ident "false"
        | SNone -> AC.ident "()"
    end
    | Attribute (v, attr, Load) -> AC.field_lookup attr (compile_expr v)
    | Subscript (v, Slice (lower, upper, step), Load) ->
        AC.(let lower = match lower with
            | Some v -> (cons "Some" [(compile_expr v)])
            | None -> (ident "None") in
            let upper = match upper with
            | Some v -> (cons "Some" [(compile_expr v)])
            | None -> (ident "None") in 
            let step = match step with
            | Some v -> (cons "Some" [(compile_expr v)])
            | None -> (ident "None") in
            call "Libshoots.Prelude.slice"
                [(compile_expr v); lower; upper; step] []
        )
    | Name (id, Load) -> (AC.ident id)
    | Tuple (items, Load) -> AC.tuple (List.map compile_expr items)
    | Null -> AC.fail "hit Null expr"
    | List (elts, Load) -> AC.cons "::" (List.map compile_expr elts)
    | Name (_id, (Store|Del|AugLoad|AugStore)) -> fail_noassign ()
    | List (_id, (Store|Del|AugLoad|AugStore)) -> fail_noassign ()
    | Tuple (_id, (Store|Del|AugLoad|AugStore)) -> fail_noassign ()

and zip_compare l (ops : Cst.cmpop list) comps : Ast.expr= 
    match (ops, comps) with
    | [], [] -> (compile_expr l)
    | _, [] -> AC.fail "unbalanced comparators"
    | [], _ -> AC.fail "unbalanced comparators"
    | oh :: ot, ch :: ct -> AC.(
        call (cmp_op_ident oh) [(compile_expr l); (zip_compare ch ot ct)] []
    )

and maybe_compile_expr v = 
    match v with
    | None -> None
    | Some v -> Some (compile_expr v)

and compile_keyword (k, v) = (k, compile_expr v)

and compile_pat v : pat= 
    match v with
    | Attribute (value, id, Store) -> Attr_lookup_pat ((compile_pat value), id)
    | Name (id, Store) -> Name_pat id
    | Tuple (elts, Store) -> Tuple_pat (List.map compile_pat elts)
    | Subscript (v, Slice (lower, upper, None), Load) -> Slice_pat ((compile_pat v), (
        (maybe_compile_pat lower),
        (maybe_compile_pat upper),
        None))
    | Num (Int v) -> Int_pat v
    | Num (Float v) -> Float_pat v
    | Str v -> String_pat v
    | _ -> AC.fail "Invalid destructuring"

and maybe_compile_pat v : pat option=
    match v with
    | None -> None
    | Some v -> Some (compile_pat v)

and compile_fun_arg label (default : Ast.expr option) (arg : Cst.expr) (body : Ast.expr) = 
    match arg with
    | Name (s, _) -> (AC.fun1 (label, (Name_pat s), default, body))
    | Tuple (vals, _ctx) -> AC.fun1 (label, (Tuple_pat (List.map compile_pat vals)), 
        default, body)
    | _ -> AC.fail "invalid destructuring"

and compile_fun ctx 
        (args : Cst.expr list)
        (kwargs : Cst.keyword list)
        (body : Cst.stmt list) : Ast.expr = 
    match (args, kwargs) with 
    | [], [] -> (AC.do_ (List.map (compile_stmt ctx) body))
    | [], (name, v) :: kwargs -> compile_fun_arg
        (Labelled name) (Some (compile_expr v)) (Cst.Name (name, Load))
        (compile_fun ctx [] kwargs body)
    | arg :: args, _ -> compile_fun_arg Nolabel None arg 
        (compile_fun ctx args kwargs body)

and maybe_compile_stmts ctx s = 
    match s with 
    | None -> []
    | Some v -> List.map (compile_stmt ctx) v

and compile_stmt ctx statement = 
    let loc, statement = statement in
    AC.with_loc loc (fun () -> 
        match statement with 
        | FunctionDef (name, [], [], body) -> AC.(
            def (Name_pat name) (fun0 (do_ (List.map (compile_stmt ctx) body)))
        )
        | FunctionDef (name, args, kwargs, body) -> AC.(
            def (Name_pat name) (compile_fun ctx args kwargs body)
        )
        | Widget (spec, _args, params, body) ->
                AC.widget spec
                    (List.map (fun (id, v) -> (id, compile_expr v)) params)
                    (maybe_compile_stmts ctx body)
        | For (target, iter, body) -> 
                AC.(call "Libshoots.Prelude.for_" [
                    (cons "[]" []);
                    (compile_fun ctx [iter] [] body);
                    (compile_expr target)
                ] [])
        | Break -> (call_unit "Libshoots.Prelude.break_")
        | Continue -> (call_unit "Libshoots.Prelude.continue_")
        | If (test, body, els) -> 
                (AC.if_
                    (compile_expr test)
                    (compile_stmts ctx body)
                    (match els with 
                    | [] -> None
                    | els -> Some (compile_stmts ctx els)))
        | Let (items, body) ->
                let rec mk_bindings res items = 
                    match items with 
                    | [] -> res
                    | (_, None) :: t -> mk_bindings res t
                    | (target, Some src) :: t -> mk_bindings (
                        (compile_pat target, compile_expr src) :: res) t in
                (AC.let_ (mk_bindings [] items) (compile_stmts ctx body))
        | Require s -> add_require ctx s; AC.ident "()"
        | Import aliases -> add_import ctx aliases; (AC.ident "()")
        | ImportFrom (_from, aliases, _level) -> add_import ctx aliases;
            (AC.ident "()")
        | Assert (test, msg) -> AC.assert_ (compile_expr test)
                                        (match msg with 
                                        | None -> None
                                        | Some msg -> Some (compile_expr msg))
        | Expr expr -> compile_expr expr
    )

and compile_stmts ctx stmts = 
    (AC.do_ (List.map (compile_stmt ctx) stmts))

and compile_cst path tree = 
    let ctx = create_ctx path in 
    (ctx, (Do (List.map (compile_stmt ctx) tree)))

