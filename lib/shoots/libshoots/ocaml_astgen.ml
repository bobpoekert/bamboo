open Ppxlib.Ast_builder.Default
open Parsetree

let rec extract_requires tree =
    match tree with 
    | [] -> res 
    | Require r :: t -> r :: (extract_requires t)

let do_expr exprs = 
    List.fold_left pexp_sequence exprs

exception SyntaxError of (string * Location.t)

let pp_exceptions () = begin
    Location.register_error_of_exn (function
        | SyntaxError (msg, loc) ->
                Some (Location.error ~loc msg)
        | _ ->
                None
    );
    Printexc.register_printer (function exn ->
        try
            ignore (Format.flush_str_formatter ());
            Location.report_exception Format.str_formatter exn;
            Some (Format.flush_str_formatter ());
        with _ ->
            None
    );
end

let fail_loc msg loc = raise SyntaxError (msg, loc)
let warn_loc _msg _loc = ()

let ast_unit = pexp_ident "()"
let ast_cons = pexp_ident "::"

let cons_expr loc l r = 
    pexp_construct ~loc ast_cons (pexp_tuple ~loc [l; r]) in

let list_literal loc exprs = 
    List.fold_right (cons_expr loc) (pexp_construct ~loc (pexp_ident "[]")) exprs

module type UICompiler = sig
    val make_widget : string -> arg list -> Parsetree.expression
    val prelude : Parsetree.expression
end

module Pat = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
        {
            ppat_desc = d;
            ppat_loc = loc;
            ppat_loc_stack = [];
            ppat_attributes = attrs;
        }
    let attr d a = {d with ppat_attributes = d.ppat_attributes @ [a]}

    let any ?loc ?attrs () = mk ?loc ?attrs Ppat_any
    let var ?loc ?attrs a = mk ?loc ?attrs (Ppat_var a)
    let alias ?loc ?attrs a b = mk ?loc ?attrs (Ppat_alias (a, b))
    let constant ?loc ?attrs a = mk ?loc ?attrs (Ppat_constant a)
    let interval ?loc ?attrs a b = mk ?loc ?attrs (Ppat_interval (a, b))
    let tuple ?loc ?attrs a = mk ?loc ?attrs (Ppat_tuple a)
    let construct ?loc ?attrs a b = mk ?loc ?attrs (Ppat_construct (a, b))
    let variant ?loc ?attrs a b = mk ?loc ?attrs (Ppat_variant (a, b))
    let record ?loc ?attrs a b = mk ?loc ?attrs (Ppat_record (a, b))
    let array ?loc ?attrs a = mk ?loc ?attrs (Ppat_array a)
    let or_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_or (a, b))
    let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_constraint (a, b))
    let type_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_type a)
    let lazy_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_lazy a)
    let unpack ?loc ?attrs a = mk ?loc ?attrs (Ppat_unpack a)
    let open_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_open (a, b))
    let exception_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_exception a)
    let extension ?loc ?attrs a = mk ?loc ?attrs (Ppat_extension a)
end

module Make(UI : UICompiler) = struct

    let rec num_to_parsetree loc n =
        match n with
        | Int n -> Pconst_integer ((int_to_string n), None)
        | Float n -> Pconst_float ((float_to_string n), None)

    and op_ident op =
        match op with
        | Add -> pexp_ident "+"
        | Sub -> pexp_ident "-"
        | StringFmt -> evar "Printf.sprintf"
        | Mult -> pexp_ident "*"
        | MatMult -> pexp_ident "*"
        | Div -> pexp_ident "/"
        | Mod -> pexp_ident "mod"
        | Pow -> pexp_ident "**"
        | LShift -> pexp_ident "lsl"
        | RShift -> pexp_ident "lsr"
        | BitOr -> pexp_ident "lor"
        | BitXor -> pexp_ident "lxor"
        | BitAnd -> pexp_ident "land"

    and unary_op_ident op = 
        match op with
        | Invert -> pexp_ident "lnot"
        | Not -> pexp_ident "not"
        | UAdd -> pexp_ident "~+"
        | USub -> pexp_ident "~-"

    and bin_op_to_parsetree loc (l, op, r) = 
        let l = expr_to_parsetree loc l in 
        let r = expr_to_parsetree loc r in 
        let op = op_ident op in 
        pexp_apply ~loc op [(Nolabel, l); (Nolabel, r)]

    and unary_op_to_parsetree loc (op, expr) =
        let op = unary_op_ident op in 
        let expr = expr_to_parsetree loc expr in 
        pexp_apply ~loc op [(Nolabel, expr)]

    and and_all exprs =
        List.fold_right 
            (fun res e -> pexp_apply ~loc (op_ident And) 
                [(Nolabel, res); (Nolabel, e)])
            (List.hd exprs) (List.tl exprs)

    and compare_to_parsetree loc (left, ops, comps) = 
        let rec iter res ops comps = 
            if ops == [] then res else
                let op :: ops = ops in 
                let comp :: comps = comps in 
                pexp_apply ~loc (comp_op_ident op) [
                    (Nolabel, res); (Nolabel, (iter res ops comps))] in
        iter left ops comps

    and call_to_parsetree loc (func, args, keywords) = 
        let func = expr_to_parsetree loc func in 
        let args = List.map (fun v -> (Nolabel, expr_to_parsetree loc v)) args in 
        let keywords = List.map 
            (fun (k, v) -> ((Labelled k), (expr_to_parsetree loc v)))
            keywords in 
        pexp_apply ~loc func (args @ keywords)

    (* multi-arg functions are really, actually curried single-arg functions *)

    and arg_pat loc arg = 
        match arg with
        | None, (Name (vname, _)) -> ppat_var ~loc vname
        | None, (Tuple (elts, _)) -> ppat_tuple ~loc (List.map (arg_pat loc) elts)
        | Some id, default -> ppat_var ~loc id 

    and arg_id loc arg = 
        match arg with 
        | None, _ -> Nolabel
        | Some id, _ -> Optional id

    and function_decl loc args body = 
        match args with 
        | [] -> pexp_fun ~loc
                    Nolabel
                    (ppat_construct ~loc "()" None)
                    (cst_to_parsetree body)
        | [a] -> pexp_fun ~loc
                    (arg_id loc a)
                    (arg_pat loc a)
                    (cst_to_parsetree body)
        | h :: t -> pexp_fun ~loc
                        (arg_id loc h)
                        (arg_pat loc h)
                        (function_decl loc t body)

    and let_binding loc bindings body = 
        pexp_let ~loc Nonrecursive 
            (List.map
                (fun (k, v) -> 
                    (value_binding ~loc
                        (arg_pat loc k)
                        (expr_to_parsetree loc v)))
                bindings)
            (cst_to_parsetree body)


    and keyword_to_parsetree loc (id, expr) = (id, (expr_to_parsetree loc expr))

    and expr_to_parsetree loc expr =
        match expr with 
        | BoolOp _ -> fail_loc "Hit bool op at wrong point (compiler bug)" loc
        | BinOp v -> bin_op_to_parsetree loc v 
        | UnaryOp v -> unary_op_to_parsetree loc v
        | IfExp (cond, t, f) -> pexp_ifthenelse ~loc:loc
            (expr_to_parsetree loc cond)
            (expr_to_parsetree loc t)
            (Some (expr_to_parsetree loc f))
        | Dict (ks, vs) -> list_literal loc (
            List.map2 (fun k v ->
                pexp_tuple ~loc [(expr_to_parsetree loc k); (expr_to_parsetree loc v)]) ks vs)
        | Set items -> list_literal loc (List.map (expr_to_parsetree loc) items)
        | Compare v -> compare_to_parsetree loc v
        | Call v -> call_to_parsetree loc v
        | Num n -> num_to_parsetree loc n
        | Str s -> (Pconst_string s None)
        | JoinedStr parts -> join_op (pexp_ident "^") loc parts
        | Bytes s -> (Pconst_string s None)
        | NameConstant True -> pexp_construct ~loc (pexp_ident "true")
        | NameConstant False -> pexp_construct ~loc (pexp_ident "false")
        | NameConstant SNone -> pexp_construct ~loc (pexp_ident "()")
        | Attribute _ -> fail_loc "Inappropriate attribute" loc
        | Subscript _ -> fail_loc "Inappropriate subscript" loc
        | Name (id, Load) -> pexp_desc ~loc (pexp_ident id)
        | Name (id, Param) -> (Pconst_string id None)
        | Name (_id, _) -> fail_loc "Invalid name context" loc
        | List (els, Load) -> list_literal loc (List.map (expr_to_parsetree loc) els)
        | List (_, _) -> fail_loc "List literal not load (should be impossible)" loc
        | Tuple (els, Load) -> pexp_tuple ~loc (List.map (expr_to_parsetree loc) els)
        | Tuple (_, _) -> fail_loc "Tuple literal not load (impossible)" loc
        | Null -> fail_loc "Unknown parse error" loc

    and assert_to_parsetree loc v =
        pexp_assert ~loc:loc (expr_to_parsetree loc v)

    and fdef_to_parsetree widget_gen loc (name, args, body, decorators, returns) next = 
        if returns != None then fail_loc "stub of a return statement appeared" loc;
        let args = arguments_to_ppat loc args in 
        let body = List.map (stmt_to_parsetree widget_gen loc) body in 
        let_binding loc name
            (List.fold_left (fun res (e, args) -> function_call e args @ [res])
                (function_decl args body)
                decorators)
            (stmt_to_parsetree next)

    and let_to_parsetree loc (items, body) = 
        let_binding loc items (List.map (stmt_to_parsetree loc) body)

    and stmt_to_parsetree stmts = 
        match stmts with
        | [] -> pexp_construct ast_unit None
        | (loc, stmt) :: t ->
            match stmt with 
            | Widget spec, args, props, children -> 
                    UI.make_widget spec name
                        (List.map expr_to_parsetree loc args) 
                        (List.map keyword_to_parsetree loc props) begin
                        match children with
                        | None -> None
                        | Some v -> Some (List.map stmt_to_parsetree v)
                    end
            | FunctionDef func -> fdef_to_parsetree widget_gen loc func t
            | For f -> for_to_parsetree loc f t
            | If f -> if_to_parsetree loc f t
            | Let f -> let_to_parsetree loc f t
            | Assert v -> assert_to_parsetree loc v t
            | Require _ -> stmt_to_parsetree t (* we already parsed these *)
            | ImportFrom _ -> stmt_to_parsetree t (* TODO: parse these in prev pass *)
            | Expr e -> pexp_sequence ~loc (expr_to_parsetree loc e) (stmt_to_parsetree t)
            | Break -> fail_loc "Break must be in a for loop" loc
            | Continue -> fail_loc "Continue must be in a for loop" loc

    let cst_to_parsetree (inp:Cst.stmt list) = 
        let requires = extract_requires inp in 
        pexp_sequence UI.prelude (stmt_to_parsetree inp)

end
