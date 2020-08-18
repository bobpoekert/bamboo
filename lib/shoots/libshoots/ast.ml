
type widget_spec = Cst.widget_spec

type pat = 
    | Name_pat of string
    | Tuple_pat of pat list 
    | Attr_lookup_pat of (pat * string)

and function_arg_label = 
    Nolabel
    | Labelled of string
    | Optional of string

and binding = (pat * expr)

and keyword = (string * expr)


and func = (function_arg_label *
            pat *
            expr option (* default *) * 
            expr (* body *))

and inner_expr = 
    | Call of (expr (* op *) * expr list (* args *) * keyword list (* kwargs *))
    | Field_lookup of (string * expr)
    | Cons of (string (* op *) * expr list (* elems *))
    | Let of (binding list * expr)
    | Def of (string (* name *) * expr (* v *))
    | Fun0 of expr (* body *)
    | Fun1 of func 
    | String of string
    | Int of int
    | Float of float
    | Widget of (widget_spec list * keyword list (* params *) * expr list (* children *))
    | Tuple of expr list
    | Record of keyword list
    | If of (expr (* cond *) * expr (* t *) * expr option (* f *))
    | Do of expr list
    | Ident of string
    | Variant of string
    | Assert of (expr (* test *) * expr option (* msg *))

and expr = (Ppxlib.Location.t * inner_expr)


let rec transform f (loc, tree) =
    let fc = (f loc) in
    (loc, match tree with
    | Call (op, args, kws) -> fc (Call (
        (transform f op), 
        (List.map (transform f) args),
        (List.map (fun (k, v) -> (k, (transform f v))) kws)))
    | Field_lookup (k, v) -> fc (Field_lookup (k, transform f v))
    | Cons (k, els) -> fc (Cons (k, (List.map (transform f) els)))
    | Let (binds, body) -> fc (Let ((List.map (transform_binding f) binds), f (fst body) (snd body)))
    | Def (name, v) -> fc (Def (name, transform f v))
    | Fun0 x -> fc (Fun0 (f x))
    | Fun1 (al, p, default, body) -> fc (Fun1 (al, p, 
        (match default with 
        | None -> None
        | Some d -> Some (f d)),
        f body))
    | String s -> fc (String s)
    | Int i -> fc (Int i)
    | Float v -> fc (Float v)
    | Widget (specs, kws, children) -> fc (Widget (specs, 
        (List.map (transform_kw f) kws), 
        (List.map (transform f) children)))
    | Tuple v -> fc (Tuple (List.map (transform f) v))
    | Record kws -> fc (Record (List.map (transform_kw f) kws))
    | If (c, t, fl) -> fc (If (f c, f t, 
        match fl with | Some v -> Some (f v) | None -> None))
    | Do vs -> fc (Do (List.map (transform f) vs))
    | Ident s -> fc (Ident s)
    | Variant s -> fc (Variant s)
    | Assert (t, msg) -> fc (Assert (f t, match msg with | Some v -> Some (f v) | None -> None)))

and transform_binding f (pat, xpr) = (pat, f xpr)

and transform_kw f (k, x) = (k, f x)
