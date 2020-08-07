
type pat = 
    | Name of string
    | Tuple of pat list 
    | Attr_lookup of (pat * string)
    | Slice of (pat * slice)
    | Int of int
    | Float of float
    | String of string

and function_arg_label = 
    Nolabel
    | Labelled of string
    | Optional of string

and slice = (typed_pat option (* start *) * 
             typed_pat option (* stop *) * 
             typed_pat option (* step *))

and typ = 
    | Meta of string
    | Const of (string, typ list)
    | Tuple of typ list
    | List of typ list

and typed_pat = (typ option * pat)

and binding = (typed_pat * expr)

and keyword = (string * expr)

and widget_spec = 
    | Widget_name of identifier
    | Widget_id of identifier
    | Widget_cls of identifier

and func = (function_arg_label *
            typed_pat *
            expr option (* default *) * 
            expr (* body *))

and inner_expr = 
    | Call of (expr (* op *) * expr list (* args *), keyword list (* kwargs *))
    | Field_lookup of (string * expr)
    | Cons of (string (* op *) * expr list (* elems *))
    | Let of (binding list * expr)
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

and expr = (Location.t * inner_expr)

