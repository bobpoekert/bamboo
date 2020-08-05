
type pat = 
    | Name of string
    | Label of string
    | Default of (pat * expr)
    | Tuple of pat list 
    | Attr_lookup of (expr * string)
    | Subscript_lookup of (expr * slice)

and typ = 
    | Ident of string
    | Metavar of string
    | Parameterized of (string * typ list)

and typed_pat = (typ * pat)

and binding = (pat * expr)

and keyword = (string * expr)

and widget_spec = 
    | Widget_name of identifier
    | Widget_id of identifier
    | Widget_cls of identifier

and func = (pat (* arg *) * expr option (* default *) * expr (* body *))

and inner_expr = 
    | Call_ident of (string (* name *) * expr list (* args *), keyword list (* kwargs *))
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
    | Tuple of expr list
    | If of (expr (* cond *) * expr (* t *) * expr option (* f *))
    | Require of string
    | Import of string
    | Do of expr list
    | Ident of string

and expr = (Location.t * inner_expr)

