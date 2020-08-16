
type widget_spec = Cst.widget_spec

type pat = 
    | Name_pat of string
    | Tuple_pat of pat list 
    | Attr_lookup_pat of (pat * string)
    | Slice_pat of (pat * slice)
    | Int_pat of int
    | Float_pat of float
    | String_pat of string

and function_arg_label = 
    Nolabel
    | Labelled of string
    | Optional of string

and slice = (pat option (* start *) * 
             pat option (* stop *) * 
             pat option (* step *))

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
    | Def of binding
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

