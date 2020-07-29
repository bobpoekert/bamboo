(*

Portions taken from https://github.com/R1kM/ocaml-python3, based on https://docs.python.org/3/library/ast.html#abstract-grammar

Copyright (c) 2017 Aymeric Fromherz

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.*

*)

type identifier = string

and modl = 
    | Module of stmt list (* body *)
    | Interactive of stmt list (* body *)
    | Expression of expr (* body *)
    (* Only useful in Jython's typesystem *)
    | Suite of stmt list (* body *)

and stmt =
    | FunctionDef of identifier (* name *)
            * arguments (* args *)
            * stmt list (* body *)
            * expr list (* decorator list *)
            * expr option (* returns *)
    | Widget of (identifier * expr list * keyword list * stmt list option)
    | For of expr (* target *)
            * expr (* iter *)
            * stmt list (* body *)
            * stmt list (* else *)
    | While of expr (* target *)
            * stmt list (* body *)
            * stmt list (* else *)
    | If of expr (* test *)
            * stmt list (* body *)
            * stmt list (* else *)
    | With of withitem list (* items *)
            * stmt list (* body *)
    | Assert of expr (* test *)
            * expr option (* msg *)
    | Import of alias list (* names *)
    | Require of alias list (* names *)
    | ImportFrom of identifier option (* module *)
            * alias list (* names *)
            * int option (* level *)
    | Expr of expr (* value *)
    | Break
    | Continue

and expr = 
    | BoolOp of boolop (* operator *)
            * expr list (* values *)
    | BinOp of expr (* left *)
            * operator (* op *)
            * expr (* right *)
    | UnaryOp of unaryop (* op *)
            * expr (* operand *)
    | IfExp of expr (* test *)
            * expr (* body *)
            * expr (* else *)
    | Dict of expr list (* keys *)
            * expr list (* values *)
    | Set of expr list (* elts *)
    | ListComp of expr (* elt *)
            * comprehension list (* generators *)
    | SetComp of expr (* elt *)
            * comprehension list (* generators *)
    | DictComp of expr (* key *)
            * expr (* value *)
            * comprehension list (* generators *)
    | GeneratorExp of expr (* elt *)
            * comprehension list (* generators *)
    (* The grammar constraints where yield expressions can occur *)
    | Compare of expr (* left *)
            * cmpop list (* ops *)
            * expr list (* comparators *)
    | Call of expr (* func *)
            * expr list (* args *)
            * keyword list (* keywords *)
    | Num of number (* n *) (* number as PyObject *)
    | Str of string (* s *)
    | FormattedValue of expr (* value *)
            * int option (* conversion *)
            * expr option (* format_spec *)
    | JoinedStr of expr list (* values *)
    | Bytes of string (* s *)
    | NameConstant of singleton (* value *)
    | Ellipsis
(*    | Constant of constant (* value *) *)
    (* The following expression can appear in assignment context *)
    | Attribute of expr (* value *)
            * identifier (* attr *)
            * expr_context (* ctx *)
    | Subscript of expr (* value *)
            * slice (* slice *)
            * expr_context (* ctx *)
    | Starred of expr (* value *)
            * expr_context (* ctx *)
    | Name of identifier (* id *)
            * expr_context (* ctx *)
    | List of expr list (* elts *)
            * expr_context (* ctx *)
    | Tuple of expr list (* elts *)
            * expr_context (* ctx *)
    | Null (* should raise an error if accessed *)

and expr_context = 
    | Load 
    | Store
    | Del
    | AugLoad
    | AugStore
    | Param

and slice = 
    | Slice of expr option (* lower *)
            * expr option (* upper *)
            * expr option (* step *)
    | ExtSlice of slice list (* dims *)
    | Index of expr (* value *)

and boolop =
    | And 
    | Or

and operator =
    | Add
    | Sub
    | StringFmt
    | Mult
    | MatMult
    | Div
    | Mod
    | Pow
    | LShift
    | RShift
    | BitOr
    | BitXor
    | BitAnd
    | FloorDiv

and unaryop =
    | Invert
    | Not
    | UAdd
    | USub
    | WidgetId

and cmpop = 
    | Eq
    | NotEq
    | Lt
    | LtE
    | Gt
    | GtE
    | Is
    | IsNot
    | In
    | NotIn

and comprehension = expr (* target *) 
        * expr (* iter *)
        * expr list (* ifs *)
        * bool (* is_async *)

and arguments = arg list (* args *)
        * arg option (* vararg *)
        * arg list (* kwonlyargs *)
        * expr list (* kw_defaults *)
        * arg option (* kwarg *)
        * expr list (* defaults *)

and arg = identifier (* arg *)
        * expr option (* annotation *)

(* keyword arguments supplied to call (NULL identifier for **kwargs) *)
and keyword = identifier option (* arg *)
        * expr (* value *)

(* import name with optional 'as' alias *)
and alias = identifier (* name *) * identifier option (* asname *)

and withitem = expr (* context_expr *) * expr option (* optional_vars *)

and number = 
    | Int of int
    | Float of float
    | Imag of string

and singleton = 
    | True
    | False
    | SNone
