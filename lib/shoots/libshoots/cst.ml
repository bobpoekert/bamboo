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

and widget = (widget_spec list * expr list * keyword list * stmt list option)

and inner_stmt =
    | FunctionDef of identifier (* name *)
            * arguments (* args *)
            * stmt list (* body *)
            * expr list (* decorator list *)
            * expr option (* return type *)
    | Widget of widget
    | For of expr (* target *)
            * expr (* iter *)
            * stmt list (* body *)
    | If of expr (* test *)
            * stmt list (* body *)
            * stmt list (* else *)
    | Let of letitem list (* items *)
            * stmt list (* body *)
    | Assert of expr (* test *)
            * expr option (* msg *)
    | Import of alias list (* names *)
    | Require of string (* names *)
    | ImportFrom of identifier option (* module *)
            * alias list (* names *)
            * int option (* level *)
    | Expr of expr (* value *)
    | Break
    | Continue

and stmt = (Ppxlib.Location.t * inner_stmt)

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
    (* The following expression can appear in assignment context *)
    | Attribute of expr (* value *)
            * identifier (* attr *)
            * expr_context (* ctx *)
    | Subscript of expr (* value *)
            * slice (* slice *)
            * expr_context (* ctx *)
    | Name of identifier (* id *)
            * expr_context (* ctx *)
    | Tuple of expr list (* elts *)
            * expr_context (* ctx *)
    | List of expr list (* elts *)
            * expr_context (* ctx *)

    | Null (* should raise an error if accessed *)


and widget_spec = 
    | Widget_name of identifier
    | Widget_id of identifier
    | Widget_cls of identifier

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

and unaryop =
    | Invert
    | Not
    | UAdd
    | USub

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

and arguments = arg list (* args *)
        * arg option (* vararg *)
        * arg list (* kwonlyargs *)
        * expr list (* kw_defaults *)
        * arg option (* kwarg *)
        * expr list (* defaults. empty defaults are Null *)

and arg = identifier (* arg *)
        * expr option (* annotation *)

(* keyword arguments supplied to call (NULL identifier for **kwargs) *)
and keyword = identifier option (* arg *)
        * expr (* value *)

(* import name with optional 'as' alias *)
and alias = identifier (* name *) * identifier option (* asname *)

and letitem = expr (* assign target *) * expr option (* assign source *)

and number = 
    | Int of int
    | Float of float

and singleton = 
    | True
    | False
    | SNone


