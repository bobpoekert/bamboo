
type node = 
    | Statement of Cst.stmt
    | Expr of Cst.expr
    | Ocaml of Parsetree.expression
