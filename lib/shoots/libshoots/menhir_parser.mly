
(*

Portions taken from https://github.com/R1kM/ocaml-python3

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

 
%{
    open Cst

    (* 
    let with_pos left right thing = 
        ({
            Ppxlib.Location.loc_start = left;
            Ppxlib.Location.loc_end = right;
            loc_ghost = false;
        }, thing)
        *)

%}

%token <string> IDENT
%token <int> INT
%token <float> FLOAT
%token <string> IMAG
%token <string> STR
%token <string> BYTES

%token INDENT DEDENT NEWLINE
%token EOF

(* Operators *)
%token ADD SUB MUL POW DIV TDIV MOD
%token AT SHARP
%token LSHIFT RSHIFT BITAND BITOR BITXOR BITNOT
%token LT GT LE GE EQUAL NEQ
%token LET

%token SEMICOLEND (* ; at the end of a line *)
%token COMMARSQ   (* , ] *)
%token COMMARPAR  (* , ) *)
%token COMMARBRA  (* , } *)

(* Delimiters *)
%token LPAR RPAR LSQ RSQ LBRACE RBRACE
%token COMMA COLON DOT SEMICOL EQ ARROW

(* List of tokens for keywords *)

%token FALSE TRUE AND AS ASSERT BREAK
%token CONTINUE DEF ELIF ELSE
%token FOR FROM IF IMPORT REQUIRE IN
%token IS NOT OR

(* Entrypoint *)
%start file_input

(* Type returned *)
%type <Cst.stmt list> file_input

%type <Cst.stmt list> stmt
%type <string> dotted_name

%%

(* We omit single_input and eval_input *)

file_input:
    nl_stmt_list EOF { $1 }
;

nl_stmt_list:
    | { [] }
    | NEWLINE nl_stmt_list { $2 }
    | stmt nl_stmt_list { $1 @ $2 }
;

funcdef:
    | DEF name parameters COLON suite 
        { FunctionDef($2, $3, $5, [], None) }
    | DEF name parameters ARROW test COLON suite 
        { FunctionDef($2, $3, $7, [], Some $5) }
;

parameters:
    | LPAR typedargslist RPAR  { $2 }
    | LPAR typedargslist COMMARPAR  { $2 }
;

typedargslist:
   | { [], (None : arg option), [], [], (None : arg option), [] }
   | tfpdef { [$1], (None : arg option), [], [], (None : arg option), [Null] }
   | tfpdef EQ test { [$1], (None : arg option), [], [], (None : arg option), [$3] }
   | tfpdef COMMA typedargslist {
       match $3 with (a, va, kwon, kwdef, kwa, def) ->
       ($1 :: a, va, kwon, kwdef, kwa, Null :: def)
       }   
   | tfpdef EQ test COMMA typedargslist {
       match $5 with (a, va, kwon, kwdef, kwa, def) ->
       ($1 :: a, va, kwon, kwdef, kwa, $3 :: def)
    }
   | MUL tfpvarargs { match $2 with (va, kwon, kwdef, kwa) ->
        ( [], va, kwon, kwdef, kwa, [] ) }
   | POW tfpdef     { ([], (None : arg option), [], [], Some $2, []) }
;

tfpdef:
    | name { ($1, (None : expr option)) }
    | name COLON test { ($1, Some $3) }
;

tfpvarargs:
    | tfpdef tfpkwonly_args { 
        match $2 with (kwonly, kwdef, kwargs) ->
        (Some $1, kwonly, kwdef, kwargs) }
    | COMMA tfpdef tfpkwonly_args {
            match $3 with (kwonly, kwdef, kwarg) ->
            ((None : arg option), $2 :: kwonly, Null :: kwdef, kwarg)
        }
    | COMMA tfpdef EQ test tfpkwonly_args {
        match $5 with (kwonly, kwdef, kwarg) ->
            ((None : arg option), $2 :: kwonly, $4 :: kwdef, kwarg)
        }
;        

tfpkwonly_args:
    | COMMA tfpdef tfpkwonly_args { match $3 with
            ( kwonly, kwdef, kwarg) ->
            ($2 :: kwonly, Null :: kwdef, kwarg)
        }
    | COMMA tfpdef EQ test tfpkwonly_args {
        match $5 with (kwonly, kwdef, kwarg) ->
            ($2 :: kwonly, $4 :: kwdef, kwarg)
        }
    | COMMA tfpkwargs { ([], [], Some $2) }
    | { [], [], (None : arg option) }
;

tfpkwargs:
    POW tfpdef { $2 }
;


stmt:
    | simple_stmt { $1 : stmt list } 
    | compound_stmt { List.cons $1 [] }
;

simple_stmt:
    | separated_nonempty_list(SEMICOL, small_stmt) NEWLINE      { $1 }
    | separated_nonempty_list(SEMICOL, small_stmt) SEMICOLEND   { $1 }
;

small_stmt:
    | expr_stmt    { $1 }
    | flow_stmt    { $1 }
    | import_stmt  { $1 }
    | assert_stmt  { $1 }
    | widget_small { $1 }
;    

expr_stmt:
    | testlist_star_expr    { Expr $1 }
;


test_starexpr:
    | test      { $1 }
    | star_expr { $1 }
;

testlist_star_expr:
    | nonempty_list(terminated(test_starexpr, COMMA))    { Tuple ($1, Load) }
    | separated_nonempty_list(COMMA, test_starexpr)         { match $1 with 
        | [s] -> s
        | l -> Tuple (l, Load) }
;



flow_stmt:
    | break_stmt    { $1 }
    | continue_stmt { $1 }
;

break_stmt:
    BREAK         { Break }
;

continue_stmt:
    CONTINUE      { Continue }
;


import_stmt:
    | import_name   { $1 }
    | import_from   { $1 }
;

import_name:
    | IMPORT dotted_as_names  { Import $2 }
    | REQUIRE strings { Require $2 }
;

import_from:
    | FROM importfrom_module IMPORT MUL    
         { ImportFrom (fst $2, ["*", None], snd $2) }
    | FROM importfrom_module IMPORT LPAR import_as_names RPAR 
        { ImportFrom (fst $2, $5, snd $2) }
    | FROM importfrom_module IMPORT import_as_names  
        { ImportFrom (fst $2, $4, snd $2) }
;

importfrom_module:
    | dotted_name   { Some $1, Some 0 }
    | dot_level dotted_name { Some $2, Some $1 }
    | DOT dot_level     { (None : identifier option), Some (1 + $2) }
;    
     
dot_level:
    | DOT dot_or_zero { 1 + $2 }
;

dot_or_zero:
    | { 0 }
    | DOT dot_or_zero { 1 + $2 }
;

import_as_name:
    | name          { $1, None }
    | name AS name  { $1, Some $3 }
;

dotted_as_name:
    | dotted_name           {  $1, (None : identifier option)  }
    | dotted_name AS name   {  $1, Some $3  }
;

import_as_names:
    | import_as_name COMMA?     { [$1] }
    | import_as_name COMMA import_as_names { $1 :: $3 }
;

dotted_as_names:
    | dotted_as_name        { [$1] }
    | dotted_as_name COMMA dotted_as_names { $1 :: $3 }
;

dotted_name:
    | name                  { $1 }
    | name DOT dotted_name  { $1 ^ "." ^ $3 }  
;


assert_stmt:
    | ASSERT test            { Assert ($2, None) }
    | ASSERT test COMMA test { Assert ($2, Some $4) }
;    

compound_stmt:
    | if_stmt       { $1 }
    | for_stmt      { $1 }
    | let_stmt     { $1 }
    | funcdef       { $1 }
    | widget_block  { $1 }
;

widget_spec: 
    | { [] }
    | name widget_spec       { (Widget_name $1) :: $2 }
    | DOT name widget_spec   { (Widget_cls $2) :: $3 }
    | SHARP name widget_spec { (Widget_id $2) :: $3 }
;

widget_block:
    | MOD widget_spec LPAR RPAR COLON suite { Widget($2, [], [], Some $6) }
    | MOD widget_spec LPAR arglist RPAR COLON suite { Widget ($2, fst $4, snd $4, Some $7) }
;

widget_small:
    | MOD widget_spec LPAR RPAR { Widget($2, [], [], None) }
    | MOD widget_spec LPAR arglist RPAR { Widget ($2, fst $4, snd $4, None) }
;


if_stmt:
    | IF test COLON suite elif_else { If ($2, $4, $5) }
;

elif_else:
    | { [] }
    | ELIF test COLON suite elif_else { [ If ($2, $4, $5) ] }
    | ELSE COLON suite      { $3 }
;        
    
for_stmt:
    | FOR exprlist IN testlist COLON suite  { match $2 with
        | [e]  -> For (e, $4, $6, [])
        | l -> For (Tuple(l, Store), $4, $6, []) }
    | FOR exprlist IN testlist COLON suite ELSE COLON suite    { match $2 with
        | [e] -> For (e, $4, $6, $9)
        | l -> For (Tuple(l, Store), $4, $6, $9) }
;


let_stmt:
    LET let_items COLON suite    { Let ( $2, $4) }
;

let_items:
    separated_nonempty_list(COMMA, let_item) { $1 }
;

let_item:
    | test          { ( $1, (None : expr option) ) }
    | test EQ expr  { ( $1, Some $3) }
;

suite:
    | simple_stmt                   { $1 }
    | NEWLINE INDENT stmt+ DEDENT   { List.concat $3 }   
;

test:
    | or_test                       { $1 }         
    | or_test IF or_test ELSE test  { IfExp($3, $1, $5) }
;

test_nocond:
    | or_test           { $1 }
;    

or_test:
    | separated_nonempty_list(OR, and_test) { match $1 with
        | [s] -> s
        | l -> BoolOp(Or, l) }
;

and_test:
    | separated_nonempty_list(AND, not_test)    { match $1 with
        | [s] -> s
        | l -> BoolOp(And, l) }
;        

not_test:
    | NOT not_test      { UnaryOp(Not, $2) }
    | comparison        { $1 }
;

comparison:
    | expr              { $1 }
    | expr comp_list    { Compare ($1, fst $2, snd $2) }
;

comp_list:
    | comp_op expr comp_list_empty    { $1 :: (fst $3), $2 :: (snd $3) }
;            

comp_list_empty:
    | { [], [] }
    | comp_op expr comp_list_empty    { $1 :: (fst $3), $2 :: (snd $3) }
;

comp_op:
   | LT             { Lt }
   | GT             { Gt }
   | EQUAL          { Eq }
   | GE             { GtE }
   | LE             { LtE }
   | NEQ            { NotEq }
   | IN             { In }
   | NOT IN         { NotIn }
   | IS             { Is }
   | IS NOT         { IsNot }
;

star_expr:
    MUL expr       { Starred ($2, Load) }
;     

expr:
    | xor_expr      { $1 }
    | expr BITOR xor_expr { BinOp($1, BitOr, $3) }
;

xor_expr:
    | and_expr      { $1 }
    | xor_expr BITXOR and_expr  { BinOp($1, BitXor, $3) }
;

and_expr:
    | shift_expr                    { $1 }
    | and_expr BITAND shift_expr    { BinOp($1, BitAnd, $3) }
;

shift_expr:
    | arith_expr                    { $1 }
    | shift_expr LSHIFT arith_expr  { BinOp($1, LShift, $3) }
    | shift_expr RSHIFT arith_expr  { BinOp($1, RShift, $3) }
;

arith_expr:
    | term                         { $1 }
    | arith_expr ADD term          { BinOp($1, Add, $3) }    
    | arith_expr SUB term          { BinOp($1, Sub, $3) }    
;

term:
    | factor                { $1 }
    | term term_op factor   { BinOp ($1, $2, $3) }
;

term_op:
    | MUL   { Mult }      
    | AT    { MatMult }
    | DIV   { Div }
    | MOD   { Mod }
    | TDIV  { FloorDiv }
;    

factor:
    | power             { $1 }
    | factor_op factor  { UnaryOp ($1, $2) }
;

factor_op:
    | ADD       { UAdd }
    | SUB       { USub }
    | BITNOT    { Invert }
    | SHARP     { WidgetId }
;
            
power:
    | atom_expr             { $1 }            
    | atom_expr POW factor  { BinOp($1, Pow, $3) }
;

atom_expr:
    | atom_trailer          { $1 }
;

atom_trailer:
    | atom                      { $1 }
    | atom_trailer LPAR RPAR    { Call ($1, [], []) }
    | atom_trailer LPAR arglist RPAR { Call ($1, fst $3, snd $3) }
    | atom_trailer LPAR arglist COMMARPAR { Call ($1, fst $3, snd $3) }
    | atom_trailer LSQ subscriptlist RSQ { match $3 with
        | [s] -> Subscript ($1, s, Load)
        | l -> Subscript ($1, ExtSlice l, Load)
        }
    | atom_trailer LSQ subscriptlist COMMARSQ {Subscript ($1, ExtSlice $3, Load) }
    | atom_trailer DOT name     { Attribute ($1, $3, Load) }
;

atom:
    | atom_tuple        { $1 }
    | atom_list         { $1 }
    | atom_dict         { $1 }
    | name              { Name ($1, Load) }
    | number            { Num $1 }
    | strings           { Str $1 }
    | bytes             { Bytes $1 }
    | DOT DOT DOT       { Ellipsis }
    | TRUE              { NameConstant True }
    | FALSE             { NameConstant False }
;     
 
(* Iterable cannot be used in comprehension : Star_expr forbidden *)
atom_tuple:
    | LPAR RPAR                 { Tuple ([], Load) }
    | LPAR separated_nonempty_list(COMMA, test_starexpr) RPAR   { match $2 with
        | [s] -> s
        | l -> Tuple(l, Load) }
    | LPAR separated_nonempty_list(COMMA, test_starexpr) COMMARPAR   { Tuple($2, Load) }
    | LPAR test comp_for RPAR   { GeneratorExp($2, $3) }
;

(* Iterable cannot be used in comprehension : Star_expr forbidden *)
atom_list:
    | LSQ RSQ               { List ([], Load) }
    | LSQ separated_nonempty_list(COMMA, test_starexpr) RSQ { List ($2, Load) }
    | LSQ separated_nonempty_list(COMMA, test_starexpr) COMMARSQ { List ($2, Load) }
    | LSQ test comp_for RSQ { ListComp($2, $3) }
;

(* Iterable cannot be used in comprehension : Star_expr forbidden *)
atom_dict:
    | LBRACE RBRACE                 { Dict ([], []) }
    | LBRACE test COLON test comp_for RBRACE  { DictComp ($2, $4, $5) }
    | LBRACE dict_elts  RBRACE      { Dict (fst $2, snd $2) }
    | LBRACE dict_elts  COMMARBRA   { Dict (fst $2, snd $2) }
    | LBRACE test comp_for RBRACE   { SetComp($2, $3) }
    | LBRACE separated_nonempty_list(COMMA, test_starexpr) RBRACE  { Set $2 } 
    | LBRACE separated_nonempty_list(COMMA, test_starexpr) COMMARBRA  { Set $2 } 
;

number:
    | INT   { Int $1 }
    | FLOAT { Float $1 }
    | IMAG  { Imag $1 }
;

strings:
    | STR           { $1 }
    | STR strings   { ($1) ^ ($2) }
;

bytes:
    | BYTES         { $1 }
    | BYTES bytes   { ($1) ^ ($2) }
;    

subscriptlist:
    separated_nonempty_list(COMMA, subscript) { $1 }
;

subscript:
    | test                      { Index $1 }
    | test? COLON test?     { Slice ($1,$3, None) }
    | test? COLON test? COLON test?     { Slice ($1,$3, $5) }
;

expr_star:
    | expr          { $1 }
    | star_expr     { $1 }
;

exprlist:
   | nonempty_list(terminated(expr_star, COMMA))  { $1 }
   | separated_nonempty_list(COMMA, expr_star)    { $1 }
;
    
testlist:
   | nonempty_list(terminated(test, COMMA)) { Tuple($1, Load) } 
   | separated_nonempty_list(COMMA, test)  { match $1 with
        | [s] -> s
        | l -> Tuple (l, Load) }
;

dict_elts:
    | test COLON test  { [$1], [$3] }
    | POW expr         { [Null], [$2] }
    | test COLON test COMMA dict_elts   { $1 :: (fst $5), $3 :: (snd $5)  }
    | POW expr COMMA dict_elts   { Null :: (fst $4), $2 :: (snd $4) }
;

arglist:
    separated_nonempty_list(COMMA, argument)  { 
        let rec result = function
            | [(a, b)] -> a, b
            | (a, b) :: l -> let (c, d) = result l in a @ c, b @ d
            | _ -> assert false 
        in result $1            
        }
;

argument:
    | test          { ([$1], [] ) } 
    | test comp_for { ([GeneratorExp($1, $2)], []) }
    | name EQ test  { ([], [(Some $1, $3)]) }
    | POW test      { ([], [((None : identifier option), $2)]) }
    | MUL test      { ([$2], []) }
;    

comp_for:
    | nonempty_list(comp_for1)    { $1 }  
;

comp_for1:
    | FOR exprlist IN or_test list(comp_if) { match $2 with
        | [s] -> (s, $4, $5)
        | l -> (Tuple(l, Store), $4, $5) }
;          

comp_if:
    IF test_nocond  { $2 }
;


name:
    IDENT   { $1 }
;
