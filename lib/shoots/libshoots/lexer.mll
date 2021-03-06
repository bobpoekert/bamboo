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

{

    open Menhir_parser

    exception LexError of string

    let[@inline] failwith msg = raise (LexError msg)

    let[@inline] illegal c =
          failwith (Printf.sprintf "[lexer] unexpected character: '%c'" c)


    let keywords = [
        "false", FALSE;
        "true", TRUE;
        "and", AND;
        "mod", MOD;
        "def", DEF;
        "elif", ELIF;
        "else", ELSE;
        "for", FOR;
        "break", BREAK;
        "continue", CONTINUE;
        "assert", ASSERT;
        "if", IF;
        "import", IMPORT;
        "require", REQUIRE;
        "in", IN;
        "is", IS;
        "not", NOT;
        "or", OR;
        "let", LET; 
    ]

    let id_or_kwd s = try List.assoc s keywords with _ -> IDENT s

    let newline (lexbuf:Lexing.lexbuf) =
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- {
            pos with pos_lnum = pos.pos_lnum + 1;
            pos_bol = pos.pos_cnum
        }

    let buffer = ref (Buffer.create 0)

    let stack = ref [0]
    let rec unindent n = match !stack with
        | m :: _ when m = n -> []
        | m :: st when m > n -> stack := st; DEDENT :: unindent n
        | _ -> failwith "incorrect indentation"

  let open_pars = ref 0
}

let space = ' ' | '\t' 
let endline = '\n' | '\r' | "\r\n"
let whitespace = space | endline

(* Identifiers *)

let id_continue = ['0' - '9' 'a' - 'z' 'A' - 'Z' '_'] (* TODO : Add unicode from OOO080 to 10FFFF *)
let id_start = ['a' - 'z' 'A' - 'Z' '_'] (* TODO : Add unicode from OOO080 to 10FFFF *)
let identifier = id_start id_continue*

(* Strings *)
let stringprefix =  "u" | "U" 
let rawstringprefix = "r" | "R" 
let escapeseq = "\\" _

(* Bytestrings *)
let byteprefix = "b" | "B" 
let rawbyteprefix = "br" | "Br" | "bR" | "BR" | "rb" | "rB" | "Rb" | "RB"

(* Integer literals *)

let digit = ['0' - '9']
let bindigit = ['0' '1']
let octdigit = ['0' - '7']
let hexdigit = digit | (['a' - 'f' 'A' - 'F'])
let nonzerodigit = ['1' - '9']
let decinteger = nonzerodigit (['_'] | digit)* | '0' + (['_'] '0')*
let bininteger = '0' ('b' | 'B') (['_'] bindigit)+
let octinteger = '0' ('o' | 'O') (['_'] octdigit)+
let hexinteger = '0' ('x' | 'X') (['_'] hexdigit)+
let integer = decinteger | bininteger | octinteger | hexinteger

(* Floating point literals *)

let digitpart = digit ('_'? digit)*
let exponent = ('e' | 'E') ['+' '-'] digitpart 
let fraction = '.' digitpart
let pointfloat = digitpart* fraction | digitpart "."
let exponentfloat = (digitpart | pointfloat) exponent
let floatnumber = pointfloat | exponentfloat


rule token = parse
    | space +                   { token lexbuf }
    | space* "//" [^ '\n']* endline    { token lexbuf }
    (* Line-joining *)
    | '\\' endline              { newline lexbuf; token lexbuf }
    | '\n'                      { newline lexbuf;
                                    let n = indentation lexbuf in
                                    if !open_pars > 0 then token lexbuf else
                                    match !stack with
                                        | m :: _ when m < n ->
                                            stack := n :: !stack;
                                            [NEWLINE; INDENT]
                                        | _ -> NEWLINE :: unindent n
                                }
    | identifier as id          { [id_or_kwd id] } (* TODO : Check if the identifier has a valid unicode name *)
    (* Operators *)
    | '+'                       { [ADD] }
    | '-'                       { [SUB] }
    | '*'                       { [MUL] }
    | "**"                      { [POW] }
    | '/'                       { [DIV] }
    | '%'                       { [MOD] }
    | '#'                       { [SHARP] } (* for widget id *)
    | '@'                       { [AT] }
    | "<<"                      { [LSHIFT] }
    | ">>"                      { [RSHIFT] }
    | "&"                       { [BITAND] }
    | "|"                       { [BITOR] }
    | "^"                       { [BITXOR] }
    | "~"                       { [BITNOT] }
    | "<"                       { [LT] }
    | ">"                       { [GT] }
    | "<="                      { [LE] }
    | ">="                      { [GE] }
    | "=="                      { [EQUAL] }
    | "<>"                      { [NEQ] }
    | "!="                      { [NEQ] }
    (* Delimiters *)
    | ',' space* ']'            { [COMMARSQ] }
    | ',' space* ')'            { [COMMARPAR] }
    | ',' space* '}'            { [COMMARBRA] }
    | "("                       { incr open_pars; [LPAR] }
    | ")"                       { decr open_pars; [RPAR] }
    | "["                       { [LSQ] }
    | "]"                       { [RSQ] }
    | "{"                       { [LBRACE] }
    | "}"                       { [RBRACE] }
    | "," whitespace*           { [COMMA] }
    | ":"                       { [COLON] }
    | "."                       { [DOT] }
    | ";"                       { [SEMICOL] }
    | "="                       { [EQ] }
    (* Numbers *)
    | integer as i              { [INT (int_of_string i)] }
    | floatnumber as f          { [FLOAT (float_of_string f)] }
    (* Long string literals *)
    | rawbyteprefix "\"\"\"" 
        { [BYTES (let x = unesc_long_dq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] }
    | rawstringprefix  "\"\"\"" 
        { [STR (let x = unesc_long_dq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] } 
    | byteprefix "\"\"\""
        { [BYTES (let x = long_dq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] }
    | stringprefix? "\"\"\"" 
        { [STR (let x = long_dq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] }
    | rawbyteprefix  "'''"
        { [BYTES (let x = unesc_long_sq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] } 
    | rawstringprefix  "'''" 
        { [STR (let x = unesc_long_sq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] } 
    | byteprefix  "'''"
        { [BYTES (let x = long_sq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] }
    | stringprefix? "'''" 
        { [STR (let x = long_sq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] }
    (* Short string literals *)
    | rawbyteprefix  '\''  
        { [BYTES (let x = unesc_sq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] }
    | rawstringprefix  '\''
        { [STR (let x = unesc_sq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] } 
    | byteprefix '\'' 
        { [BYTES (let x = sq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] }
    | stringprefix? '\''
        { [STR  (let x = sq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] }
    | rawbyteprefix  '"'
        { [BYTES (let x = unesc_dq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] } 
    | rawstringprefix  '"'
        { [STR (let x = unesc_dq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] } 
    | byteprefix '"'
        { [BYTES (let x = dq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] }
    | stringprefix? '"' 
        { [STR (let x = dq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] }
    | eof                       { [EOF] }
    | _ as c                    { illegal c }
    
and indentation = parse
    | space* as s   { String.length s }

and unesc_dq_prefix = parse
    | eof           { failwith "unterminated string" } 
    | "\\\n"        { newline lexbuf; unesc_dq_prefix lexbuf }
    | "\""          { [] }
    | _ as c        { (c) :: (unesc_dq_prefix lexbuf) }    

and dq_prefix = parse
    | eof           { failwith "unterminated string" } 
    | "\\\n"        { newline lexbuf; dq_prefix lexbuf }
    | "\""          { [] }
    | "\\\\"        {  '\\' :: (dq_prefix lexbuf) }
    | "\\\'"        {  '\'' :: (dq_prefix lexbuf) }
    | "\\\""        {  '"' :: (dq_prefix lexbuf)  }
    | "\\a"         { (Char.chr 7) :: (dq_prefix lexbuf) }
    | "\\b"         { ('\b') :: (dq_prefix lexbuf) }
    | "\\f"         { (Char.chr 12) :: (dq_prefix lexbuf) } 
    | "\\n"         { ('\n') :: (dq_prefix lexbuf) }
    | "\\r"         { ('\r'):: (dq_prefix lexbuf) }
    | "\\t"         { ('\t')::(dq_prefix lexbuf) }
    | "\\v"         { (Char.chr 11) :: (dq_prefix lexbuf) }
    | _ as c        { (c) :: (dq_prefix lexbuf) }    

and unesc_sq_prefix = parse
    | eof           { failwith "unterminated string" } 
    | "\\\n"        { newline lexbuf; unesc_sq_prefix lexbuf }
    | "\'"          { [] }
    | _ as c        { (c) :: (unesc_sq_prefix lexbuf) }    

and sq_prefix = parse
    | eof           { failwith "unterminated string" } 
    | "\\\n"        { newline lexbuf; sq_prefix lexbuf }
    | "\'"          { [] }
    | "\\\\"        {  '\\' :: (sq_prefix lexbuf) }
    | "\\\'"        {  '\'' :: (sq_prefix lexbuf) }
    | "\\\""        {  '"' :: (sq_prefix lexbuf)  }
    | "\\a"         { (Char.chr 7) :: (sq_prefix lexbuf) }
    | "\\b"         { ('\b') :: (sq_prefix lexbuf) }
    | "\\f"         { (Char.chr 12) :: (sq_prefix lexbuf) } 
    | "\\n"         { ('\n') :: (sq_prefix lexbuf) }
    | "\\r"         { ('\r'):: (sq_prefix lexbuf) }
    | "\\t"         { ('\t')::(sq_prefix lexbuf) }
    | "\\v"         { (Char.chr 11) :: (sq_prefix lexbuf) }
    | _ as c        { (c) :: (sq_prefix lexbuf) }    

and unesc_long_sq_prefix = parse
    | eof           { failwith "unterminated string" } 
    | "\\\n"        { newline lexbuf; unesc_long_sq_prefix lexbuf }
    | "\'\'\'"      { [] }
    | _ as c        { (c) :: (unesc_long_sq_prefix lexbuf) }    

and long_sq_prefix = parse
    | eof           { failwith "unterminated string" } 
    | "\\\n"        { newline lexbuf; long_sq_prefix lexbuf }
    | "\'\'\'"      { [] }
    | "\\\\"        {  '\\' :: (long_sq_prefix lexbuf) }
    | "\\\'"        {  '\'' :: (long_sq_prefix lexbuf) }
    | "\\\""        {  '"' :: (long_sq_prefix lexbuf)  }
    | "\\a"         { (Char.chr 7) :: (long_sq_prefix lexbuf) }
    | "\\b"         { ('\b') :: (long_sq_prefix lexbuf) }
    | "\\f"         { (Char.chr 12) :: (long_sq_prefix lexbuf) } 
    | "\\n"         { ('\n') :: (long_sq_prefix lexbuf) }
    | "\\r"         { ('\r'):: (long_sq_prefix lexbuf) }
    | "\\t"         { ('\t')::(long_sq_prefix lexbuf) }
    | "\\v"         { (Char.chr 11) :: (long_sq_prefix lexbuf) }
    | _ as c        { (c) :: (long_sq_prefix lexbuf) }    

and unesc_long_dq_prefix = parse
    | eof           { failwith "unterminated string" } 
    | "\\\n"        { newline lexbuf; unesc_long_dq_prefix lexbuf }
    | "\"\"\""      { [] }
    | _ as c        { (c) :: (unesc_long_dq_prefix lexbuf) }    

and long_dq_prefix = parse
    | eof           { failwith "unterminated string" } 
    | "\\\n"        { newline lexbuf; long_dq_prefix lexbuf }
    | "\"\"\""      { [] }
    | "\\\\"        {  '\\' :: (long_dq_prefix lexbuf) }
    | "\\\'"        {  '\'' :: (long_dq_prefix lexbuf) }
    | "\\\""        {  '"' :: (long_dq_prefix lexbuf)  }
    | "\\a"         { (Char.chr 7) :: (long_dq_prefix lexbuf) }
    | "\\b"         { ('\b') :: (long_dq_prefix lexbuf) }
    | "\\f"         { (Char.chr 12) :: (long_dq_prefix lexbuf) } 
    | "\\n"         { ('\n') :: (long_dq_prefix lexbuf) }
    | "\\r"         { ('\r'):: (long_dq_prefix lexbuf) }
    | "\\t"         { ('\t')::(long_dq_prefix lexbuf) }
    | "\\v"         { (Char.chr 11) :: (long_dq_prefix lexbuf) }
    | _ as c        { (c) :: (long_dq_prefix lexbuf) }    
(* TODO : Deal with \ooo, \xhh, \uxxxx and \Uxxxx according to the doc *)
 
 {
    (* Useful for debug *)
    let print_token = function


        | IDENT s -> "IDENT " ^ s ^" "
        | INT i -> "INT " ^ (string_of_int i) ^ " "
        | FLOAT f-> "FLOAT "^ (string_of_float f) ^ " "
        | STR s-> "STR " ^ s ^ " "
        | BYTES s -> "BYTES " ^ s ^" "
        
        | INDENT  -> "INDENT "
        | DEDENT -> "DEDENT "
        | NEWLINE -> "NEWLINE "
        | EOF -> "EOF "
        
        (* Operators *)
        | ADD -> "ADD "
        | SUB -> "SUB "
        | MUL -> "MUL "
        | POW -> "POW "
        | DIV -> "DIV "
        | MOD -> "MOD "
        | AT -> "AT "
        
        | LSHIFT -> "LSHIFT "
        | RSHIFT -> "RSHIFT "
        | BITAND -> "BITAND "
        | BITOR -> "BITOR "
        | BITXOR -> "BITXOR "
        | BITNOT -> "BITNOT "
        
        | LT -> "LT "
        | GT -> "GT "
        | LE -> "LE "
        | GE -> "GE "
        | EQUAL -> "EQUAL "
        | NEQ -> "NEQ "
        
        | SEMICOLEND -> "SEMICOLEND "(* ; at the end of a line *)
        | COMMARSQ  -> "COMMARSQ " (* , ] *)
        | COMMARPAR -> "COMMAPAR " (* , ) *)
        | COMMARBRA -> "COMMARBRA " (* , } *)
        | LPAR -> "LPAR "
        |  RPAR -> "RPAR "
        | LSQ -> "LSQ "
        | RSQ -> "RSQ "
        | LBRACE -> "LBRACE "
        | RBRACE -> "RBRACE "
        | COMMA -> "COMMA "
        | COLON -> "COLON "
        | DOT -> "DOT "
        | SEMICOL -> "SEMICOL "
        | EQ -> "EQ "
        
        | FALSE -> "FALSE "
        | TRUE -> "TRUE "
        | AND -> "AND "
        | AS -> "AS "
        | ASSERT -> "ASSERT "
        | DEF -> "DEF "
        | ELIF -> "ELIF "
        | ELSE -> "ELSE "
        | FOR -> "FOR "
        | FROM -> "FROM "
        | IF -> "IF "
        | IMPORT -> "IMPORT "
        | IN -> "IN "
        | IS -> "IS "
        | NOT -> "NOT "
        | OR -> "OR "

        | LET -> "LET "
        | SHARP -> "SHARP "
        | REQUIRE -> "REQUIRE "
        | CONTINUE -> "CONTINUE "
        | BREAK -> "BREAK "

      let next_token =
        let tokens = Queue.create () in
        fun lb -> 
            if Queue.is_empty tokens then begin
        let l = token lb in
        List.iter (fun t -> Queue.add t tokens) l
            end;
            Queue.pop tokens


}            
