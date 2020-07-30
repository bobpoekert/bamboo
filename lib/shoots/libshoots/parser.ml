include Nice_parser.Make(struct 
    type result = Cst.modl
    type token = Menhir_parser.token
    exception ParseError = Menhir_parser.Error
    let parse = Menhir_parser.file_input
    include Lexer
end)

let rec consume_tokens lexbuf res = 
    match Lexer.next_token lexbuf with 
    | EOF -> res
    | _ as a -> a :: (consume_tokens lexbuf res)

let tokenize infname = 
    let inf = open_in infname in 
    let lexbuf = Lexing.from_channel inf in 
    let res = consume_tokens lexbuf [] in 
    close_in inf;
    res
