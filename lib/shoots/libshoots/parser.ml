include Nice_parser.Make(struct 
    type result = Cst.modl
    type token = Menhir_parser.token
    exception ParseError = Menhir_parser.Error
    let parse = Menhir_parser.file_input
    include Lexer
end)
