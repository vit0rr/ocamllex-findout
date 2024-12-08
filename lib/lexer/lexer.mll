{
  type token =
    | STRING of string
    | NUMBER of float
    | TRUE
    | FALSE
    | NULL
    | LBRACE   (* { *)
    | RBRACE   (* } *)
    | LBRACK   (* [ *)
    | RBRACK   (* ] *)
    | COLON    (* : *)
    | COMMA    (* , *)
    | EOF

    let string_buffer = Buffer.create 256

    let token_to_string = function
    | STRING s -> "STRING(" ^ s ^ ")"
    | NUMBER n -> "NUMBER(" ^ string_of_float n ^ ")"
    | TRUE -> "TRUE"
    | FALSE -> "FALSE"
    | NULL -> "NULL"
    | LBRACE -> "LBRACE"
    | RBRACE -> "RBRACE"
    | LBRACK -> "LBRACK"
    | RBRACK -> "RBRACK"
    | COLON -> "COLON"
    | COMMA -> "COMMA"
    | EOF -> "EOF"
}

let digit = ['0'-'9']
let int = '-'? digit+
let frac = '.' digit+
let exp = ['e' 'E'] ['-' '+']? digit+
let number = int (frac)? (exp)?
let white = [' ' '\t' '\r' '\n']

rule token = parse
  | white+        { token lexbuf }
  | number as n   { NUMBER (float_of_string n) }
  | "true"        { TRUE }
  | "false"       { FALSE }
  | "null"        { NULL }
  | "{"          { LBRACE }
  | "}"          { RBRACE }
  | "["          { LBRACK }
  | "]"          { RBRACK }
  | ":"          { COLON }
  | ","          { COMMA }
  | '"'          { read_string lexbuf }
  | eof          { EOF }
  | _            { failwith ("Unexpected char: " ^ Lexing.lexeme lexbuf) }

  and read_string = parse
  | '"'                           { 
      let s = Buffer.contents string_buffer in
      Buffer.clear string_buffer;
      STRING s 
    }
  | '\\' '"'                      { Buffer.add_char string_buffer '"'; read_string lexbuf }
  | '\\' '\\'                     { Buffer.add_char string_buffer '\\'; read_string lexbuf }
  | '\\' '/'                      { Buffer.add_char string_buffer '/'; read_string lexbuf }
  | '\\' 'b'                      { Buffer.add_char string_buffer '\b'; read_string lexbuf }
  | '\\' 'f'                      { Buffer.add_char string_buffer '\012'; read_string lexbuf }
  | '\\' 'n'                      { Buffer.add_char string_buffer '\n'; read_string lexbuf }
  | '\\' 'r'                      { Buffer.add_char string_buffer '\r'; read_string lexbuf }
  | '\\' 't'                      { Buffer.add_char string_buffer '\t'; read_string lexbuf }
  | [^ '"' '\\']+                 { 
      Buffer.add_string string_buffer (Lexing.lexeme lexbuf);
      read_string lexbuf 
    }
  | _ { failwith ("Illegal string character: " ^ Lexing.lexeme lexbuf) }