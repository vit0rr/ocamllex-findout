# 1 "lib/lexer/lexer.mll"
 
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

# 34 "lib/lexer/lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\242\255\243\255\244\255\245\255\246\255\247\255\248\255\
    \249\255\250\255\000\000\000\000\000\000\014\000\024\000\005\000\
    \079\000\089\000\099\000\109\000\121\000\001\000\000\000\253\255\
    \000\000\004\000\002\000\252\255\001\000\003\000\251\255\002\000\
    \004\000\133\000\255\255\247\255\248\255\249\255\250\255\251\255\
    \252\255\253\255\254\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\013\000\013\000\013\000\001\000\013\000\000\000\
    \255\255\255\255\001\000\001\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \009\000\010\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\000\000\255\255\255\255\000\000\032\000\
    \032\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\015\000\015\000\000\000\000\000\015\000\015\000\015\000\
    \000\000\000\000\015\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \015\000\000\000\003\000\000\000\034\000\015\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\004\000\014\000\000\000\000\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\005\000\000\000\017\000\000\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\000\000\016\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\007\000\000\000\006\000\033\000\000\000\
    \255\255\024\000\000\000\000\000\000\000\023\000\011\000\027\000\
    \000\000\000\000\000\000\000\000\025\000\029\000\010\000\030\000\
    \000\000\000\000\021\000\016\000\012\000\028\000\022\000\026\000\
    \000\000\000\000\020\000\009\000\020\000\008\000\000\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\042\000\
    \016\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\000\000\040\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\041\000\000\000\000\000\000\000\000\000\000\000\039\000\
    \000\000\000\000\000\000\038\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\037\000\000\000\000\000\000\000\036\000\
    \000\000\035\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\000\000\255\255\000\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\255\255\000\000\015\000\015\000\
    \255\255\255\255\015\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\255\255\031\000\015\000\032\000\255\255\
    \255\255\255\255\255\255\255\255\000\000\000\000\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\013\000\255\255\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\255\255\013\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\000\000\031\000\255\255\
    \032\000\011\000\255\255\255\255\255\255\022\000\000\000\026\000\
    \255\255\255\255\255\255\255\255\024\000\028\000\000\000\029\000\
    \255\255\255\255\012\000\013\000\000\000\010\000\021\000\025\000\
    \255\255\255\255\016\000\000\000\016\000\000\000\255\255\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\033\000\
    \018\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\255\255\033\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \018\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\033\000\255\255\255\255\255\255\255\255\255\255\033\000\
    \255\255\255\255\255\255\033\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\033\000\255\255\255\255\255\255\033\000\
    \255\255\033\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\031\000\255\255\032\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec token lexbuf =
   __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 41 "lib/lexer/lexer.mll"
                  ( token lexbuf )
# 178 "lib/lexer/lexer.ml"

  | 1 ->
let
# 42 "lib/lexer/lexer.mll"
              n
# 184 "lib/lexer/lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 42 "lib/lexer/lexer.mll"
                  ( NUMBER (float_of_string n) )
# 188 "lib/lexer/lexer.ml"

  | 2 ->
# 43 "lib/lexer/lexer.mll"
                  ( TRUE )
# 193 "lib/lexer/lexer.ml"

  | 3 ->
# 44 "lib/lexer/lexer.mll"
                  ( FALSE )
# 198 "lib/lexer/lexer.ml"

  | 4 ->
# 45 "lib/lexer/lexer.mll"
                  ( NULL )
# 203 "lib/lexer/lexer.ml"

  | 5 ->
# 46 "lib/lexer/lexer.mll"
                 ( LBRACE )
# 208 "lib/lexer/lexer.ml"

  | 6 ->
# 47 "lib/lexer/lexer.mll"
                 ( RBRACE )
# 213 "lib/lexer/lexer.ml"

  | 7 ->
# 48 "lib/lexer/lexer.mll"
                 ( LBRACK )
# 218 "lib/lexer/lexer.ml"

  | 8 ->
# 49 "lib/lexer/lexer.mll"
                 ( RBRACK )
# 223 "lib/lexer/lexer.ml"

  | 9 ->
# 50 "lib/lexer/lexer.mll"
                 ( COLON )
# 228 "lib/lexer/lexer.ml"

  | 10 ->
# 51 "lib/lexer/lexer.mll"
                 ( COMMA )
# 233 "lib/lexer/lexer.ml"

  | 11 ->
# 52 "lib/lexer/lexer.mll"
                 ( read_string lexbuf )
# 238 "lib/lexer/lexer.ml"

  | 12 ->
# 53 "lib/lexer/lexer.mll"
                 ( EOF )
# 243 "lib/lexer/lexer.ml"

  | 13 ->
# 54 "lib/lexer/lexer.mll"
                 ( failwith ("Unexpected char: " ^ Lexing.lexeme lexbuf) )
# 248 "lib/lexer/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and read_string lexbuf =
   __ocaml_lex_read_string_rec lexbuf 31
and __ocaml_lex_read_string_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 57 "lib/lexer/lexer.mll"
                                  ( 
      let s = Buffer.contents string_buffer in
      Buffer.clear string_buffer;
      STRING s 
    )
# 264 "lib/lexer/lexer.ml"

  | 1 ->
# 62 "lib/lexer/lexer.mll"
                                  ( Buffer.add_char string_buffer '"'; read_string lexbuf )
# 269 "lib/lexer/lexer.ml"

  | 2 ->
# 63 "lib/lexer/lexer.mll"
                                  ( Buffer.add_char string_buffer '\\'; read_string lexbuf )
# 274 "lib/lexer/lexer.ml"

  | 3 ->
# 64 "lib/lexer/lexer.mll"
                                  ( Buffer.add_char string_buffer '/'; read_string lexbuf )
# 279 "lib/lexer/lexer.ml"

  | 4 ->
# 65 "lib/lexer/lexer.mll"
                                  ( Buffer.add_char string_buffer '\b'; read_string lexbuf )
# 284 "lib/lexer/lexer.ml"

  | 5 ->
# 66 "lib/lexer/lexer.mll"
                                  ( Buffer.add_char string_buffer '\012'; read_string lexbuf )
# 289 "lib/lexer/lexer.ml"

  | 6 ->
# 67 "lib/lexer/lexer.mll"
                                  ( Buffer.add_char string_buffer '\n'; read_string lexbuf )
# 294 "lib/lexer/lexer.ml"

  | 7 ->
# 68 "lib/lexer/lexer.mll"
                                  ( Buffer.add_char string_buffer '\r'; read_string lexbuf )
# 299 "lib/lexer/lexer.ml"

  | 8 ->
# 69 "lib/lexer/lexer.mll"
                                  ( Buffer.add_char string_buffer '\t'; read_string lexbuf )
# 304 "lib/lexer/lexer.ml"

  | 9 ->
# 70 "lib/lexer/lexer.mll"
                                  ( 
      Buffer.add_string string_buffer (Lexing.lexeme lexbuf);
      read_string lexbuf 
    )
# 312 "lib/lexer/lexer.ml"

  | 10 ->
# 74 "lib/lexer/lexer.mll"
      ( failwith ("Illegal string character: " ^ Lexing.lexeme lexbuf) )
# 317 "lib/lexer/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_string_rec lexbuf __ocaml_lex_state

;;

