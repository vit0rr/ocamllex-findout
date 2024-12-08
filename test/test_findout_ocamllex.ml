let test_lexer input =
  let lexbuf = Lexing.from_string input in
  let rec print_tokens () =
    let token = Lexer.token lexbuf in
    Printf.printf "%s\n" (Lexer.token_to_string token);
    if token <> EOF then print_tokens ()
  in
  print_tokens ()

let () =
  let input =
    "\n\
    \    {\n\
    \      \"name\": \"Vitor\",\n\
    \      \"age\": 20,\n\
    \      \"city\": \"Singapore\",\n\
    \      \"children\": [\n\
    \        { \"name\": \"Anna\", \"age\": 5 },\n\
    \        { \"name\": \"Alex\", \"age\": 3 }\n\
    \      ]\n\
    \    }\n\
    \  "
  in
  test_lexer input
