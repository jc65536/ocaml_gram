open Ocaml_gram

let () =
  Format.set_margin 80;
  let lexbuf = Lexing.from_channel stdin in
  try
    let ast = Parser.main Lexer.token lexbuf in
    Prolog_unparse.unparse ast
  with Parser.Error ->
    Printf.printf "At offset %d: unexpected character."
      (Lexing.lexeme_start lexbuf)
