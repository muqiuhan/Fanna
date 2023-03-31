open Core

let _ =
  try
    In_channel.read_all (Sys.get_argv ()).(1)
    |> Fanna.Lexer.to_token_list
    (* |> List.iter ~f:(fun token -> token |> Fanna.Lexer.Token.to_string |> print_endline) *)
    |> Fanna.Parser.to_ast
    |> Fanna.Parser.AST.to_string
    |> fun ast -> Format.sprintf "\nAST: \n%s\n" ast |> Simlog.info
  with Failure e -> print_endline e
