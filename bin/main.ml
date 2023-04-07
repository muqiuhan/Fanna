open Core

let _ =
  try
    In_channel.read_all (Sys.get_argv ()).(1)
    |> Fanna.Lexer.to_token_list
    |> Fanna.Parser.to_ast
    |> List.iter ~f:(fun ast -> Fanna.AST.show ast |> print_endline)
  with Failure e -> print_endline e
