open Core

let _ =
  try
    In_channel.read_all (Sys.get_argv ()).(1)
    |> Fanna.Lexer.to_token_list
    |> List.map ~f:Fanna.Lexer.Token.show
    |> List.iter ~f:print_endline
  with Failure e -> print_endline e
