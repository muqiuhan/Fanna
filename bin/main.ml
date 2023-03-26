open Core

let () =
  In_channel.read_all (Sys.get_argv ()).(1)
  |> Fanna.Lexer.to_token_list
  |> List.iter ~f:(fun token ->
         Fanna.Lexer.Token.to_string token |> prerr_endline)
