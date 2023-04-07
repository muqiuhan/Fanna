(** The MIT License (MIT)
 ** 
 ** Copyright (c) 2022 Muqiu Han
 ** 
 ** Permission is hereby granted, free of charge, to any person obtaining a copy
 ** of this software and associated documentation files (the "Software"), to deal
 ** in the Software without restriction, including without limitation the rights
 ** to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 ** copies of the Software, and to permit persons to whom the Software is
 ** furnished to do so, subject to the following conditions:
 ** 
 ** The above copyright notice and this permission notice shall be included in all
 ** copies or substantial portions of the Software.
 ** 
 ** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 ** IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 ** FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 ** AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 ** LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 ** OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 ** SOFTWARE. *)

open Core
open Lexer
open Token

type parser =
  { token_stream: Token.t list;
    ast: AST.t }

and ast = AST.t

let rec parse {token_stream; ast} =
  match token_stream with
  (* comment *)
  | {token_type= Comment; _} :: token_stream -> {token_stream; ast} |> parse
  (* module *)
  | {token_type= Module; _} :: module_token :: token_stream ->
    {token_stream; ast= AST.Module {module_token} :: ast} |> parse
  (* import *)
  | {token_type= Import; _} :: import_token :: token_stream ->
    {token_stream; ast= AST.Import {import_token} :: ast} |> parse
  (* define *)
  | ({token_type= Identifier; _} as identifier_token)
    :: {token_type= Operator; token_value= ":="; _}
    :: token_stream ->
    let define_identifier = AST.Identifier {identifier_token}
    and define_value_ast = {token_stream; ast= []} |> parse in
    let token_stream = define_value_ast.token_stream
    and define_value = define_value_ast.ast in
    { token_stream;
      ast=
        AST.Define {define_identifier; define_value= AST.Program [define_value]}
        :: ast }
  (* lambda *)
  | ({token_type= Lambda; _} as lambda_token) :: token_stream ->
    let token_stream, lambda_args = parse_lambda_args token_stream in
    let lambda_body_ast = {token_stream; ast= []} |> parse in
    let token_stream = lambda_body_ast.token_stream
    and lambda_body = lambda_body_ast.ast in
    { token_stream;
      ast=
        AST.Lambda
          {lambda_token; lambda_args; lambda_body= AST.Program [lambda_body]}
        :: ast }
  (* program *)
  | {token_type= Left_Braces; _} :: token_stream ->
    let token_stream, program = parse_program token_stream in
    {token_stream; ast= AST.Program program :: ast}
  (* call *)
  | ({token_type= Identifier; _} as identifier_token)
    :: {token_type= Left_Parenthesis; _}
    :: token_stream ->
    let token_stream, call_args = parse_call_args token_stream
    and call_identifier = AST.Identifier {identifier_token} in
    {token_stream; ast= AST.Call {call_identifier; call_args} :: ast}
  (* call *)
  | ({token_type= Operator; _} as identifier_token)
    :: {token_type= Left_Parenthesis; _}
    :: token_stream ->
    let token_stream, call_args = parse_call_args token_stream
    and call_identifier = AST.Identifier {identifier_token} in
    {token_stream; ast= AST.Call {call_identifier; call_args} :: ast}
  (* cond *)
  | {token_type= Operator; token_value= "|"; _} :: token_stream ->
    let token_stream, cond_list, cond_default = parse_cond token_stream in
    {token_stream; ast= AST.Cond {cond_list; cond_default} :: ast}
  (* loop *)
  | ({token_type= Loop; _} as loop_token) :: token_stream ->
    let token_stream, loop_def, loop_cond, loop_body =
      parse_loop token_stream
    in
    { token_stream;
      ast= AST.Loop {loop_token; loop_def; loop_cond; loop_body} :: ast }
  (* identifier *)
  | ({token_type= Identifier; _} as identifier_token) :: token_stream ->
    {token_stream; ast= AST.Identifier {identifier_token} :: ast}
  (* string *)
  | ({token_type= String; _} as string_token) :: token_stream ->
    {token_stream; ast= AST.String {string_token} :: ast}
  (* char *)
  | ({token_type= Char; _} as char_token) :: token_stream ->
    {token_stream; ast= AST.Char {char_token} :: ast}
  (* number *)
  | ({token_type= Number; _} as number_token) :: token_stream ->
    {token_stream; ast= AST.Number {number_token} :: ast}
  (* call args end *)
  | {token_type= Right_Parenthesis; _} :: token_stream -> {token_stream; ast}
  (* program end *)
  | {token_type= Right_Braces; _} :: token_stream -> {token_stream; ast}
  (* cond end *)
  | {token_type= Operator; token_value= "->"; _} :: token_stream ->
    {token_stream; ast}
  (* loop def end *)
  | {token_type= Until; _} :: token_stream -> {token_stream; ast}
  (* statement end *)
  | {token_type= Semicolon; _} :: token_stream -> {token_stream; ast}
  (* parameter separation *)
  | {token_type= Comma; _} :: token_stream -> {token_stream; ast}
  (* operator to identifier *)
  | ({token_type= Operator; _} as identifier_token) :: token_stream ->
    {token_stream; ast= AST.Identifier {identifier_token} :: ast}
  | _ ->
    let error =
      Format.sprintf "Syntax error: %s"
        (Token.to_string (List.hd_exn token_stream))
    in
    Simlog.error error ; failwith error

and parse_loop token_stream =
  let parse_loop_def token_stream =
    let {token_stream; ast} = parse {token_stream; ast= []} in
    match ast with
    | [] -> (token_stream, [])
    | defexprs ->
      ( token_stream,
        List.map defexprs ~f:(function
          | AST.Define _ as defexpr -> defexpr
          | expr ->
            failwith
              ( "Syntax error: Illegal loop initialization expression"
              ^ AST.show_expr expr ) ) )
  and parse_loop_cond token_stream =
    let {token_stream; ast} = parse {token_stream; ast= []} in
    match ast with
    | [] -> failwith "Syntax error: loop termination condition is empty"
    | [cond] -> begin
      match cond with
      | AST.Call _ -> (token_stream, cond)
      | _ -> failwith "Syntax error: illegal loop termination expression"
    end
    | _ -> failwith "Syntax error: illegal loop termination expression"
  and parse_loop_body token_stream = parse {token_stream; ast= []} in
  let token_stream, loop_def = parse_loop_def token_stream in
  let token_stream, loop_cond = parse_loop_cond token_stream in
  let {token_stream; ast} = parse_loop_body token_stream in
  let loop_body = ast in
  (token_stream, loop_def, loop_cond, loop_body)

and parse_cond token_stream =
  let parse_cond_list token_stream =
    let rec parse_cond_cond token_stream =
      let {token_stream; ast} = {token_stream; ast= []} |> parse in
      match ast with
      | [] -> failwith "Syntax Error: Empty condition"
      | [cond] -> begin
        match cond with
        | AST.Call _
         |AST.Identifier _
         |AST.Number _
         |AST.String _
         |AST.Char _ -> (token_stream, cond)
        | _ -> failwith "Syntax Error: Invalid conditional expression"
      end
      | _ -> failwith "Syntax Error: Invalid conditional expression"
    and parse_cond_body token_stream =
      let {token_stream; ast} = {token_stream; ast= []} |> parse in
      (token_stream, ast)
    and loop (token_stream, cond_list) =
      match token_stream with
      | {token_type= Operator; token_value= "->"; _}
        :: {token_type= Default; _}
        :: token_stream -> (token_stream, cond_list)
      | _ ->
        let token_stream, cond_cond = parse_cond_cond token_stream in
        let token_stream, cond_body = parse_cond_body token_stream in
        loop (token_stream, (cond_cond, cond_body) :: cond_list)
    in
    loop (token_stream, [])
  and parse_cond_default token_stream =
    let {token_stream; ast} = {token_stream; ast= []} |> parse in
    (token_stream, ast)
  in
  let token_stream, cond_list = parse_cond_list token_stream in
  let token_stream, cond_default = parse_cond_default token_stream in
  (token_stream, cond_list, cond_default)

and parse_call_args token_stream =
  let rec loop (token_stream, args) =
    let parse_arg token_stream =
      let {token_stream; ast} = parse {token_stream; ast= []} in
      (token_stream, ast)
    in
    match token_stream with
    | {token_type= Right_Parenthesis; _} :: token_stream -> (token_stream, args)
    | _ ->
      let token_stream, arg = parse_arg token_stream in
      loop (token_stream, arg :: args)
  in
  loop (token_stream, [])

and parse_program token_stream =
  let rec loop (token_stream, statements) =
    match token_stream with
    | {token_type= Right_Braces; _} :: token_stream -> (token_stream, statements)
    | _ ->
      let {token_stream; ast} = parse {token_stream; ast= []} in
      loop (token_stream, statements @ [ast])
  in
  loop (token_stream, [])

and parse_lambda_args token_stream =
  let rec loop (token_stream, args) =
    match token_stream with
    | {token_type= Operator; token_value= "->"; _} :: token_stream ->
      (token_stream, args)
    | ({token_type= Identifier; _} as identifier_token) :: token_stream ->
      loop (token_stream, AST.Identifier {identifier_token} :: args)
    | _ ->
      let error =
        Format.sprintf "Parse args error: %s"
          (Token.to_string (List.hd_exn token_stream))
      in
      Simlog.error error ; failwith error
  in
  loop (token_stream, [])

let to_ast token_stream =
  let program = Stack.create ()
  and remaining = ref token_stream in
  while List.is_empty !remaining |> not do
    let {token_stream; ast} = {token_stream= !remaining; ast= []} |> parse in
    Stack.push program ast ;
    remaining := token_stream
  done ;
  Stack.to_list program |> List.rev
