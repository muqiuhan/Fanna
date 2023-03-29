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

open Error
open Core
open Lexer
open Lexer.Token

module AST = struct
  type expr =
    | Module of Token.t
    | Import of Token.t
    | Identifier of Token.t
    | Number of Token.t
    | String of Token.t
    | Char of Token.t
    | Assignment of expr * expr
    | Lambda of expr * expr
    | Args of expr list
    | If of expr * expr
    | Else of expr
    | Call of expr * expr
    | Program of expr list
    | Cond of expr * expr list
    | Loop of expr * expr * expr
    | END

  and t = expr

  let rec to_string = function
    | Module v -> Format.sprintf "(Module %s)" (Token.to_string v)
    | Import v -> Format.sprintf "(Import %s)" (Token.to_string v)
    | Identifier v -> Token.to_string v
    | Number v -> Token.to_string v
    | String v -> Token.to_string v
    | Char v -> Token.to_string v
    | Assignment (assignment_identifier_ast, assignment_value_ast) ->
      Format.sprintf "(Assignment (%s) (%s))"
        (to_string assignment_identifier_ast)
        (to_string assignment_value_ast)
    | Lambda (lambda_args_ast, lambda_body_ast) ->
      Format.sprintf "(Lambda (%s) (%s))"
        (to_string lambda_args_ast)
        (to_string lambda_body_ast)
    | Args args -> List.map args ~f:to_string |> String.concat ~sep:"\t\n"
    | If (if_cond, if_body) ->
      Format.sprintf "(If (%s) (%s))" (to_string if_cond) (to_string if_body)
    | Else else_body -> Format.sprintf "(Else (%s))" (to_string else_body)
    | Cond (cond_target, cond_list) ->
      Format.sprintf "(Cond (%s) (%s))" (to_string cond_target)
        (List.map cond_list ~f:to_string |> String.concat ~sep:"\t\n")
    | Call (call_identifier, call_args) ->
      Format.sprintf "(Call (%s) (%s))"
        (to_string call_identifier)
        (to_string call_args)
    | Loop (loop_env, loop_until, loop_body) ->
      Format.sprintf "(Loop (%s) (%s) (%s))" (to_string loop_env)
        (to_string loop_until) (to_string loop_body)
    | Program expr_list ->
      List.map expr_list ~f:to_string |> String.concat ~sep:"\n"
    | END -> "(END)"
end

class token_stream token_list =
  object (self)
    val mutable current_token = List.hd_exn token_list

    val mutable next_token = List.nth_exn token_list 1

    val mutable pos = 0

    val token_list = token_list |> List.to_array

    method private advance () =
      try
        pos <- pos + 1 ;
        current_token <- token_list.(pos) ;
        next_token <- token_list.(pos + 1)
      with Invalid_argument _ -> raise End_of_file

    method private advance_n n =
      for _ = 1 to n do
        self#advance ()
      done

    method private error msg ast =
      print_endline "=========================================" ;
      print_endline ("o- Error: " ^ msg) ;
      print_endline "o- AST:" ;
      print_endline (AST.to_string ast) ;
      print_endline "o- Token Stream: " ;
      Array.iter token_list ~f:(fun token ->
          print_endline (Token.to_string token) ) ;
      failwith msg
  end

class is token_list =
  object
    inherit token_stream token_list

    method private is_comment () =
      match current_token with
      | {token_type= Comment; _} -> true
      | _ -> false

    method private is_module () =
      match current_token with
      | {token_type= Module; _} -> true
      | _ -> false

    method private is_import () =
      match current_token with
      | {token_type= Import; _} -> true
      | _ -> false

    method private is_assignment () =
      match current_token with
      | {token_type= Identifier; _} -> begin
        match next_token with
        | {token_type= Operator; token_value= ":="; _} -> true
        | _ -> false
      end
      | _ -> false

    method private is_lambda () =
      match current_token with
      | {token_type= Lambda; _} -> true
      | _ -> false

    method private is_if () =
      match current_token with
      | {token_type= If; _} -> true
      | _ -> false

    method private is_cond () =
      match current_token with
      | {token_type= Cond; _} -> true
      | _ -> false

    method private is_in () =
      match current_token with
      | {token_type= In; _} -> true
      | _ -> false

    method private is_identifier () =
      match current_token with
      | {token_type= Identifier; _} -> true
      | _ -> false

    method private is_call () =
      match current_token with
      | {token_type= Left_Parenthesis; _} -> true
      | _ -> false

    method private is_call_end () =
      match current_token with
      | {token_type= Right_Parenthesis; _} -> true
      | _ -> false

    method private is_end () =
      match current_token with
      | {token_type= End; _} -> true
      | _ -> false

    method private is_number () =
      match current_token with
      | {token_type= Number; _} -> true
      | _ -> false

    method private is_string () =
      match current_token with
      | {token_type= String; _} -> true
      | _ -> false

    method private is_char () =
      match current_token with
      | {token_type= Char; _} -> true
      | _ -> false

    method private is_block () =
      match current_token with
      | {token_type= Left_Braces; _} -> true
      | _ -> false

    method private is_block_end () =
      match current_token with
      | {token_type= Right_Braces; _} -> true
      | _ -> false

    method private is_cond_start () =
      match current_token with
      | {token_type= Operator; token_value= "|"; _} -> true
      | _ -> false

    method private is_if_cond_end () =
      match current_token with
      | {token_type= Operator; token_value= "->"; _} -> true
      | _ -> false

    method private is_statement_end () =
      match current_token with
      | {token_type= Semicolon; _} -> true
      | _ -> false

    method private is_until () =
      match current_token with
      | {token_type= Until; _} -> true
      | _ -> false

    method private is_do () =
      match current_token with
      | {token_type= Do; _} -> true
      | _ -> false

    method private is_default () =
      match current_token with
      | {token_type= Operator; token_value= "|"; _} -> begin
        match next_token with
        | {token_type= Default; _} -> true
        | _ -> false
      end
      | _ -> false

    method private is_loop () =
      match current_token with
      | {token_type= Loop; _} -> true
      | _ -> false
  end

class parser token_list =
  object (self)
    inherit is token_list

    method skip_comment () =
      while self#is_comment () do
        self#advance ()
      done

    method parse_module () =
      let module_ast = AST.Module next_token in
      print_endline (AST.to_string module_ast) ;
      self#advance_n 2 ;
      module_ast

    method parse_import () =
      let import_ast = AST.Import next_token in
      print_endline (AST.to_string import_ast) ;
      self#advance_n 2 ;
      import_ast

    method private parse_assignment () =
      let assignment_identifier_ast = AST.Identifier current_token in
      self#advance_n 2 ;
      let assignment_value_ast = self#parse () in
      let assignment_ast =
        AST.Assignment (assignment_identifier_ast, assignment_value_ast)
      in
      print_endline (AST.to_string assignment_ast) ;
      assignment_ast

    method private parse_lambda () =
      let parse_lambda_args () =
        self#advance () ;
        let args_list = Stack.create () in
        while
          match current_token with
          | {token_type= Operator; token_value= "->"; _} -> false
          | {token_type= Identifier; _} -> true
          | _ ->
            failwith
              ("Parse Lambda args error : " ^ Token.to_string current_token)
        do
          Stack.push args_list (AST.Identifier current_token) ;
          self#advance ()
        done ;
        self#advance () ;
        AST.Args (args_list |> Stack.to_list |> List.rev)
      in
      let lambda_args_ast = parse_lambda_args () in
      let lambda_body_ast = self#parse () in
      let lambda_ast = AST.Lambda (lambda_args_ast, lambda_body_ast) in
      print_endline (AST.to_string lambda_ast) ;
      lambda_ast

    method private parse_cond () =
      self#advance () ;
      let parse_cond_list () =
        let cond_list_ast = Stack.create () in
        while
          match current_token with
          | {token_type= Default; _} -> true
          | _ -> false
        do
          Stack.push cond_list_ast (self#parse ()) ;
          self#advance ()
        done ;
        Stack.push cond_list_ast (self#parse ()) ;
        cond_list_ast |> Stack.to_list |> List.rev
      in
      let cond_target_ast = self#parse () in
      let cond_cond_list_ast = parse_cond_list () in
      let cond_ast = AST.Cond (cond_target_ast, cond_cond_list_ast) in
      print_endline (AST.to_string cond_ast) ;
      cond_ast

    method private parse_if () =
      self#advance () ;
      let if_cond_ast = self#parse () in
      self#advance () ;
      let if_body_ast = self#parse () in
      AST.If (if_cond_ast, if_body_ast)

    method private parse_default () =
      self#advance_n 2 ;
      let default_body_ast = self#parse () in
      AST.Else default_body_ast

    method private parse_call () =
      let parse_call_args () =
        self#advance () ;
        let args_list = Stack.create () in
        while
          match next_token with
          | {token_type= Right_Bracket; _} -> self#advance () ; false
          | _ -> true
        do
          Stack.push args_list (self#parse ()) ;
          self#advance ()
        done ;
        self#advance () ;
        AST.Args (args_list |> Stack.to_list |> List.rev)
      in
      self#advance () ;
      let call_identifier_ast = AST.Identifier current_token in
      let call_args_ast = parse_call_args () in
      let call_ast = AST.Call (call_identifier_ast, call_args_ast) in
      print_endline (AST.to_string call_ast) ;
      call_ast

    method private parse_loop () =
      self#advance () ;
      let loop_env = self#parse ()
      and loop_until = self#parse ()
      and loop_body = self#parse () in
      AST.Loop (loop_env, loop_until, loop_body)

    method private parse_identifier () =
      let identifier_ast = AST.Identifier current_token in
      self#advance () ; identifier_ast

    method private parse_number () =
      let number_ast = AST.Number current_token in
      self#advance () ; number_ast

    method private parse_string () =
      let string_ast = AST.String current_token in
      self#advance () ; string_ast

    method private parse_char () =
      let char_ast = AST.Char current_token in
      self#advance () ; char_ast

    method private parse_expr () =
      if self#is_comment () then begin
        self#skip_comment () ; self#parse_expr ()
      end
      else if self#is_module () then
        self#parse_module ()
      else if self#is_import () then
        self#parse_import ()
      else if self#is_assignment () then
        self#parse_assignment ()
      else if self#is_lambda () then
        self#parse_lambda ()
      else if self#is_cond () then
        self#parse_cond ()
      else if self#is_default () then
        self#parse_default ()
      else if self#is_cond_start () then
        self#parse_if ()
      else if self#is_if_cond_end () then
        raise End_of_file
      else if self#is_statement_end () then
        raise End_of_file
      else if self#is_in () then
        raise End_of_file
      else if self#is_end () then
        raise End_of_file
      else if self#is_call () then
        self#parse_call ()
      else if self#is_call_end () then
        raise End_of_file
      else if self#is_loop () then
        self#parse_loop ()
      else if self#is_until () then
        raise End_of_file
      else if self#is_do () then
        raise End_of_file
      else if self#is_number () then
        self#parse_number ()
      else if self#is_string () then
        self#parse_string ()
      else if self#is_char () then
        self#parse_char ()
      else if self#is_identifier () then
        self#parse_identifier ()
      else
        raise (Parser.Unknown_Token current_token)

    method parse () =
      let program = Stack.create () in
      try
        while true do
          let ast = self#parse_expr () in
          Stack.push program ast;
        done ;
        failwith "parse"
      with
      | End_of_file -> AST.Program (Stack.to_list program |> List.rev)
      | Parser.Unknown_Token token ->
        self#error
          ("Unknown Token " ^ Token.to_string token)
          (AST.Program (Stack.to_list program |> List.rev))
  end

let to_ast token_list = (new parser token_list)#parse ()
