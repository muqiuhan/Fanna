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
open Lexer.Token

module AST = struct
  type expr =
    | Identifier of {identifier_token : Lexer.Token.t}
    | Lambda of {
        lambda_token : Lexer.Token.t;
        lambda_paramaters : expr list;
        lambda_body : expr;
      }
    | If of {
        if_token : Lexer.Token.t;
        if_cond : expr;
        if_body : expr;
      }
    | Else of {
        else_token : Lexer.Token.t;
        else_cond : expr;
        else_body : expr;
      }
    | Call of {
        call_identifier : expr;
        call_parameter : expr list;
    }
end

class t token_array =
  object (self)
    val mutable pos = 0
    val mutable current_token = token_array.(0)
    val mutable next_token = token_array.(1)
    val token_array = token_array

    method private advance () =
      try
        current_token <- token_array.(pos);
        next_token <- token_array.(pos + 1);
        pos <- pos + 1
      with Invalid_argument _ -> raise End_of_file

    method private read_while predicate =
      let token_list = ref [] in
      while predicate current_token do
        token_list := !token_list @ [current_token]
      done;
      !token_list

    method private parse_lambda () =
      let lambda_token = current_token in
      let lambda_paramaters =
        self#read_while (fun token ->
            match token.token_type with
            | Identifier -> true
            | _ -> false)
        |> List.map ~f:(fun identifier_token ->
               AST.Identifier {identifier_token})
      in
      (* lambda without parameters *)
      (* if List.is_empty lambda_paramaters then *)
      (* skip lambda start operator "->" *)
      self#advance () |> ignore;
      let lambda_body = self#parse_expr () in
      AST.Lambda {lambda_token; lambda_paramaters; lambda_body}

    method private parse_if () =
      let if_token = current_token in
      let if_cond = self#parse_expr () in
      let if_body = self#parse_expr () in
      AST.If {if_token; if_cond; if_body}

    method private parse_else () =
      let else_token = current_token in
      let else_cond = self#parse_expr () in
      let else_body = self#parse_expr () in
      AST.Else {else_token; else_cond; else_body}

    method private parse_call () =
      let call_identifier = AST.Identifier {identifier_token = current_token} in
      (* skip parameter start punctuation ")" *)
      self#advance () |> ignore;
      let call_parameter =
        self#read_while (fun token ->
            match token.token_type with
            | Punctuation -> true
            | _ -> false)
        |> List.map ~f:(fun identifier_token ->
               AST.Identifier {identifier_token})
      in
      (* skip parameter end punctuation ")" *)
      self#advance () |> ignore;
      AST.Call {call_identifier; call_parameter}

    method private parse_identifier () =
      let identifier_token = current_token in
      match next_token.token_type with
      | Punctuation -> self#parse_call ()
      | _ -> AST.Identifier {identifier_token}

    method parse_expr () =
      match current_token.token_type with
      | Lambda -> self#parse_lambda ()
      | If -> self#parse_if ()
      | Else -> self#parse_else ()
      | Identifier -> self#parse_identifier ()
      | _ -> failwith ("Parser error: " ^ to_string current_token)

    method private parse () =
      let prog_stack = Stack.create () in
      try
        while true do
          Stack.push prog_stack (self#parse_expr ())
        done;
        failwith "Parse Error"
      with End_of_file -> prog_stack |> Stack.to_array |> Array.rev
  end
