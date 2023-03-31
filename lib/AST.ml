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

open Lexer

type expr =
  | Module of {module_token: Token.t}
  | Import of {import_token: Token.t}
  | Identifier of {identifier_token: Token.t}
  | Define of
      { define_identifier: expr;
        define_value: expr }
  | Lambda of
      { lambda_token: Token.t;
        lambda_args: expr list;
        lambda_body: expr }
  | Call of
      { call_identifier: expr;
        call_args: t list }
  | String of {string_token: Token.t}
  | Number of {number_token: Token.t}
  | Char of {char_token: Token.t}
  | Cond of
      { cond_list: (expr * t) list;
        cond_default: t }
  | Return of {return_value: expr}
  | Loop of
      { loop_token: Token.t;
        loop_def: expr list;
        loop_cond: expr;
        loop_body: t }
  | Program of t list
[@@deriving show]

and t = expr list [@@deriving show]