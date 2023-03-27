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

module Token = struct
  module Type = struct
    type t =
      | Punctuation
      | Identifier
      | Number
      | Char
      | Bool
      | String
      | Operator
      | If
      | Then
      | Comment
      | Else
      | Lambda
      | Let
      | Import
      | Module
      | For
      | While

    let to_string = function
      | Punctuation -> "Punctuation"
      | Identifier -> "Identifier"
      | Number -> "Number"
      | Char -> "Char"
      | String -> "String"
      | Operator -> "Operator"
      | Bool -> "Bool"
      | Comment -> "Comment"
      | If -> "If"
      | Then -> "Then"
      | Else -> "Else"
      | Lambda -> "Lambda"
      | Let -> "Let"
      | Import -> "Import"
      | Module -> "Module"
      | For -> "For"
      | While -> "While"
  end

  type t = {
    token_value : string;
    token_type : Type.t;
    token_pos : int * int;
  }

  let to_string
      {token_value : string; token_type : Type.t; token_pos : int * int} =
    let line, col = token_pos in
    let token_type = Type.to_string token_type in
    let token_value =
      match token_value with
      | "\t" -> "\\t"
      | "\n" -> "\\n"
      | token_value -> token_value
    in
    Format.sprintf {|<%s: "%s" [%d, %d]>|} token_type token_value line col

  let to_keyword = function
    | "if" -> Type.If
    | "let" -> Type.Let
    | "else" -> Type.Else
    | "then" -> Type.Then
    | "module" -> Type.Module
    | "import" -> Type.Import
    | "for" -> Type.For
    | "while" -> Type.While
    | "fn" -> Type.Lambda
    | _ -> failwith "unknown keyword"

  let is_keyword = function
    | "if" -> true
    | "else" -> true
    | "let" -> true
    | "then" -> true
    | "for" -> true
    | "while" -> true
    | "import" -> true
    | "module" -> true
    | "fn" -> true
    | _ -> false

  let is_number number =
    Char.is_digit number || List.for_all ['.'] ~f:(fun c -> Char.(c = number))

  let is_identifier_start = Char.is_alpha
  let is_bool_start c = Char.(c = '#')

  let is_identifier identifier =
    Char.(identifier = '_')
    || Char.is_alpha identifier
    || Char.is_digit identifier

  let is_operator operator =
    let result = ref false in
    List.iter
      [
        '+';
        '-';
        '*';
        '/';
        '%';
        '=';
        '&';
        '|';
        '>';
        '<';
        '!';
        '#';
        '@';
        '?';
        '$';
        ':';
      ] ~f:(fun c -> if Char.(c = operator) then result := true else ());
    !result

  let is_punctuation punctuation =
    let result = ref false in
    List.iter [','; ';'; '('; ')'; '['; ']'; '{'; '}'] ~f:(fun c ->
        if Char.(c = punctuation) then result := true else ());
    !result

  let is_string_start string = Char.(string = '"')
  let is_char_start char = Char.(char = '\'')
  let is_whitespace = Char.is_whitespace
end

class t source_code =
  object (self)
    val mutable pos = 1
    val mutable line = 1
    val mutable col = 0
    val mutable current_char = String.get source_code 0
    val source_code = String.to_array (source_code ^ " ")

    method private advance () =
      try
        current_char <- source_code.(pos);
        pos <- pos + 1;
        if Char.(current_char = '\n') then (
          line <- line + 1;
          col <- 0)
        else
          col <- col + 1
      with Invalid_argument _ -> raise End_of_file

    method private read_while predicate =
      let str = ref "" in
      while predicate current_char do
        str := !str ^ Char.to_string current_char;
        self#advance ()
      done;
      !str

    method private read_identifier () =
      let identifier = self#read_while Token.is_identifier in
      if Token.is_keyword identifier then
        Token.
          {
            token_type = to_keyword identifier;
            token_value = identifier;
            token_pos = (line, col);
          }
      else
        Token.
          {
            token_type = Identifier;
            token_value = identifier;
            token_pos = (line, col);
          }

    method private read_number () =
      let number = self#read_while Token.is_number in
      Token.
        {
          token_type = Type.Number;
          token_value = number;
          token_pos = (line, col);
        }

    method private read_comment () =
      let comment = self#read_while (fun c -> Char.(c <> '\n')) in
      Token.
        {
          token_type = Type.Comment;
          token_value = comment;
          token_pos = (line, col);
        }

    method private read_operator () =
      let operator = self#read_while Token.is_operator in
      if String.(operator = "#|") then
        self#read_comment ()
      else
        Token.
          {
            token_type = Type.Operator;
            token_value = operator;
            token_pos = (line, col);
          }

    method private read_punctuation () =
      let punctuation = Char.to_string current_char in
      self#advance ();
      Token.
        {
          token_type = Type.Punctuation;
          token_value = punctuation;
          token_pos = (line, col);
        }

    method private read_char () =
      self#advance ();
      let char =
        self#read_while (fun c ->
            if Char.(c = '\'') then
              false
            else
              true)
      in
      self#advance ();
      Token.
        {token_type = Type.Char; token_value = char; token_pos = (line, col)}

    method private read_string () =
      self#advance ();
      let string =
        self#read_while (fun c ->
            if Char.(c = '"') then
              false
            else
              true)
      in
      self#advance ();
      Token.
        {
          token_type = Type.String;
          token_value = string;
          token_pos = (line, col);
        }

    method next () =
      if Token.is_whitespace current_char then (
        self#read_while Token.is_whitespace |> ignore;
        self#next ())
      else if Token.is_identifier_start current_char then
        self#read_identifier ()
      else if Token.is_number current_char then
        self#read_number ()
      else if Token.is_operator current_char then
        self#read_operator ()
      else if Token.is_punctuation current_char then
        self#read_punctuation ()
      else if Token.is_char_start current_char then
        self#read_char ()
      else if Token.is_string_start current_char then
        self#read_string ()
      else
        failwith ("Unknown char: \'" ^ Char.to_string current_char ^ "\'")
  end

let to_token_array source_code =
  let lexer = new t source_code in
  let token_stack = Stack.create () in
  try
    while true do
      Stack.push token_stack (lexer#next ())
    done;
    failwith "Lexer error"
  with End_of_file -> token_stack |> Stack.to_array |> Array.rev
