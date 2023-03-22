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

(** Provides operations to read characters from a string. *)
module Input_Stream = struct
  type input_stream =
    { mutable pos : int
    ; mutable line : int
    ; mutable col : int
    }

  class t (input : string) =
    object (self)
      val inner = { pos = 0; line = 1; col = 0 }
      val input = String.to_array input

      (** returns the next value but without removing it from the stream. *)
      method peek () = input.(inner.pos)

      (** returns the next value and also discards it from the stream. *)
      method next () =
        let ch = input.(inner.pos) in
        if Char.(ch = '\n')
        then (
          inner.line <- inner.line + 1;
          inner.col <- 0)
        else inner.col <- inner.col + 1;
        ch

      (** returns true if and only if there are no more values in the stream. *)
      method eof () =
        try
          self#peek () |> ignore;
          false
        with
        | Invalid_argument _ -> true

      (** does raise new Failure(msg). *)
      method croak msg =
        let _ =
          raise
            (Failure
               (String.concat
                  [ msg
                  ; " ("
                  ; Int.to_string inner.line
                  ; ":"
                  ; Int.to_string inner.col
                  ; ")"
                  ]))
        in
        ()
    end
end
