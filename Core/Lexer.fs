namespace Fanna.Core.Lexer

open System
open Fanna.Core.Token

type Lexer(fileValue: array<char>) =
    let mutable __pos = 0
    let mutable __line = 0
    let mutable __character = 0

    let mutable __currentChar = fileValue[__pos]

    member private this.TokenList = Collections.Generic.List()
    member private this.FileValue = fileValue

    member private this.Advance() =
        __pos <- __pos + 1
        __currentChar <- this.FileValue[__pos]

    member private this.ReadString() =
        let value = Collections.Generic.List()

        while not (__currentChar = '"') do
            value.Add(__currentChar)
            this.Advance()

        String.Concat(value)

    member private this.ReadIdentifier() =
        let value = Collections.Generic.List()

        while Char.IsLetter(__currentChar) do
            value.Add(__currentChar)
            this.Advance()

        String.Concat(value)


    member private this.Next() =
        // Whitespace
        if Char.IsWhiteSpace(__currentChar) then
            this.Next()

        // String literal
        else if __currentChar = '"' then
            Token(String, Fanna.Core.Value.String(this.ReadString()), (__line, __character))

        // Identifier
        else if Char.IsLetter(__currentChar) then
            Token(Identifier, Fanna.Core.Value.String(this.ReadIdentifier()), (__line, __character))

        else
            failwith "Unknown token"

    member public this.Tokenizer() =
        try
            while true do
                this.TokenList.Add(this.Next())

            failwith "This can't happen"
        with :? IndexOutOfRangeException ->
            this.TokenList
