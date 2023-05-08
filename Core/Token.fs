namespace Fanna.Core.Token

open Fanna.Core.Value

type Type =
    | Identifier
    | String

    override this.ToString() =
        match this with
        | Identifier -> "identifier"
        | String -> "string"

type Token(tokenType: Type, tokenValue: Value, tokenPos: (int * int)) =
    let (__line, __character) = tokenPos

    member public this.Type = tokenType
    member public this.Value = tokenValue
    member public this.Line = __line
    member public this.Character = __character

    override this.ToString() =
        $"""[<{this.Type.ToString()}>: "{this.Value.ToString}" - ({__line}, {__character})]"""
