namespace Fanne.Core.Parser

open System
open Fanna.Core.Token
open Fanna.Core.Value
open Fanna.Core.ByteCode

type Proto =
    { Constrants: Collections.Generic.List<Value>
      ByteCodes: Collections.Generic.List<ByteCode> }

type Parser(tokenList: list<Token>) =
    let __proto =
        { Constrants = Collections.Generic.List<Value>()
          ByteCodes = Collections.Generic.List<ByteCode>() }

    member public _.TokenList = tokenList

    member private this.ParseIdentifier(token: Token) =
        __proto.Constrants.Add(token.Value)
        __proto.ByteCodes.Add(ByteCode.GetGlobal(0uy, uint8 (__proto.Constrants.Count - 1)))

    member private this.ParseString(token: Token) =
        __proto.Constrants.Add(token.Value)
        __proto.ByteCodes.Add(ByteCode.LoadConst(1uy, uint8 (__proto.Constrants.Count - 1)))
        __proto.ByteCodes.Add(ByteCode.Call(2uy, uint8 (__proto.Constrants.Count - 1)))

    member private this.__Parse(tokenList: list<Token>) =
        match this.TokenList with
        | [] -> __proto
        | token :: tokenList ->
            match token.Type with
            | Fanna.Core.Token.String -> this.ParseString(token)
            | Fanna.Core.Token.Identifier -> this.ParseIdentifier(token)

            this.__Parse (tokenList)

    member public this.Parse() = this.__Parse (this.TokenList)
