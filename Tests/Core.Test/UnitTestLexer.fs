module Core.Test

open NUnit.Framework

open Fanna.Core.Lexer
open Fanna.Core.Token

[<TestFixture>]
type TestLexer() =
    [<Test>]
    static member Tokenizer() =
        Lexer(""" print "hello world!" """.ToCharArray())
            .Tokenizer()
            .ForEach((fun token -> printfn $"{token.ToString()}"))
