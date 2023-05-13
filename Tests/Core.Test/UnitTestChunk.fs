module Fanna.Tests.Core.Chunk

open NUnit.Framework
open Fanna.Core.Chunk
open Fanna.Core.Utils.Executor

[<TestFixture>]
type TestBinaryChunk() =
    static member private TestWithChunkFile(f: array<byte> -> unit) =
        let assets_file_path =
            $"""{System.IO.Path.Combine([| __SOURCE_DIRECTORY__; "assets"; "UnitTestChunk.lua" |])}"""

        let output_chunk_file_path =
            $"""{System.IO.Path.Combine([| __SOURCE_DIRECTORY__; "assets"; "UnitTestChunk.o" |])}"""

        (ExecAsync "luac5.3" [| "-o"; output_chunk_file_path; assets_file_path |])
        <|> (Failure, (fun result -> eprintfn $"{result.StandardError}"))
        <|> (Success, (fun result -> printfn $"{result.StandardOutput}"))
        |> ignore

        try
            f (System.IO.File.ReadAllBytes(output_chunk_file_path))
        with e ->
            Assert.Warn(e.ToString())
            Assert.Fail()

        System.IO.File.Delete(output_chunk_file_path)

    [<Test>]
    static member ``BinaryChunk.Undump``() =
        TestBinaryChunk.TestWithChunkFile(fun chunk ->
            Array.iter (fun byte -> printf "0x%X " byte) chunk
            BinaryChunk.Undump(chunk) |> ignore)
