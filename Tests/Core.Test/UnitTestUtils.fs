module Fanna.Tests.Core.Utils

open NUnit.Framework
open Fanna.Core.Utils

[<TestFixture>]
type TestNumeric() =
    [<Test>]
    static member ``Numeric.Convert.Int64ToByteArray``() =
        Assert.AreEqual([| 0uy; 1uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy |], Numeric.Convert.Int64ToByteArray(256L))

[<TestFixture>]
type TestByteStreamReader() =
    [<Test>]
    static member ``ByteStreamReader.ReadByte``() =
        Assert.AreEqual(0x1uy, ByteStreamReader([| 0x1uy |]).ReadByte())

    [<Test>]
    static member ``ByteStreamReader.ReadBytes``() =
        Assert.AreEqual(
            [| 0x1uy; 0x2uy; 0x3uy; 0x4uy; 0x5uy |],
            ByteStreamReader([| 0x1uy; 0x2uy; 0x3uy; 0x4uy; 0x5uy |]).ReadBytes(5)
        )

    [<Test>]
    static member ``ByteStreamReader.ReadUint32``() =
        Assert.AreEqual(0x1B6C7561, ByteStreamReader([| 0x1Buy; 0x6Cuy; 0x75uy; 0x61uy; |]).ReadUint32())

    [<Test>]
    static member ``ByteStreamReader.ReadUint64``() =
        Assert.AreEqual(
            256ul,
            ByteStreamReader([| 0x0uy; 0x1uy; 0x0uy; 0x0uy; 0x0uy; 0x0uy; 0x0uy; 0x0uy |])
                .ReadUint64()
        )

    [<Test>]
    static member ``ByteStreamReader.ReadLuaInteger``() =
        Assert.AreEqual(
            256L,
            ByteStreamReader([| 0x0uy; 0x1uy; 0x0uy; 0x0uy; 0x0uy; 0x0uy; 0x0uy; 0x0uy |])
                .ReadLuaInteger()
        )

    [<Test>]
    static member ``ByteStreamReader.ReadLuaNumber``() =
        Assert.AreEqual(
            370.5,
            ByteStreamReader([| 0uy; 0uy; 0uy; 0uy; 0uy; 40uy; 119uy; 64uy |])
                .ReadLuaNumber()
        )

    [<Test>]
    static member ``ByteStreamReader.ReadString``() =
        Assert.AreEqual(
            "fanna",
            ByteStreamReader([| 6uy; byte ('f'); byte ('a'); byte ('n'); byte ('n'); byte ('a') |])
                .ReadString()
        )

        Assert.AreEqual("", ByteStreamReader([| 0x00uy |]).ReadString())

        let str256len =
            "a;lsdkfja;lskdfj;aslkdfjasdfaslkdfj;asldkgjqowpiserupqowierutpqwoeifaxkjcvn,zxmcnvx,nvlsdkfjghlsakdfujhgtpoeruithypoewuirthyosughslkjdfcbng,mxjzcbvnlkjasdfhglakdsjfhtgpoqeurithlksjghlksjbnvslx,mncvbalkdsfjfghalksdfjughlkasdjchgklsadjcvbkjvbalkdsfjghalkfsud"

        Assert.AreEqual(
            str256len,
            ByteStreamReader(
                [| 0xffuy
                   yield! Numeric.Convert.Int64ToByteArray(256)
                   yield! System.Text.Encoding.UTF8.GetBytes(str256len) |]
            )
                .ReadString()
        )
