module Fanna.Tests.Core.Utils

open NUnit.Framework
open Fanna.Core.Utils

[<TestFixture>]
type TestNumeric() =
    [<Test>]
    static member ``Numeric.Convert.Int64ToByteArray``() =
        Assert.AreEqual([| 0uy; 1uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy |], Numeric.Convert.Int64ToByteArray(256L))