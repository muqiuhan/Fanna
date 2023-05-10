module Fanna.Core.Utils

type ByteStreamReader(data: array<byte>) =
    let __data = data
    let mutable __pos = -1

    member public this.ReadByte() =
        __pos <- __pos + 1
        __data[__pos]

    member public this.ReadBytes(n: int) =
        let bytes = Array.zeroCreate<byte> (n)

        for i = 0 to n - 1 do
            bytes[i] <- this.ReadByte()

        bytes


    /// Read an integer of cint type (4 bytes, mapped to F# uint32) from the byte stream in little endian mode
    member public this.ReadUint32() =
        let uint32Bytes = this.ReadBytes(4)

        let uint32Bytes =
            if System.BitConverter.IsLittleEndian then
                uint32Bytes
            else
                Array.rev (uint32Bytes)

        System.BitConverter.ToUInt32(uint32Bytes)

    /// Read an integer of size_t type (8 bytes, mapped to F# uint64) from the byte stream in little endian mode
    member public this.ReadUint64() =
        let uint64Bytes = this.ReadBytes(8)

        let uint64Bytes =
            if System.BitConverter.IsLittleEndian then
                uint64Bytes
            else
                Array.rev (uint64Bytes)

        System.BitConverter.ToUInt64(uint64Bytes)

    /// Read a Lua integer from the byte stream with ReadUint64() (8 bytes, mapped to F# int64 type)
    member public this.ReadLuaInteger() = int64 (this.ReadUint64())

    /// Read a Lua float number from the byte stream with ReadUint64() (8 bytes, mapped to F# float type)
    member public this.ReadLuaNumber() = float (this.ReadUint64())

    /// Read a string from the byte stream
    member public this.ReadString() =
        match this.ReadByte() with
        | 0uy -> ""
        | 0xFFuy -> System.BitConverter.ToString(this.ReadBytes(int (this.ReadUint64())))
        | error -> failwith $"ReadString() -> {error}"
