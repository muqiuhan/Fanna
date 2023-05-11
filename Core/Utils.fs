module Fanna.Core.Utils

open System.Diagnostics
open System.Threading.Tasks

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
                Array.rev (uint32Bytes)
            else
                uint32Bytes

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

    /// Read a Lua float number from the byte (8 bytes, mapped to F# float type)
    member public this.ReadLuaNumber() =
        let floatBytes = this.ReadBytes(8)

        let floatBytes =
            if System.BitConverter.IsLittleEndian then
                floatBytes
            else
                Array.rev (floatBytes)

        System.BitConverter.ToDouble(floatBytes)

    /// Read a string from the byte stream
    member public this.ReadString() =
        match this.ReadByte() with
        | 0uy -> ""
        | 0xFFuy -> System.Text.Encoding.UTF8.GetString(this.ReadBytes(int (this.ReadUint64())))
        | size -> System.Text.Encoding.UTF8.GetString(this.ReadBytes(int (size) - 1))

module Numeric =
    type Convert =
        static member Int64ToByteArray(i: int64) =
            let mutable result = Array.zeroCreate<byte> 8

            for j = 0 to 7 do
                result.[j] <- byte (i >>> (j * 8))

            result

module Executor =

    type ExitStatus =
        | Success
        | Failure

    type CommandResult =
        { ExitCode: int
          StandardOutput: string
          StandardError: string }

    let ExecuteCommand: string -> seq<string> -> Async<CommandResult> =
        fun executable args ->
            async {
                let startInfo = ProcessStartInfo()
                startInfo.FileName <- executable

                for a in args do
                    startInfo.ArgumentList.Add(a)

                startInfo.RedirectStandardOutput <- true
                startInfo.RedirectStandardError <- true
                startInfo.UseShellExecute <- false
                startInfo.CreateNoWindow <- true

                use p = new Process()
                p.StartInfo <- startInfo
                p.Start() |> ignore

                let outTask =
                    Task.WhenAll([| p.StandardOutput.ReadToEndAsync(); p.StandardError.ReadToEndAsync() |])

                do! p.WaitForExitAsync() |> Async.AwaitTask
                let! out = outTask |> Async.AwaitTask

                return
                    { ExitCode = p.ExitCode
                      StandardOutput = out.[0]
                      StandardError = out.[1] }
            }

    let Bind: CommandResult -> (ExitStatus * (CommandResult -> unit)) -> CommandResult =
        fun result (status, processer) ->
            (match status with
             | Success ->
                 if result.ExitCode = 0 then
                     processer result
             | Failure ->
                 if result.ExitCode <> 0 then
                     processer result)

            result

    let (<|>) = Bind

    let Exec: string -> array<string> -> Async<CommandResult> =
        fun command args -> ExecuteCommand command args

    let ExecAsync: string -> array<string> -> CommandResult =
        fun command args -> Exec command args |> Async.RunSynchronously
