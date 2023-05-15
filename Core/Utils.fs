module Fanna.Core.Utils

open System.Diagnostics
open System.Threading.Tasks

module Numeric =
    type Convert =
        static member Int64ToByteArray(i: int64) =
            let mutable result = Array.zeroCreate<byte> 8

            for j = 0 to 7 do
                result.[j] <- byte (i >>> (j * 8))

            result

/// Copyright (c) 2022 Muqiu Han
/// A simple, convenient library for calling shell commands in F# (supports Async).
/// https://github.com/muqiuhan/FsExecute
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
                    Task.WhenAll(
                        [| p.StandardOutput.ReadToEndAsync()
                           p.StandardError.ReadToEndAsync() |]
                    )

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

    let Exec: string -> array<string> -> Async<CommandResult> = ExecuteCommand

    let ExecAsync: string -> array<string> -> CommandResult =
        fun command args -> Exec command args |> Async.RunSynchronously
