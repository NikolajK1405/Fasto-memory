#!/usr/bin/env -S dotnet fsi
open System.IO

let glue (clangPath : string, fastoPath : string) : string array =
    let clangLines = File.ReadAllLines(clangPath)
    let functions = clangLines |> Array.takeWhile (fun line -> not (line.Contains("heap:")))
    let symbols = clangLines |> Array.skipWhile (fun line -> not (line.Contains("heap:")))
    File.ReadAllLines(fastoPath)
    |> Seq.collect (fun line ->
        if line.Contains(".data") then
            seq {
                // Insert functions from clang and lib.s before .data
                yield! Array.append functions (File.ReadAllLines("./memory/lib.s"))
                // Keep the .data line
                yield line
                // Insert symbols after .data
                yield! symbols
            }
        else
            seq { yield line }
    )
    |> Seq.toArray

let main (argv: string[]) : int =
    if argv.Length < 2 then
        printfn "Usage: cleanupRV.fsx <clang.s> <fasto.asm>"
        0
    else
        let clangPath = argv.[0]
        let fastoPath = argv.[1]
        let res = glue (clangPath, fastoPath)
        File.WriteAllLines(fastoPath, res)
        printfn "Wrote glued code to  %s" fastoPath
        0
        
main (fsi.CommandLineArgs |> Array.skip 1)