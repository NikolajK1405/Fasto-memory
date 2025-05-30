#!/usr/bin/env -S dotnet fsi
open System
open System.IO
open System.Text.RegularExpressions

let isDirective (line : string) : bool =
    Regex.IsMatch(line, @"^\s*\.[a-zA-Z][a-zA-Z0-9\s()""\.,@_#-]*\n?$") 

let cleanLine (line : string) : string option =
    // TODO: Make case for every single directive 
    if isDirective(line) then 
        if   line.Contains(".text") then Some line
        elif line.Contains(".data") then Some line
        elif line.Contains(".word") then Some line
        else None
    elif   line.Contains("main:") then Some "f.main:\n"
    else Some line

let cleanup (inPath : string) (outPath : string) =
    let lines = File.ReadAllLines inPath
    let cleaned =
        lines
        |> Seq.choose cleanLine
        |> Seq.toArray
    File.WriteAllLines(outPath, [|"\n"|]) // Add newline first, to avoid bash cat adding to same line as outpath eof
    File.AppendAllLines(outPath, cleaned)
    // Add nesecary library at the end:
    let lib = File.ReadAllLines("./memory/lib.s")
    File.AppendAllLines(outPath, lib)

//[<EntryPoint>]
let main (argv: string[]) : int =
    if argv.Length < 2 then
        printfn "Usage: cleanupRV.fsx <input.s> <output.s>"
        0
    else
        let inputPath = argv.[0]
        let outputPath = argv.[1]
        cleanup inputPath outputPath
        printfn "Wrote clean file to  %s" outputPath
        0
        
main (fsi.CommandLineArgs |> Array.skip 1)
