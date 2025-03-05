#!/usr/bin/env -S dotnet fsi
open System
open System.IO
open System.Text.RegularExpressions

/// Regex to match:  lui   x?, %hi(symbol)
/// Captures ( "x?", "symbol" )
let patternLUI = 
    Regex(@"^\s*lui\s+(\w+)\s*,\s*%hi\((\w+)\)\s*$")

/// Regex to match:  lw x?, %lo(symbol)(x?)   OR   sw x?, %lo(symbol)(x?)
/// Captures ( "lw" or "sw", "x?", "symbol", "x?" )
let patternLWSW = 
    Regex(@"^\s*(lw|sw)\s+(\w+)\s*,\s*%lo\(\w+\)\((\w+)\)\s*$")

let isDirective (line : string) : bool =
    Regex.IsMatch(line, @"^\s*\.[a-zA-Z][a-zA-Z0-9\s()""\.,@_#-]*\n?$") 

let cleanLine (line : string) : string option =
    let LUI = patternLUI.Match(line)
    let LWSW = patternLWSW.Match(line)
    if LUI.Success then 
        let reg     = LUI.Groups.[1].Value
        let symbol  = LUI.Groups.[2].Value
        Some (sprintf "\tla\t%s, %s" reg symbol)
    elif LWSW.Success then
        let op     = LWSW.Groups.[1].Value  // lw or sw
        let reg1   = LWSW.Groups.[2].Value  // fx a1
        let reg2   = LWSW.Groups.[3].Value  // fx a0
        Some (sprintf "\t%s\t%s, 0(%s)" op reg1 reg2)
    //elif line.Contains("# @heap") then Some line // For glue.fsx to find split between functions and symbols
    elif isDirective(line) then
        if   line.Contains(".text") then Some "\t.text\t0x00400000\n\tla\tx3, d.heap\n\tjal\tf.main\n\tjal\tp.stop\n"
        elif line.Contains(".word") then Some line
        //elif line.Contains(".data") then Some "\t.data\n"
        else None
    elif   line.Contains("main:") then Some "f.main:\n"
    else Some line

let cleanup (inPath : string) (outPath : string) =
    let lines = File.ReadAllLines inPath
    let cleaned =
        lines
        |> Seq.skip 5 // remove first 5 lines
        |> Seq.choose cleanLine
        |> Seq.toArray
    File.WriteAllLines(outPath, cleaned)
    // Add nesecary library at the end:
    //let lib = File.ReadAllLines("./memory/lib.s")
    //File.AppendAllLines(outPath, lib)

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
