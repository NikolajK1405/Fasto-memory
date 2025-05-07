module ANFPretty
open ANorm
open System
(* pretty printing A-normal form, mostly made by chatgpt, just for testing.. *)

let ppType (tp : MiniFasto.Type) : string =
    match tp with 
    | MiniFasto.Int -> "int"
    | MiniFasto.Array _ -> "arr"

// Helper: Pretty-print an AVal
let rec ppAVal (v : AVal) : string =
    match v with
    | N n -> string n
    | V x -> x
// Helper: join strings with separator
let joinWith (sep : string) (xs : string list) =
    String.Join(sep, xs)

// Recursively pretty-print AComp
let rec ppAComp (c : AComp) : string =
    match c with
    | ApplyA(fn, args) ->
        let argStrs = args |> List.map ppAVal |> joinWith ", "
        sprintf "%s(%s)" fn argStrs

    | AddA(v1, v2) ->
        sprintf "%s + %s" (ppAVal v1) (ppAVal v2)

    | ArrLitA(vals, tp) ->
        let vs = vals |> List.map ppAVal |> joinWith "; "
        sprintf "{%s}" vs

    | IndexA(arr, idx, tp) ->
        sprintf "%s[%s]" (ppAVal arr) (ppAVal idx)
    
    | LenA (arr) -> 
        sprintf "len(%s)" (ppAVal arr)

    | IfA(cond, aThen, aElse) ->
        // For If, we can inline or show multi-line
        // We'll inline for simplicity
        sprintf "if %s then (\n\n%s)\n\n else (\n%s)\n\n" 
                (ppAVal cond)
                (ppANorm aThen)
                (ppANorm aElse)

// Recursively pretty-print ANorm
// We'll produce lines that look like:
//   let x = ...
//   in ...
and ppANorm (a : ANorm) : string =
    match a with
    | ValueA(v) ->
        ppAVal v

    | LetA(x, tp, c, body) ->
        let rhs = ppAComp c
        let bodyStr = ppANorm body
        match tp with
        | MiniFasto.Int -> sprintf "letI %s = %s in\n%s" x rhs bodyStr
        | MiniFasto.Array _ -> sprintf "letA %s = %s in\n%s" x rhs bodyStr
    | IncA(x, body) ->
        let bodyStr = ppANorm body
        sprintf "inc %s in\n%s" x bodyStr
    | DecA(x, body) ->
        // e.g. drop x in body
        // We'll do:
        // drop x
        // <ppBody>
        let bodyStr = ppANorm body
        sprintf "dec %s in\n%s" x bodyStr

// For convenience, define a top-level function that
// produces a nice multi-line string
// We'll just call "ppANorm a" and maybe trim
let prettyPrint (a : ANorm) : string =
    ppANorm a

let ppAFunDec (f : AFunDec) : string =
    let (AFunDec (fname, tp, args, A)) = f
    let argString = args |> List.fold (fun s (MiniFasto.Param (arg,tp)) -> s + arg + ": " + ppType tp + ", " ) "" 
    let argString' = argString.[.. argString.Length - 3]
    sprintf "%s(%s):\n%s" fname argString' (prettyPrint A)

let prettyPrintProg (prog : AProg) : string =
    prog |> List.fold (fun s fn -> s + ppAFunDec fn + "\n\n") ""