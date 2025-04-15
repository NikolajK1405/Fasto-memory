module ANFPretty
open ANorm
open System
(* pretty printing A-normal form, Made by chatgpt, just for testing.. *)


// Helper: Pretty-print an AVal
let rec ppAVal (v : AVal) : string =
    match v with
    | N n -> string n
    | V x -> x
    | Arr (vs, _) -> (vs |>List.fold (fun acc v -> acc + ", " + (ppAVal v)) "{") + "}"

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
    
    | LenA (arr,_) -> 
        sprintf "len(%s)" (ppAVal arr)

    | IfA(cond, aThen, aElse) ->
        // For If, we can inline or show multi-line
        // We'll inline for simplicity
        sprintf "if %s then (%s) else (%s)" 
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

    | LetA(x, c, body) ->
        // e.g. let x = c in body
        // We'll do:
        // let x = <ppC>
        // in <ppBody>
        let rhs = ppAComp c
        let bodyStr = ppANorm body
        sprintf "let %s = %s in\n%s" x rhs bodyStr

    | DropA(x, body) ->
        // e.g. drop x in body
        // We'll do:
        // drop x
        // <ppBody>
        let bodyStr = ppANorm body
        sprintf "drop %s in\n%s" x bodyStr

// For convenience, define a top-level function that
// produces a nice multi-line string
// We'll just call "ppANorm a" and maybe trim
let prettyPrint (a : ANorm) : string =
    ppANorm a