module RandomGen
open System            // Random, List, etc.
open MiniFasto         // Int, Array, TypedExp, …

(* Create random programs, just for testing, made by chat GPT*)

// -----------------------------------------------------------------------------
// helpers
// -----------------------------------------------------------------------------
let rnd              = Random()
let between lo hi    = rnd.Next (lo, hi+1)
let inline pick (xs: 'a[]) = xs.[rnd.Next xs.Length]
let mutable idCnt = 0
let fresh pre       = idCnt <- idCnt + 1; pre + string idCnt

// -----------------------------------------------------------------------------
// tiny DSL helpers
// -----------------------------------------------------------------------------
let intC n           = Constant (IntVal n)
let var  s           = Var s

// -----------------------------------------------------------------------------
// generate an *Int* expression (no arrays here)
// -----------------------------------------------------------------------------
let rec genIntExpr depth : TypedExp =
    if depth = 0 then intC (between -9 9) else
    pick [|
        (fun () -> intC (between -20 20))
        (fun () -> Plus (genIntExpr (depth-1), genIntExpr (depth-1)))
    |] ()

// -----------------------------------------------------------------------------
// generate a small array literal of Ints
// -----------------------------------------------------------------------------
let genArrayLit depth =
    let len   = between 1 4
    let elems = List.init len (fun _ -> genIntExpr (max 0 (depth-1)))
    ArrayLit (elems, Int)

// -----------------------------------------------------------------------------
// helper-function generator
// -----------------------------------------------------------------------------
type FunSig = string * Type * Type list   // name * return * param list

let genHelperFun () : FunSig * FunDec =
    let fname = fresh "f"
    // single parameter: an array of ints
    let pName = "a"
    let param = Param (pName, Array Int)

    // randomly decide whether the helper returns Int or Array
    if rnd.NextDouble() < 0.5 then
        // return *array* unchanged  ──────────────────────────
        let retTp = Array Int
        let body  = var pName
        ((fname, retTp, [Array Int]),
         FunDec (fname, retTp, [param], body))
    else
        // return *length*           ──────────────────────────
        let retTp = Int
        let body  = Length (var pName)
        ((fname, retTp, [Array Int]),
         FunDec (fname, retTp, [param], body))

// -----------------------------------------------------------------------------
// build a whole program
// -----------------------------------------------------------------------------
let genProg size : Prog =
    // create 1–3 helpers
    let helperCount = between 1 3
    let sigs, helpers =
        List.init helperCount (fun _ -> genHelperFun ())
        |> List.unzip
    let sigs = sigs |> List.toArray

    // make one fresh array literal (argument for the call)
    let arrExpr = genArrayLit (size/2)

    // pick a helper at random to call from main
    let (hName, hRet, _hPars) = pick sigs

    // expression that calls the helper
    let callExpr = Apply (hName, [ arrExpr ])

    // ensure main still *returns an Int* so the harness can compare results
    // Randomly decide whether to wrap the call in an if-else
    let mainBody : TypedExp =
        let makeIfBranch () =
            match hRet with
            | Int ->
                Plus(callExpr, intC (between 0 5))
            | Array Int ->
                Length callExpr
            | _ -> failwith "unexpected return type"

        if rnd.NextDouble() < 0.5 then
            // create a fresh variable g and bind it to a random 0/1 guard
            let g = fresh "g"
            let guardVal = Plus (Constant (IntVal (between 0 1)), Constant (IntVal 0))   // 0 or 1
            Let (g, guardVal,
                If (Var g,
                    makeIfBranch (),
                    makeIfBranch ()))
        else
            match hRet with
            | Int       -> callExpr
            | Array Int -> Length callExpr
            | _         -> failwith "unexpected return type"

    let mainFun = FunDec ("main", Int, [], mainBody)

    // helpers ++ [main]
    helpers @ [ mainFun ]