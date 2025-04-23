open MiniFasto
open ANorm
open ANFPretty
(* Program with one function *)
let sumFun = FunDec(
    "sum",
    Int,
    [ Param("a",Int); Param("b",Int) ],
    Plus( Var("a"), Var("b") ) 
)
let eSum : TypedExp =
    Apply("sum", [ Constant(IntVal 10)
                   Constant(IntVal 32) ]) 
let mainFun = FunDec(
    "main", Int, [], eSum
)

let progSum : Prog = [ sumFun; mainFun ]

(* If test *)
let eIfTest : TypedExp =
    If(Constant(IntVal 0),
       Constant(IntVal 123),
       Constant(IntVal 999)
    )
let mainFunIf = FunDec(
    "main", Int, [], eIfTest
)

let progIf : Prog = [ sumFun; mainFunIf ]

(* Program with 3 functions *)
let doubleFun = 
    FunDec("double", 
           Int, 
           [ Param("x", Int) ], 
           Plus(Var("x"), Var("x")))
let tripleFun =
    FunDec("triple",
           Int,
           [ Param("x", Int) ],
           Plus(Var("x"), Plus(Var("x"), Var("x"))))
let mainFun2 =
    FunDec("main",
           Int,
           [],
           Apply("sum", [
               Apply("triple", [ Constant(IntVal 4) ]);
               Apply("double", [ Constant(IntVal 2) ])
           ]))

let multiProg : Prog = [ sumFun; doubleFun; tripleFun; mainFun2 ]


(* Recursive function *)
let sumToFun = FunDec(
    "sumTo",
    Int,
    [ Param("n", Int) ],
    If(
        Var("n"),
        Plus(
            Var("n"),
            Apply("sumTo", [ Plus(Var("n"), Constant(IntVal -1)) ])
        ),
        Constant(IntVal 0) 
    )
)

let mainSumTo = FunDec(
    "main", Int, [],
    Apply("sumTo", [ Constant(IntVal 5) ]) // 5 + 4 + 3 + 2 + 1 = 15
)

let progSumDown : Prog = [ sumToFun; mainSumTo ]

(* Build program with a function list and main function*)
let buildProg (funs : (string * Param list * TypedExp) list)
              (mainBody : TypedExp)
              : Prog =

    let funDecs =
        funs
        |> List.map (fun (name, pars, body) ->
            FunDec(name, Int, pars, body))

    let mainDec = FunDec("main", Int, [], mainBody)

    funDecs @ [mainDec]  

(* Build program from just an expression *)
let buildProgExpr (mainBody : TypedExp) : Prog =
    // simply wrap the expression in a main function
    [ FunDec("main", Int, [], mainBody) ]


let anfTest = Plus(Constant (IntVal 2), Constant (IntVal 1))
let anfTest2 = Plus(Plus(Constant (IntVal 1), Constant (IntVal 2)), Plus(Constant (IntVal 3), Constant (IntVal 4)))
let anfTest3 = ArrayLit (List.init 4 (fun i -> Constant (IntVal i)), Int)
let anfTest4 = ArrayLit ([anfTest;anfTest2], Int)

let e1 = Plus(Constant(IntVal 4), Constant(IntVal 20))
let e2 = ArrayLit(List.init 5 (fun x -> Constant(IntVal x)), Int)
let e3 = Let("a", e2, Index("a", Constant(IntVal 3), Int))
let e4 = Length(e2, Array Int)
let e5 = Let("a", e2, Let("b", e2, Plus(Index("a", Constant(IntVal 2), Int), Index("b", Constant(IntVal 4), Int) )))
let e6 = Let("a", Constant(IntVal 1), Plus (Var "a", Constant(IntVal 2)))

let p1 = buildProgExpr e1
let p2 = buildProgExpr e2
let p3 = buildProgExpr e3
let p4 = buildProgExpr e4
let p5 = buildProgExpr e5

[<EntryPoint>]
let main argv =
    runTestA p1 "p1"
    runTestA p2 "p2"
    runTestA p3 "p3"
    runTestA p4 "p4"
    runTestA p5 "p5"
    runTestA progSum "Sum"
    runTestA progIf "If"
    runTestA multiProg "3 funs"
    runTestA progSumDown "Recursive"
    (*
    printfn "e1:\n%s" (e1 |> anf |> prettyPrint)
    printfn "e2:\n%s" (e2 |> anf |> prettyPrint)
    printfn "e3:\n%s" (e3 |> anf |> prettyPrint)
    printfn "e4:\n%s" (e4 |> anf |> prettyPrint)
    printfn "e5:\n%s" (e5 |> anf |> prettyPrint)
    printfn "e5 analysed:\n%s" (e5 |> anf |> (fun x -> analyse x Set.empty)|> fst |> prettyPrint)
    printfn "e6:\n%s" (e6 |> anf |> prettyPrint)*)
    printfn "p5:\n%s" (multiProg |> anfProg |> prettyPrintProg)
    0