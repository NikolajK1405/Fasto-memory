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


let anfTest = Plus(Constant (IntVal 2), Constant (IntVal 1))
let anfTest2 = Plus(Plus(Constant (IntVal 1), Constant (IntVal 2)), Plus(Constant (IntVal 3), Constant (IntVal 4)))
let anfTest3 = ArrayLit (List.init 4 (fun i -> Constant (IntVal i)), Int)
let anfTest4 = ArrayLit ([anfTest;anfTest2], Int)

let e1 = Plus(Constant(IntVal 4), Constant(IntVal 20))
let e2 = ArrayLit(List.init 5 (fun x -> Constant(IntVal x)), Int)
let e3 = Let("a", e2, Index("a", Constant(IntVal 3), Int))
let e4 = Length(e2, Array Int)
let e5 = Let("a", e2, Let("b", e2, Plus(Index("a", Constant(IntVal 2), Int), Index("b", Constant(IntVal 4), Int) )))
let e1r,_ = simulate (flatGen e1)
let e2r,_ = simulate (flatGen e2)
let e3r,_ = simulate (flatGen e3)
let e4r,_ = simulate (flatGen e4)
let e5r,_ = simulate (flatGen e5)



[<EntryPoint>]
let main argv =
    runTest progSum "Sum"
    runTest progIf "If"
    runTest multiProg "3 funs"
    runTest progSumDown "Recursive"
    printfn "Anf test: \n%s" (prettyPrint(anf anfTest4))
    printfn "e1 res: %A" e1r
    printfn "e2 res: %A" e2r
    printfn "e3 res: %A" e3r
    printfn "Anf test: \n%s" (prettyPrint(anf e3))
    printfn "e4 res: %A" e4r
    printfn "e5 res: %A" e5r
    runTestA progSum "Sum"
    runTestA multiProg "3 funs"
    0