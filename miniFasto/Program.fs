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
let e4 = Length(e2)
let e5 = Let("a", e2, Let("b", e2, Plus(Index("a", Constant(IntVal 2), Int), Index("b", Constant(IntVal 4), Int) )))
let e6 = Let("a", Constant(IntVal 1), Plus (Var "a", Constant(IntVal 2)))

let p1 = buildProgExpr e1
let p2 = buildProgExpr e2
let p3 = buildProgExpr e3
let p4 = buildProgExpr e4
let p5 = buildProgExpr e5

let unusedArgFun =
    FunDec("unused",
           Int,
           [ Param("x", Int) ; Param("y", Int)],
           Plus(Var("x"), Var("x")))
let mainFunUnused =
    FunDec("main",
           Int,
           [],
           Apply("unused", [Constant(IntVal 5) ; Constant(IntVal 3)]))

let unusedProg : Prog = [ unusedArgFun; mainFunUnused]

// len(x) + len(y) + len(z)
let dupLenFun =
    FunDec("dupLen",
           Int,
           [ Param("x", Array Int)
             Param("y", Array Int)
             Param("z", Array Int) ],
           Plus( Length(Var "x"),
                 Plus(Length(Var "y"), Length(Var "z"))))

// main: a duplicated (a is passed twice); a & b never used again
let mainDup =
    FunDec("main",
           Int,
           [],
           Let("a",
               ArrayLit(List.init 5 (fun i -> Constant(IntVal i)), Int),
               Let("b",
                   ArrayLit(List.init 3 (fun i -> Constant(IntVal (i+10))), Int),
                   Apply("dupLen", [ Var "a"; Var "b"; Var "a" ]))))

let progDup : Prog = [ dupLenFun; mainDup ]

// return arr[0]
let firstElemFun =
    FunDec("firstElem",
           Int,
           [ Param("arr", Array Int) ],
           Index("arr", Constant(IntVal 0), Int))

// main: array a is used again (Length) after the call
let mainAfterUse =
    FunDec("main",
           Int,
           [],
           Let("a",
               ArrayLit(List.init 4 (fun i -> Constant(IntVal (i+1))), Int),
               Let("tmp",
                   Apply("firstElem", [ Var "a" ]),
                   Length(Var "a"))))      // ← last use of a

let progAfterUse : Prog = [ firstElemFun; mainAfterUse ]

// p[0] + q[0]
let sumHeadsFun =
    FunDec("sumHeads",
           Int,
           [ Param("p", Array Int)
             Param("q", Array Int) ],
           Plus( Index("p", Constant(IntVal 0), Int),
                 Index("q", Constant(IntVal 0), Int)))

let mainMix =
    FunDec("main",
           Int,
           [],
           Let("p",
               ArrayLit(List.init 3 (fun i -> Constant(IntVal (i+1))), Int),
               Let("q",
                   ArrayLit(List.init 2 (fun i -> Constant(IntVal (i+4))), Int),
                   Let("tmp",
                       Apply("sumHeads", [ Var "p"; Var "q" ]),
                       Plus(Length(Var "p"), Var "tmp")))))   // p lives on, q doesn't

let progMix : Prog = [ sumHeadsFun; mainMix ]

(* ----------------------------------------------------------- *)
(*  if-arrays example                                          *)
(* ----------------------------------------------------------- *)

/// then-branch (b1)
let b1 : TypedExp =
    Let("t1", Index("arr1", Constant(IntVal 1), Int),      // t1 = arr1[1]
    Let("t2", Index("arr3", Constant(IntVal 2), Int),      // t2 = arr3[2]
    Let("t3", Plus(Var "t1", Var "t2"),                    // t3 = t1 + t2
        Var "t3")))                                        // result  t3

/// else-branch (b2)
let b2 : TypedExp =
    Let("t1", Index("arr2", Constant(IntVal 2), Int),      // t1 = arr2[2]
    Let("t2", Index("arr4", Constant(IntVal 1), Int),      // t2 = arr4[1]
    Let("t3", Plus(Var "t1", Var "t2"),                    // t3 = t1 + t2
        Var "t3")))

/// whole body of main
let mainBody : TypedExp =
    Let ("g", Constant(IntVal 1), 
    Let("arr1", ArrayLit([ Constant(IntVal 1)
                           Constant(IntVal 2)
                           Constant(IntVal 3) ], Int),
    Let("arr2", ArrayLit([ Constant(IntVal 2)
                           Constant(IntVal 3)
                           Constant(IntVal 4) ], Int),
    Let("arr3", ArrayLit([ Constant(IntVal 3)
                           Constant(IntVal 4)
                           Constant(IntVal 5) ], Int),
    Let("arr4", ArrayLit([ Constant(IntVal 4)
                           Constant(IntVal 5)
                           Constant(IntVal 6) ], Int),
    Let("x", If(Var "g", b1, b2),                          // x = if g then b1 else b2
    Let("y", Index("arr4", Constant(IntVal 0), Int),       // y = arr4[0]
    Let("z", Plus(Var "y", Var "x"),                       // z = y + x
        Var "z"))))))))                                     // result  z

/// main g = ...
let mainIfArrays =
    FunDec("main",
           Int,
           [],    // --> pass 0/1 (false/true) when you run it
           mainBody)

/// whole program
let progIfArrays : Prog = [ mainIfArrays ]

let ifArraySelect : TypedExp =
    Let("g", Constant(IntVal 1),
    Let("y", ArrayLit([ Constant(IntVal 1)
                        Constant(IntVal 2)
                        Constant(IntVal 3) ], Int),
    Let("z", ArrayLit([ Constant(IntVal 4)
                        Constant(IntVal 5)
                        Constant(IntVal 6) ], Int),
    Let("x", If(Var "g", Var "y", Var "z"),
    Let("v", Index("x", Constant(IntVal 2), Int),
    //Let("u", Index("z", Constant(IntVal 0), Int),
        Var "v")))))//)

let progIfArraySelect : Prog = buildProgExpr ifArraySelect

open MiniFasto                          // just to bring the names in scope

// array literals
let yArr : TypedExp =
    ArrayLit ( [ Constant (IntVal 1)
                 Constant (IntVal 2)
                 Constant (IntVal 3) ],
               Int )                    // element type

let zArr : TypedExp =
    ArrayLit ( [ Constant (IntVal 4)
                 Constant (IntVal 5)
                 Constant (IntVal 6) ],
               Int )

// condition for the if - expression
let cond : TypedExp = Constant (IntVal 1)   // “true”

// build the nested lets
let ifelseCopy : TypedExp =
    Let ("y", yArr,
     Let ("z", zArr,
      Let ("x", If (cond, Var "y", Var "z"),
       Let ("v", Index ("x", Constant (IntVal 1), Int),  // v = x[1]
        Let("u", Index ("y", Constant (IntVal 1), Int),
        Var "v")))))

// wrap it in a one-function programme
let ifelseCopyProg : Prog =
    [ FunDec ("main",              // function name
              Int,                 // return type
              [],                  // no parameters
              ifelseCopy) ]          // function body

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
    printfn "e6:\n%s" (e6 |> anf |> prettyPrint)
    *)
    printfn "e5:\n%s" (p5 |> anfProg |> prettyPrintProg)
    printfn "p tripple:\n%s" (multiProg |> anfProg |> prettyPrintProg)
    printfn "p unused:\n%s" (unusedProg |> anfProg |> prettyPrintProg)
    printfn "Same array given as two function args:\n%s" (progDup      |> anfProg |> prettyPrintProg)
    printfn "Array is still alive after given as a args:\n%s" (progAfterUse |> anfProg |> prettyPrintProg)
    printfn "One array dies after function, the other is still live\n%s" (progMix      |> anfProg |> prettyPrintProg)
    printfn "If program: :\n%s" (progIfArrays |> anfProg |> prettyPrintProg)
    runTestA progIfArrays "lol"
    printfn "If program: :\n%s" (progIfArraySelect |> anfProg |> prettyPrintProg)
    printfn "If copy program: :\n%s" (ifelseCopyProg |> anfProg |> prettyPrintProg)
    0