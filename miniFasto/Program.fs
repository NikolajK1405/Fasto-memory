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
let buildProgExpr (mainBody : TypedExp) (retType : Type): Prog =
    // simply wrap the expression in a main function
    [ FunDec("main", retType, [], mainBody) ]


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

let p1 = buildProgExpr e1 Int
let p2 = buildProgExpr e2 (Array Int)
let p3 = buildProgExpr e3 Int
let p4 = buildProgExpr e4 Int
let p5 = buildProgExpr e5 Int

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

let progIfArraySelect : Prog = buildProgExpr ifArraySelect Int

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
     Let ("g", Plus (Constant (IntVal 1),Constant (IntVal 1)),
      Let ("x", If (Var "g", Var "y", Var "z"),
       Let ("v", Index ("x", Constant (IntVal 1), Int),  // v = x[1]
        Let("u", Index ("y", Constant (IntVal 1), Int),
        Let("p", Plus(Var "u",Var "v"),
        Var "p")))))))

// wrap it in a one-function programme
let ifelseCopyProg : Prog =
    [ FunDec ("main",              // function name
              Int,                 // return type
              [],                  // no parameters
              ifelseCopy) ]          // function body
open RandomGen

let fuzz count size =
  for i = 1 to count do
    let prog = genProg size
    try
      //printfn "fuzz-%d:\n%s" i (prog |> anfProg |> prettyPrintProg)
      runTestA prog (sprintf "fuzz-%d" i)
    with ex ->
      printfn "‼️ crash in fuzz-%d: %s" i ex.Message
      printfn "%s" (prog |> anfProg |> prettyPrintProg)
      raise ex

let progDeadLit : Prog =
  [ FunDec ("main", Int, [],
            Let ("tmp", ArrayLit ([Constant(IntVal 1)], Int),
                 Constant (IntVal 42))) ]

let progReturnArr : Prog =
  [ FunDec ("main", Array Int, [],
            ArrayLit ([Constant(IntVal 7); Constant(IntVal 8)], Int)) ]

let helperPassthrough =
      FunDec ("id", Array Int, [Param("a", Array Int)], Var "a")

let progPassThru : Prog =
  [ helperPassthrough
    FunDec ("main", Int, [],
      Let("arr", ArrayLit([Constant(IntVal 1)], Int),
        Let("b", Apply("id",[Var "arr"]),
          Length (Var "b")))) ]

let dupLen =
      FunDec ("dblLen", Int,
              [Param("x",Array Int); Param("y",Array Int)],
              Plus(Length(Var "x"), Length(Var "y")))

let progAlias : Prog =
  [ dupLen
    FunDec ("main", Int, [],
      Let ("a", ArrayLit([Constant(IntVal 2);Constant(IntVal 3)],Int),
        Apply("dblLen",[Var "a"; Var "a"]))) ]

let progBranchLocal : Prog =
  [ FunDec ("main", Int, [],
      Let("g", Constant(IntVal 0),
        Let("x",
          If(Var "g",
             ArrayLit([Constant(IntVal 10)],Int),    // only here
             Constant(IntVal 0)),
          Plus(Var "g", Constant(IntVal 1)))) ) ]

let progShadow : Prog =
  [ FunDec ("main", Int, [],
      Let("a", ArrayLit([Constant(IntVal 5)],Int),
        Let("a", ArrayLit([Constant(IntVal 6)],Int),
          Length (Var "a")))) ]

let recFun =
      FunDec ("f", Array Int, [Param("n",Int)],
        If(Var "n",
           Let("tmp", ArrayLit([Constant(IntVal 0)],Int),
               Apply("f",[Plus(Var "n",Constant(IntVal -1))])),
           ArrayLit([Constant(IntVal 1)],Int)))

let progRecArr : Prog =
  [ recFun
    FunDec ("main", Int, [],
      Let("res", Apply("f",[Constant(IntVal 5)]),
        Length(Var "res"))) ]

let f3 =
      FunDec ("thrice", Int,
        [Param("p",Array Int); Param("q",Array Int); Param("r",Array Int)],
        Plus(Length(Var "p"), Plus(Length(Var "q"), Length(Var "r"))))

let progManyUses : Prog =
  [ f3
    FunDec ("main", Int, [],
      Let("a", ArrayLit([Constant(IntVal 0);Constant(IntVal 0)],Int),
        Apply("thrice",[Var "a"; Var "a"; Var "a"]))) ]

let prog2DDeadLit : Prog =
  [ FunDec ("main", Array Int, [],
      // build a 2×2 literal, then ignore it
      Let("m", ArrayLit(
              [ ArrayLit([Constant(IntVal 1);Constant(IntVal 2)], Int)
              ; ArrayLit([Constant(IntVal 3);Constant(IntVal 4)], Int)
              ], Array Int),
          Let("a", Index("m", Constant(IntVal 0), Array Int), Var "a")))
  ]

let prog2DReturn : Prog =
  [ FunDec ("main", Array (Array Int), [],
      ArrayLit(
        [ ArrayLit([Constant(IntVal 5)], Int)
        ; ArrayLit([Constant(IntVal 6);Constant(IntVal 7)], Int)
        ], Array Int))
  ]

let id2D =
  FunDec ("id2D", Array (Array Int), [Param("m", Array (Array Int))],
    Var "m")

let prog2DPassThru : Prog =
  [ id2D
    FunDec ("main", Int, [],
      Let("m", ArrayLit(
               [ ArrayLit([Constant(IntVal 1);Constant(IntVal 2)], Int) ]
             , Array Int),
        // pass it through id2D, then take length of first row
        Let("m2", Apply("id2D",[Var "m"]),
          Length(Index("m2", Constant(IntVal 0), Array Int)))))
  ]

let prog2DNestedIndex : Prog =
  [ FunDec ("main", Int, [],
      Let("m", ArrayLit(
        [ ArrayLit([Constant(IntVal 3);Constant(IntVal 4)], Int)
        ; ArrayLit([Constant(IntVal 5)],           Int)
        ], Array Int),
        // shadow m with first row only
        Let("m", Index("m", Constant(IntVal 0), Array Int),
          // then index into its second element
          Index("m", Constant(IntVal 1), Int))))
  ]

let prog2DBranch : Prog =
  [ FunDec ("main", Int, [],
      Let("g", Plus(Constant(IntVal 1),Constant(IntVal 1)),
        Let("r",
          If(Var "g",
             // then-branch builds 1×2 2-D array
             ArrayLit(
               [ ArrayLit([Constant(IntVal 9)], Int) ],
               Array Int
             ),
             // else-branch returns an empty 2-D
             ArrayLit([], Array (Array Int))
          ),
          // now index into the first row (either [9] or error if empty)
          // guard = 1 so we always hit the [9] case
          Let("row", Index("r", Constant(IntVal 0), Array Int),
            Length (Var "row")))))
  ]

let arr2dLiveafter: Prog =
  [ FunDec ("main", Array Int, [],
    Let ("a1", ArrayLit([Constant(IntVal 0)], Int), 
      Let ("a2", ArrayLit([ Var "a1";Var "a1" ], Array Int),
      Let ("i", Index ("a1", Constant(IntVal 0), Int),
      Index("a2", Var "i", Array Int))))
  )
  ]

// ------------------------------------------------------------
// 3-D array tests
// ------------------------------------------------------------

let prog3DDeadLit : Prog =
  [ FunDec ("main", Array (Array Int), [],
      // build a 1×1×1 literal, then slice off its first 2D face
      Let("m3", ArrayLit(
        [ ArrayLit(
            [ ArrayLit([Constant(IntVal 1)], Int) ],
            Array Int)
        ],
        Array (Array Int)),
        Let("slice2D", Index("m3", Constant(IntVal 0), Array (Array Int)),
            Var "slice2D")))
  ]

let prog3DReturn : Prog =
  [ FunDec ("main", Array (Array (Array Int)), [],
      // directly return a 1×1×2 literal
      ArrayLit(
        [ ArrayLit(
            [ ArrayLit([Constant(IntVal 2); Constant(IntVal 3)], Int) ],
            Array Int)
        ],
        Array (Array Int)))
  ]

let id3D =
  FunDec ("id3D", Array (Array (Array Int)), [Param("m3", Array (Array (Array Int)))],
    Var "m3")

let prog3DPassThru : Prog =
  [ id3D
    FunDec ("main", Int, [],
      Let("m3", ArrayLit(
        [ ArrayLit(
            [ ArrayLit([Constant(IntVal 4); Constant(IntVal 5)], Int) ],
            Array Int)
        ],
        Array (Array Int)),
        Let("m4", Apply("id3D",[Var "m3"]),
        Let("slice2", Index("m4", Constant(IntVal 0), Array (Array Int)),
        Let("slice1", Index("slice2", Constant(IntVal 0), Array Int),
            Length (Var "slice1"))))))
  ]

let prog3DNestedIndex : Prog =
  [ FunDec ("main", Int, [],
      Let("m3", ArrayLit(
        [ ArrayLit(
            [ ArrayLit([Constant(IntVal 6)], Int) ],
            Array Int)
        ],
        Array (Array Int)),
        Let("m2", Index("m3", Constant(IntVal 0), Array (Array Int)),
        Let("m1", Index("m2", Constant(IntVal 0), Array Int),
        Index("m1", Constant(IntVal 0), Int)))))
  ]

let prog3DBranch : Prog =
  [ FunDec ("main", Int, [],
      Let("g", Constant(IntVal 1),
      Let("r3",
        If(Var "g",
           // then: build 1×1×1 literal
           ArrayLit(
             [ ArrayLit([ArrayLit([Constant(IntVal 7)], Int)], Array Int) ],
             Array (Array Int)),
           // else: empty 3-D
           ArrayLit([], Array (Array Int))),
        Let("slice2", Index("r3", Constant(IntVal 0), Array (Array Int)),
      Let("slice1", Index("slice2", Constant(IntVal 0), Array Int),
          Length (Var "slice1"))))))
  ]

// ------------------------------------------------------------
// 4-D array tests
// ------------------------------------------------------------

let prog4DDeadLit : Prog =
  [ FunDec ("main", Array (Array (Array Int)), [],
      // build a 1×1×1×1 literal, then slice off its first 3-D block
      Let("m4", ArrayLit(
        [ ArrayLit(
            [ ArrayLit(
                [ ArrayLit([Constant(IntVal 1)], Int) ],
                Array Int)
            ],
            Array (Array Int))
        ],
        Array (Array (Array Int))),
        Let("slice3D", Index("m4", Constant(IntVal 0), Array (Array (Array Int))),
            Var "slice3D")))
  ]

let prog4DReturn : Prog =
  [ FunDec ("main", Array (Array (Array (Array Int))), [],
      // directly return a 1×1×1×2 literal
      ArrayLit(
        [ ArrayLit(
            [ ArrayLit(
                [ ArrayLit([Constant(IntVal 2); Constant(IntVal 3)], Int) ],
                Array Int)
            ],
            Array (Array Int))
        ],
        Array (Array (Array Int))))
  ]

let id4D =
  FunDec ("id4D", Array (Array (Array (Array Int))),
          [Param("m4", Array (Array (Array (Array Int))))],
          Var "m4")

let prog4DPassThru : Prog =
  [ id4D
    FunDec ("main", Int, [],
      Let("m4", ArrayLit(
        [ ArrayLit(
            [ ArrayLit(
                [ ArrayLit([Constant(IntVal 4); Constant(IntVal 5)], Int) ],
                Array Int)
            ],
            Array (Array Int))
        ],
        Array (Array (Array Int))),
        Let("m5", Apply("id4D",[Var "m4"]),
        Let("s3", Index("m5", Constant(IntVal 0), Array (Array (Array Int))),
        Let("s2", Index("s3", Constant(IntVal 0), Array (Array Int)),
        Let("s1", Index("s2", Constant(IntVal 0), Array Int),
            Length (Var "s1")))))))
  ]

let prog4DNestedIndex : Prog =
  [ FunDec ("main", Int, [],
      Let("m4", ArrayLit(
        [ ArrayLit(
            [ ArrayLit(
                [ ArrayLit([Constant(IntVal 6)], Int) ],
                Array Int)
            ],
            Array (Array Int))
        ],
        Array (Array (Array Int))),
        Let("m3", Index("m4", Constant(IntVal 0), Array (Array (Array Int))),
        Let("m2", Index("m3", Constant(IntVal 0), Array (Array Int)),
        Let("m1", Index("m2", Constant(IntVal 0), Array Int),
        Index("m1", Constant(IntVal 0), Int))))))
  ]

let prog4DBranch : Prog =
  [ FunDec ("main", Int, [],
      Let("g", Constant(IntVal 1),
      Let("r4",
        If(Var "g",
           ArrayLit(
             [ ArrayLit(
                 [ ArrayLit(
                     [ ArrayLit([Constant(IntVal 7)], Int) ],
                     Array Int)
                 ],
                 Array (Array Int))
             ],
             Array (Array (Array Int))),
           ArrayLit([], Array (Array (Array Int)))),
        Let("s3", Index("r4", Constant(IntVal 0), Array (Array (Array Int))),
      Let("s2", Index("s3", Constant(IntVal 0), Array (Array Int)),
      Let("s1", Index("s2", Constant(IntVal 0), Array Int),
          Length (Var "s1")))))))
  ]

let mapProg : Prog =
  [ FunDec ("main", Array Int, [],
    Let("a", ArrayLit ([Constant(IntVal 0); Constant(IntVal 1)], Int), 
      Map (Param("x",Int), Plus(Var "x", Constant (IntVal 1)), Var "a", Array Int, Array Int))
  )
  ]

let mapProg2 : Prog =
  [ FunDec ("main", Array Int, [],
    Let ("c", ArrayLit ([Constant(IntVal 4);Constant(IntVal 5);Constant(IntVal 6)], Int),
    Let ("a", ArrayLit ([Constant(IntVal 0);Constant(IntVal 1);Constant(IntVal 2)], Int),
    Map (Param ("x", Int), 
        Let ("d", ArrayLit ([Constant(IntVal 7);Constant(IntVal 8);Constant(IntVal 9)], Int),
          Let ("i", Index ("c", Var "x", Int), 
          Let ("j", Index ("d", Var "x", Int),
          Plus (Var "i", Var "j")))), Var "a", Array Int, Array Int
    )
    )
    )
    )

  ]

let mapProg3 : Prog =
  [ FunDec ("main", Array Int, [],
    Let ("c", ArrayLit ([Constant(IntVal 4);Constant(IntVal 5);Constant(IntVal 6)], Int),
    Let ("a", ArrayLit ([ArrayLit ([Constant(IntVal 0);Constant(IntVal 1);Constant(IntVal 2)], Int);ArrayLit ([Constant(IntVal 1);Constant(IntVal 5);Constant(IntVal 6)], Int)], Array Int),
    Map (Param ("x", Array Int), 
        Let ("d", ArrayLit ([Constant(IntVal 7);Constant(IntVal 8);Constant(IntVal 9)], Int),
          Let ("i", Index ("x", Constant(IntVal 0), Int), 
          Let ("j", Index ("d", Var "i", Int),
          Let ("k", Index ("c", Var "i", Int),
          Plus (Var "i", Var "j"))))), Var "a", Array Int, Array Int
    )
    )
    )
    )
  ]

let mapProg4 : Prog =
  [ FunDec ("main", Array (Array Int), [],
    Let ("c", ArrayLit ([Constant(IntVal 0);Constant(IntVal 1)], Int),
    Let ("a", ArrayLit ([ArrayLit ([Constant(IntVal 0);Constant(IntVal 1);Constant(IntVal 2)], Int);ArrayLit ([Constant(IntVal 1);Constant(IntVal 5);Constant(IntVal 6)], Int)], Array Int),
    Map (Param ("x", Int), 
        Index ("a", Var "x", Array Int), Var "c", Array Int, Array (Array Int))
    )
    )
  )
  ]

// Map function: map((fn x -> x), arr) where arr is live after
let mapProg5 : Prog =
  [ FunDec ("main", Int, [],
    Let ("a", ArrayLit ([ArrayLit ([Constant(IntVal 0);Constant(IntVal 1);Constant(IntVal 2)], Int);ArrayLit ([Constant(IntVal 1);Constant(IntVal 5);Constant(IntVal 6)], Int)], Array Int),
    Let ("arr", Map (Param ("x", Array Int), 
        Var "x", Var "a", Array (Array Int), Array (Array Int)),
        Let ("i", Index("a", Constant(IntVal 0), Array Int),
        Let ("j", Index("arr", Constant(IntVal 0), Array Int), 
        Plus (Index("i", Constant (IntVal 0), Int), Index("j", Constant (IntVal 0), Int)))
    )
  )))
  ]

let at0Fun = FunDec(
    "at0",
    Int,
    [ Param("a",Array Int)],
    Index("a", Constant(IntVal 0), Int) 
)
  
let mainMap = FunDec(
    "main", Array Int, [], Let("a", ArrayLit ([ArrayLit ([Constant(IntVal 0);Constant(IntVal 1);Constant(IntVal 2)], Int);ArrayLit ([Constant(IntVal 1);Constant(IntVal 5);Constant(IntVal 6)], Int)], Array Int), 
      Map (Param("x",Array Int), Apply("at0", [Var "x"]), Var "a", Array (Array Int), Array Int))

)

let mapProg6 : Prog = [ at0Fun; mainMap ]

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
    (*printfn "Recursive:\n%s" (progSumDown |> anfProg |> prettyPrintProg)
    printfn "e5:\n%s" (p5 |> anfProg |> prettyPrintProg)
    printfn "p tripple:\n%s" (multiProg |> anfProg |> prettyPrintProg)
    printfn "p unused:\n%s" (unusedProg |> anfProg |> prettyPrintProg)
    printfn "Same array given as two function args:\n%s" (progDup      |> anfProg |> prettyPrintProg)
    printfn "Array is still alive after given as a args:\n%s" (progAfterUse |> anfProg |> prettyPrintProg)
    printfn "One array dies after function, the other is still live\n%s" (progMix      |> anfProg |> prettyPrintProg)
    printfn "If program: :\n%s" (progIfArrays |> anfProg |> prettyPrintProg)
    printfn "If program: :\n%s" (progIfArraySelect |> anfProg |> prettyPrintProg)
    printfn "If copy program: :\n%s" (ifelseCopyProg |> anfProg |> prettyPrintProg) *)
    runTestA p5 "1"
    runTestA multiProg "2"
    runTestA unusedProg "3"
    runTestA progDup "4"
    runTestA progAfterUse "5"
    runTestA progMix "6"
    runTestA progIfArrays "7"
    runTestA progIfArraySelect "8"
    printfn "pp: :\n%s" (ifelseCopyProg |> anfProg |> prettyPrintProg)
    runTestA ifelseCopyProg "9"
    //fuzz 100 6   // 100 random programs of (rough) size ≤ 6
    runTestA progDeadLit     "dead-literal"
    runTestA progReturnArr   "return-arr"
    runTestA progPassThru    "pass-through"
    runTestA progAlias       "alias"
    runTestA progBranchLocal "branch-local"
    runTestA progShadow      "shadowing"
    runTestA progRecArr      "rec-array"
    runTestA progManyUses    "multi-use"
    //printfn "If copy program: :\n%s" (prog2DDeadLit |> anfProg |> prettyPrintProg)
    runTestA prog2DDeadLit     "2d-dead-lit"
    runTestA prog2DReturn      "2d-return"
    runTestA prog2DPassThru    "2d-pass-thru"
    runTestA prog2DNestedIndex "2d-nested-index"
    runTestA prog2DBranch      "2d-branch"
    runTestA arr2dLiveafter    "2d elm still live after"

    // 3D-array tests
    printfn "pp: :\n%s" (prog3DDeadLit |> anfProg |> prettyPrintProg)
    runTestA prog3DDeadLit     "3d-dead-lit"
    runTestA prog3DReturn      "3d-return"
    runTestA prog3DPassThru    "3d-pass-thru"
    runTestA prog3DNestedIndex "3d-nested-index"
    runTestA prog3DBranch      "3d-branch"

    // 4D-array tests
    runTestA prog4DDeadLit     "4d-dead-lit"
    runTestA prog4DReturn      "4d-return"
    runTestA prog4DPassThru    "4d-pass-thru"
    runTestA prog4DNestedIndex "4d-nested-index"
    runTestA prog4DBranch      "4d-branch"
    printfn "pp1:\n%s" (mapProg |> anfProg |> prettyPrintProg)
    printfn "pp2:\n%s" (mapProg2 |> anfProg |> prettyPrintProg)
    printfn "pp3:\n%s" (mapProg3 |> anfProg |> prettyPrintProg)
    printfn "pp4:\n%s" (mapProg4 |> anfProg |> prettyPrintProg)
    printfn "pp5:\n%s" (mapProg5 |> anfProg |> prettyPrintProg)
    printfn "pp6:\n%s" (mapProg6 |> anfProg |> prettyPrintProg)
    runTestA mapProg "Map1"
    runTestA mapProg2 "Map2"
    runTestA mapProg3 "Map3"
    runTestA mapProg4 "Map4"
    runTestA mapProg5 "Map5"
    runTestA mapProg6 "Map6"
    (*printfn "pp:\n%s" (prog2DDeadLit |> anfProg |> prettyPrintProg)
    printfn "pp:\n%s" (prog2DReturn |> anfProg |> prettyPrintProg)
    printfn "pp:\n%s" (prog2DPassThru |> anfProg |> prettyPrintProg)
    printfn "pp:\n%s" (prog2DNestedIndex |> anfProg |> prettyPrintProg)
    printfn "pp:\n%s" (prog2DBranch |> anfProg |> prettyPrintProg)
    printfn "pp:\n%s" (arr2dLiveafter |> anfProg |> prettyPrintProg)*)
    0