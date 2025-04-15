module ANorm
open MiniFasto

type AVal =
  | N of int
  | V of string
  | Arr of AVal list * Type

type AComp = 
  | ApplyA of string * AVal list
  | AddA of AVal * AVal
  | ArrLitA of AVal list * Type
  | IndexA of AVal * AVal * Type
  | LenA of AVal * Type
  | IfA of AVal * ANorm * ANorm

and ANorm =
  | ValueA of AVal
  | LetA of string * AComp * ANorm
  | DropA of string * ANorm

type VarTableA = SymTab<AVal>

(* K is continuation function *)
let rec flat (e : TypedExp) (vtab : VarTableA) (k : AVal -> ANorm): ANorm =
    match e with
    | Constant (IntVal n) ->
      k (N n)

    | Constant (ArrayVal (vs, tp)) ->
      (* Turn into arraylit to reuse code *)
      let arraylit = ArrayLit(List.map (fun v -> Constant (v)) vs, tp)
      flat arraylit vtab k

    | Var id ->
      let v = match lookup id vtab with
              | Some var -> var
              | None -> failwith (sprintf "Undefined variable: %s" id)
      k v

    | ArrayLit (elems, tp) -> 
      (*  We want:
          let t1 = ...in
          ...
          let ti = ...in
          let arr = {t1,...,ti} in
          k(arr) *)
      
      (* Takes a list of values, makes it into a computation and puts in a let, 
         final step in arraylit, used as base for flatl accumulator function *)
      let letBase (vals : AVal list) : ANorm =
          let tArr = newVar "arr"
          let comp  = ArrLitA (vals, tp)
          LetA(tArr, comp, k (V tArr))
      
      flatl elems vtab letBase

    | Plus(e1, e2) ->
      (* we use the continuation function to recursivly compile each expression 
         and put it together in a let in the end *)
      flat e1 vtab (fun l ->
          flat e2 vtab (fun r ->
              let t = newVar "t" 
              LetA (t, AddA (l, r), k (V t))))

    | Let(id, e1, e2) ->
      (* in order to enforce grammar, we make a v+0 computation, 
         unsure if this is the smart thing to do *)
      flat e1 vtab (fun v1 ->
          let var = newVar "let"
          let vtab1 = bind id (V var) vtab
          LetA (var, AddA(v1, N 0), 
                  flat e2 vtab1 (fun v2 -> k v2)))

    | Index(id, e, tp) -> 
      let arr = match lookup id vtab with
                | Some ar -> ar
                | None -> failwith (sprintf "Undefined array: %s" id)

      flat e vtab (fun i ->
          let t = newVar "i"
          LetA (t, IndexA(arr, i, tp), k (V t)))

    | Length(e, tp) ->
      flat e vtab (fun arr ->
          let t = newVar "arr"
          LetA (t, LenA (arr, tp), k (V t)))

    | Apply (f, es) -> 
      (* Simmilar approach as array lit, use flatl accumulator function to flatten each element
          make a base function for the final step *)
      let letBase (vs : AVal list) =
        let t = newVar "fRes"
        let applComp = ApplyA (f, vs)
        LetA (t, applComp, k (V t))
      
      flatl es vtab letBase

    (* Not yet implemented *)
    | If (e1, e2, e3) -> ValueA(N 4)

(* accumulator function to flatten a list of expression *)
and flatl (es : TypedExp list) (vtab : VarTableA) (acc : AVal list -> ANorm): ANorm =
    match es with 
    | [] -> acc []
    | e::rst ->
      flat e vtab (fun v ->
          flatl rst vtab (fun vs ->
              acc (v::vs)))

let anf (e : TypedExp) =
    flat e (empty()) (fun v -> ValueA v)

(* Generate Risc-v code from A normal form, split into three for the different types *)
let rec genVal (v : AVal) (vtab : VarTableC) (place : reg) : PseudoRV list =
    match v with 
    | N n -> [ LI (place, n)]
    | V var -> match lookup var vtab with
                | Some r -> [ MV (place, r) ]
                | None -> failwith (sprintf "Unkown variable %s" var)
    | Arr (vs, tp) -> genArrLit vs tp vtab place
and genComp (c : AComp) (vtab : VarTableC) (place : reg) : PseudoRV list =
    match c with 
    | AddA (v1, v2) -> 
      let t1 = newReg "plus_L"
      let t2 = newReg "plus_R"
      let code1 = genVal v1 vtab t1
      let code2 = genVal v2 vtab t2
      code1 @ code2 @ [ADD (place,t1,t2)]

    | ArrLitA (vs, tp) -> genArrLit vs tp vtab place

    | IndexA (arr, i, tp) -> 
      let arrReg = newReg "arrPtr"
      let iReg = newReg "index"
      (* Gen code for the array *)
      let codeArr = genVal arr vtab arrReg

      (* Generate code for the index *)
      let codei = genVal i vtab iReg

      (* Skip header word *)
      let initCode = [ ADDI (arrReg, arrReg, 1) ]

      (* Add the index to the array pointer *)
      let finalCode = [
        ADD (arrReg, arrReg, iReg)
        LW (place, arrReg, 0)
      ]

      codeArr @ codei @ initCode @ finalCode

    | LenA (arr, tp) -> 
      (* load word from arr pointer *)
      let arrAddr = newReg "len_arr"
      let code1    = genVal arr vtab arrAddr
      code1 @ [ LW(place, arrAddr, 0) ]

    | ApplyA (id, args) -> 
      (* Compile each argument expression, place in the symbolic argument registers *)
      let argsCode = args
                    |> List.mapi (fun i a -> genVal a vtab (getArgReg i id)) 
                    |> List.concat
      argsCode @ [CALL id; MV(place, Rret)]

      (* Not implemented *)
    | IfA (v, a1, a2) -> [LI (place, 0)]
and gen (a : ANorm) (vtab : VarTableC) (place : reg) : PseudoRV list =
    match a with
    | ValueA v -> genVal v vtab place
    | LetA (id, c, a) -> 
      let t = newReg ("let_" + id)
      (* Gen c and put result in t *)
      let code1 = genComp c vtab t
      let vtab1 = bind id t vtab
      (* Gen a with new vtable *)
      let code2 = gen a vtab1 place
      code1 @ code2
    
    (* Not implemented *)
    | DropA (id, a) -> [LI (place, 0)]   

(* Since its needed for two cases in gen function, we define arrLit as a function *)
and genArrLit (elems : AVal list) (tp : Type) (vtab : VarTableC) (place : reg) : PseudoRV list =
    let sizeReg = newReg "size"
    let addrReg = newReg "addr"
    let tmpReg  = newReg "tmp"

    (* Store size in size_reg, allocate, put addres of first elm in addr reg *)
    let header = [ LI (sizeReg, List.length elems)
                ; ALLOC (place, sizeReg)
                ; ADDI (addrReg, place, 1) ]
    
    (* Compile each expression from list, store it in addrReg addres, increment addrReg*)
    let genElm v =
        let elmCode = genVal v vtab tmpReg
        elmCode @
        [ SW (tmpReg, addrReg, 0)
        ; ADDI (addrReg, addrReg, 1) ]
    
    let elemsCode = List.collect genElm elems
    header @ elemsCode

let genFun (fn : FunDec) : (string * (reg list * PseudoRV list)) =
    let (FunDec (fname, tp, args, exp)) = fn
    (* Get function label *)
    let funLab = getFname fname

    (* Make sure each parameter exists in a vtable before flattening the expression to a normal form *)
    let (_, anfVtab, _) =
      args |> List.fold (fun (i, tab, regs) (Param(arg,_)) ->
          (i+1, bind arg (V arg) tab, regs))
          (0, empty(), [])

    let aN = flat exp anfVtab (fun v -> ValueA v)

    (* Bind each formal argument to the symbolic argument registers 
       We use a list.fold to accumulate the vtable along with a counter 
       for generation of argument registers *)
    let (_, vtab, argRegs) = 
        args 
        |> List.fold (fun (i, tab, regs) (Param (arg, _)) -> 
         let regName = getArgReg i fname
         i+1, bind arg regName tab, regs@[regName])
         (0, empty(), [])
    (* flatten and generate expression using argument registers and place in return register *)
    let fCode = gen aN vtab Rret
    fname, (argRegs, [LABEL funLab] @ fCode @ [RET])

(* Each function returns an element of a map, a function name and argument registers with the function code *)
let genProg (prog : Prog) : RVProg =
    counter <- 1
    List.map genFun prog 
    |> Map.ofList

let genSimulate (prog : Prog) : (int * Heap) =
    genProg prog |> simulate

let runTestA (prog : Prog) (pname : string) =
    let (resC, _) = genSimulate prog
    let resI = match evalProg prog with
               | IntVal v -> v
               | ArrayVal (_,_) -> 0 (* TODO, implement array testing*)
    if resC = resI then
         printfn "Sucess! Test: %s, result: %i" pname resC
    else printfn "Fail... Test: %s, expected %i, got %i" pname resI resC

(* Turns an expression into RVProg, since functions are not yet implemented we cheat a bit *)
let flatGen (e : TypedExp) : RVProg =
    let a = anf e
    let rv = gen a (empty()) Rret
    Map [("main", ([], [LABEL "f.main"]@rv@[RET]))]

(* Other apporach to flat.
   Instead of continuation function, return final value of the expression 
   Also define anf appender *)
(*
let rec append (e1: ANorm) (e2: ANorm) : ANorm =
    match e1 with
    | ValueA _ -> e2
    | LetA(x, C, A) -> LetA(x, C, append A e2)
    | DropA(x, A) -> DropA(x, append A e2)

let rec flat2 (e : TypedExp): ANorm * AVal =
    match e with
    | Constant (IntVal n) ->
      ValueA (N n), N n

    | Constant (ArrayVal (vs, tp)) ->
      (* Turn into arraylit to reuse code *)
      let arraylit = ArrayLit(List.map (fun v -> Constant v) vs, tp)
      flat2 arraylit

    | Var id ->
      ValueA (V id), V id

    | ArrayLit (elems, tp) -> ValueA(N 4), N 4

    | Plus(e1, e2) -> 
      let A1, v1 = flat2 e1
      let A2, v2 = flat2 e2
      let t = newVar "t"

      let combined = append (append A1 A2) (LetA (t, AddA (v1, v2), ValueA (V t)))
      combined, V t

    | Let(id, e1, e2) -> ValueA(N 4), N 4
    | Index(id, e, t) -> ValueA(N 4), N 4
    | Length(e, _) -> ValueA(N 4), N 4
    | If (e1, e2, e3) -> ValueA(N 4), N 4
    | Apply (f, es) -> ValueA(N 4), N 4


*)