module ANorm
open MiniFasto

type AVal =
  | N of int
  | V of string

type AComp = 
  | ApplyA of string * AVal list
  | AddA of AVal * AVal
  | ArrLitA of AVal list * Type
  | IndexA of AVal * AVal * Type
  | LenA of AVal * Type
  | IfA of AVal * ANorm * ANorm
    (*Indicate that variable is to be used as function argument, so its fine to copy a variable in this case*)
  | ArgA of AVal 

and ANorm =
  | ValueA of AVal
  | LetInt of string * AComp * ANorm
  | LetArr of string * AComp * ANorm
  | DropA of string * ANorm 

(* Set of live variables, used to analyze when we can add drops *)
type LiveSet = Set<string>

type VarTableA = SymTab<AVal>

(* Whenever we generate a typed let in flat we add the variable's type to this table, 
   to be used for finding type of expression *)
type VarTypeTable = SymTab<Type>
let mutable vtytab : VarTypeTable = empty()

(* We create a return table to be used in flat, in order to get typed lets for the case of Apply *)
type RetTable = SymTab<Type>
let getRetTab (prog:Prog) : RetTable =
    prog |> List.fold (fun tab (FunDec(fid, rt, _, _)) ->
              bind fid rt tab) (empty())

(* Helper function to get type of an expression, 
   minimal since we can use the actual type checker of fasto when moving the implementation *)

let rec getTypeExp (e : TypedExp) (vttab : VarTypeTable) (rtab : RetTable) : Type =
    match e with
    | Constant v -> valueType v
    | Var v -> 
      match lookup v vttab with
      | Some tp -> tp
      | None -> failwith (sprintf "Unkown variable in variable type table: %s" v)
    | ArrayLit (_, tp) -> tp
    | Plus (_,_) ->  Int
    | Let (x,e1,e2) ->
      let vttab1 = bind x (getTypeExp e1 vttab rtab) vttab
      getTypeExp e2 vttab1 rtab
    | Index (_,_,tp) -> tp
    | Length (_,_) -> Int
    (* We assume bot branches of if return the same type*)
    | If (_,e1,_) -> getTypeExp e1 vttab rtab
    | Apply (f,_) -> match lookup f rtab with
                     | Some tp -> tp
                     | None -> failwith (sprintf "Unkown function in return table: %s" f)

(* K is continuation function *)
let rec flat (e : TypedExp) (vtab : VarTableA) (vtytab : VarTypeTable) (rtab : RetTable) (k : AVal -> ANorm): ANorm =
    match e with
    | Constant (IntVal n) ->
      k (N n)

    | Constant (ArrayVal (vs, tp)) ->
      (* Turn into arraylit to reuse code *)
      let arraylit = ArrayLit(List.map (fun v -> Constant (v)) vs, tp)
      flat arraylit vtab vtytab rtab k

    | Var id ->
      match lookup id vtab with
              | Some v -> k v
              | None -> failwith (sprintf "Undefined variable: %s" id)

    | ArrayLit (elems, tp) -> 
      (*  We want:
          let t1 = ...in
          ...
          let ti = ...in
          let arr = {t1,...,ti} in
          k(arr) *)
      
      (* Takes a list of values, makes it into a computation and puts in a let, 
         final step in arraylit, used as base for flatl accumulator function *)
      let tArr = newVar "arr"
      let vtytab1 = bind tArr (Array tp) vtytab
      let letBase (vals : AVal list) : ANorm =
          let comp  = ArrLitA (vals, tp)
          LetArr(tArr, comp, k (V tArr))

      flatl elems vtab vtytab1 rtab letBase

    | Plus(e1, e2) ->
      (* we use the continuation function to recursivly compile each expression 
         and put it together in a let in the end 
         This is the only place we need to update the variable type table *)
      let t = newVar "t" 
      let vtytab1 = bind t Int vtytab 
      flat e1 vtab vtytab1 rtab (fun l ->
          flat e2 vtab vtytab1 rtab (fun r ->
              LetInt (t, AddA (l, r), k (V t))))

    | Let(id, e1, e2) ->
      (* call recursivly on e2 with the value from e1, no actual
         LetA types to ensure copy and constant propagation *)
      flat e1 vtab vtytab rtab (fun v1 ->
          let vtab1 = bind id v1 vtab
          let idtp = getTypeExp e1 vtytab rtab
          let vtytab1 = bind id idtp vtytab
          flat e2 vtab1 vtytab1 rtab (fun v2 -> k v2))

    | Index(id, e, tp) -> 
      let arr = match lookup id vtab with
                | Some ar -> ar
                | None -> failwith (sprintf "Undefined array: %s" id)

      let t = newVar "i"
      let vtytab1 = bind t tp vtytab 
      flat e vtab vtytab1 rtab (fun i ->
          let Let = match tp with
                    | Int -> LetInt (t, IndexA(arr, i, tp), k (V t))
                    | Array _ -> LetArr (t, IndexA(arr, i, tp), k (V t))
          Let)

    | Length(e, tp) ->
      let t = newVar "arr"
      let vtytab1 = bind t Int vtytab 
      flat e vtab vtytab1 rtab (fun arr ->
          LetInt (t, LenA (arr, tp), k (V t)))

    | Apply (f, es) -> 
      (* Simmilar approach as array lit, use flatl accumulator function to flatten each element
          make a base function for the final step *)
      let tp = match lookup f rtab with
               | Some ty -> ty
               | None -> failwith (sprintf "Unkown function %s" f)
      
      let t = newVar "fRes"
      let mutable vtytab1 = bind t tp vtytab 

      let letBase (vs : AVal list) =
        (* Generate a new variable name for each function argument *)
        let argnames = List.init (List.length vs) (fun _ -> newVar "arg")
        (* Make them into values to be used for the function call*)
        let argVals  = argnames |> List.map (fun n -> V n)

        (* Create the final let for the apply, using the new variable names as arguments *)
        let applComp = ApplyA (f, argVals)
        let applBase = match tp with
                       | Int -> LetInt (t, applComp, k (V t))
                       | Array _ -> LetArr (t, applComp, k (V t))
        (* Make each val into a let of the form Let arg_x = Arg (val), 
           using the final apply let as a base along with the variable names we generated *)
        (applBase, argnames, vs) 
        |||> List.fold2 (fun a arg v ->
                          match v with
                          | N n  -> vtytab1 <- bind arg Int vtytab1
                                    LetInt (arg, ArgA (N n), a)
                          | V vn -> match lookup vn vtytab1 with
                                    | None -> failwith (sprintf "Unkown variable in variable type table: %s" vn)
                                    | Some ty -> match ty with
                                                  | Int -> vtytab1 <- bind arg Int vtytab1
                                                           LetInt (arg, ArgA (V vn), a)
                                                  | Array _ -> vtytab1 <- bind arg (Array Int) vtytab1
                                                               LetArr (arg, ArgA (V vn), a)
                                                  ) 
      
      flatl es vtab vtytab1 rtab letBase

    | If (e1, e2, e3) -> 
    (* We assume both branches of the if return the same type, so only check the type of e2 *)
      let tp = getTypeExp e2 vtytab rtab
      let t = newVar "if"
      let vtytab1 = bind t tp vtytab

      flat e1 vtab vtytab1 rtab (fun v ->
        let a2 = flat e2 vtab vtytab rtab k
        let a3 = flat e3 vtab vtytab rtab k
        let ifComp = IfA (v, a2, a3)
        match tp with 
        | Int -> LetInt (t, ifComp, k (V t))
        | Array _ -> LetArr (t, ifComp, k (V t))
        )

(* accumulator function to flatten a list of expression *)
and flatl (es : TypedExp list) (vtab : VarTableA) (vtytab : VarTypeTable) (rtab : RetTable) (acc : AVal list -> ANorm): ANorm =
    match es with 
    | [] -> acc []
    | e::rst ->
      flat e vtab vtytab rtab (fun v ->
          flatl rst vtab vtytab rtab (fun vs ->
              acc (v::vs)))

let anf (e : TypedExp) =
    flat e (empty()) (empty()) (empty()) (fun v -> ValueA v)

(* Takes an AVal, returns a string option, Some for var, None for int *)
let getVar (v : AVal) : string option =
    match v with 
    | V var -> Some var
    | _ -> None

(* Function for returning all the variable names of a computation *)
let getVarsComp (c : AComp) : LiveSet =
    match c with
    | ApplyA (_,vals) -> vals 
    | AddA (v1,v2) -> [v1;v2]
    | ArrLitA(vals,_) -> vals
    | IndexA (arr,i,_) -> [arr;i]
    | LenA (arr,_) -> [arr]
    | IfA (g,_,_) -> [g]
    | ArgA (v) -> [v]
    |> List.choose getVar 
    |> Set.ofList

(* Analyze A-normal form and insert drops when arrays are no longer used 
   Bottom up approach, use a liveset to keep track of live variables *)
let rec analyse (a : ANorm) (live : LiveSet) : ANorm * LiveSet =
    match a with
    | ValueA (N i) -> a, live
    | ValueA (V x) -> a, live |> Set.add x
    | LetInt (id, c, body) -> 
      (* Since Int, no need to worry about freeing id
         Get the liveSet from the body of the let, if the variables from the let's computation 
         dont exist in the body's liveset, we can drop them*)
      let body1, liveIn = analyse body live
      let compLive = getVarsComp c (* TODO: Figure out how to only get array variables here *)
      (* The variables who was last used in the computation (Not present in the body) *)
      let dead = Set.difference compLive liveIn
      (* Insert a drop for each of these variables *)
      let dropbody = dead |> Set.fold (fun a v -> DropA (v,a)) body1
      (* return the new body with drops, and the body's live variable + comp's variables *)
      LetInt (id, c, dropbody), Set.union liveIn compLive //|> Set.add id - skal jeg tilføje id???
    | LetArr (id, c, body) ->
      (* Since array, we want to drop this if its never used *)
      let body1, liveIn = analyse body live
      (* Also add id here, to see if we ever use it *)
      let compLive = getVarsComp c |> Set.add id
      let dead = Set.difference compLive liveIn
      let dropbody = dead |> Set.fold (fun a v -> DropA (v,a)) body1
      LetInt (id, c, dropbody), Set.union liveIn compLive 
    | DropA (_,_) -> a, live

(* Make a function into anf, for testing purposes *)
let anfFun (fn : FunDec) (rtab : RetTable): string * ANorm =
    let (FunDec (fname, tp, args, exp)) = fn
    (* Get function label *)
    let funLab = getFname fname

    (* Make sure each parameter exists in a vtable before flattening the expression to a normal form *)
    let (_, anfVtab, anfVtytab) =
      args |> List.fold (fun (i, vtab, vtytab) (Param(arg,t)) ->
          (i+1, bind arg (V arg) vtab, bind arg t vtytab))
          (0, empty(), empty())
    let anf = flat exp anfVtab anfVtytab rtab (fun v -> ValueA v)

    fname, analyse anf Set.empty |> fst

(* Make program into anf, for testing purposes *)
let anfProg (prog : Prog) : (string * ANorm) list =
    let rtab = getRetTab prog
    counter <- 1
    List.map (fun fn -> anfFun fn rtab) prog 


(* Generate Risc-v code from A normal form, split into three for the different types *)
let rec genVal (v : AVal) (vtab : VarTableC) (place : reg) : PseudoRV list =
    match v with 
    | N n -> [ LI (place, n)]
    | V var -> match lookup var vtab with
                | Some r -> [ MV (place, r) ]
                | None -> failwith (sprintf "Unkown variable %s" var)
and genComp (c : AComp) (vtab : VarTableC) (place : reg) : PseudoRV list =
    match c with 
    | AddA (v1, v2) -> 
      let t1 = newReg "plus_L"
      let t2 = newReg "plus_R"
      let code1 = genVal v1 vtab t1
      let code2 = genVal v2 vtab t2
      code1 @ code2 @ [ADD (place,t1,t2)]

    | ArrLitA (vs, tp) ->
      let sizeReg = newReg "size"
      let addrReg = newReg "addr"
      let tmpReg  = newReg "tmp"

      (* Store size in size_reg, allocate, put addres of first elm in addr reg *)
      let header = [ LI (sizeReg, List.length vs)
                  ; ALLOC (place, sizeReg)
                  ; ADDI (addrReg, place, 1) ]
      
      (* Compile each expression from list, store it in addrReg addres, increment addrReg*)
      let genElm v =
          let elmCode = genVal v vtab tmpReg
          elmCode @
          [ SW (tmpReg, addrReg, 0)
          ; ADDI (addrReg, addrReg, 1) ]
      
      let elemsCode = List.collect genElm vs
      header @ elemsCode

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
      (* Generate each argument into its own temporary register, then move them all to the argument registers
         We use temporary registers as well to avoid overwriting when using recursive functions*)
      let argsCode = args
                    |> List.mapi (fun i a -> 
                          let t = newReg "argTemp"
                          (genVal a vtab t) @ [MV (getArgReg i id, t)]
                          ) 
                    |> List.concat
      argsCode @ [CALL id; MV(place, Rret)]

    | IfA (v, a1, a2) ->
      let thenLabel = newLab "then"
      let elseLabel = newLab "else"
      let endLabel = newLab "endif"
      let cond = newReg "cond"
      let code1 = genVal v vtab cond
      let code2 = gen a1 vtab place
      let code3 = gen a2 vtab place
      code1 @ [BEQ (cond, R0, elseLabel); LABEL thenLabel] @ code2  @
        [ J endLabel; LABEL elseLabel ] @
        code3 @ [LABEL endLabel]
    (* Maybe needs some tweaking, im unsure of the exact behavior that is expected here
        put into the argument caller saved registers in real RISC-V, here however we just move it over *)
    | ArgA (v) -> 
      let r = newReg "arg"
      let code = genVal v vtab r
      code @ [MV (place, r)]

and gen (a : ANorm) (vtab : VarTableC) (place : reg) : PseudoRV list =
    match a with
    | ValueA v -> genVal v vtab place
    (* Do the same for both lets *)
    | LetInt (id, c, a)
    | LetArr (id, c, a) -> 
      let t = newReg ("let_" + id)
      (* Gen c and put result in t *)
      let code1 = genComp c vtab t
      let vtab1 = bind id t vtab
      (* Gen a with new vtable *)
      let code2 = gen a vtab1 place
      code1 @ code2

    (* Not implemented *)
    | DropA (id, a) -> [LI (place, 0)]   

(* Bind each parameter in a vtable and return parameters
   also create function prologue which makes local variables to be used in the function to ensure recursion works *)
let rec genArgs (args : Param list) 
                (vtab : VarTableC) 
                (argNum : int) 
                (argRegs : reg list)
                (fname: string)
              : (PseudoRV list * reg list *VarTableC) =
    match args with
    | [] -> ([], argRegs,vtab)
    | (Param (arg, _)::rst) ->
      let argReg = getArgReg argNum fname
      let localReg = newReg ("local_"+fname)
      let vtab1 = bind arg localReg vtab
      let (code2,regs,vtab2) = genArgs rst vtab1 (argNum + 1) (argRegs@[argReg]) fname
      [MV (localReg, argReg)] @ code2, regs,vtab2


let genFun (fn : FunDec) (rtab : RetTable): (string * (reg list * PseudoRV list)) =
    let (FunDec (fname, tp, args, exp)) = fn
    (* Get function label *)
    let funLab = getFname fname

    (* Make sure each parameter exists in a vtable before flattening the expression to a normal form *)
    let (_, anfVtab, anfVtytab) =
      args |> List.fold (fun (i, vtab, vtytab) (Param(arg,t)) ->
          (i+1, bind arg (V arg) vtab, bind arg t vtytab))
          (0, empty(), empty())

    let aN = flat exp anfVtab anfVtytab rtab (fun v -> ValueA v)
    //printfn "%A" aN

    (* Bind each formal argument to the symbolic argument registers 
       We use a list.fold to accumulate the vtable along with a counter 
       for generation of argument registers *)
    let (prologCode, argRegs,vtab) = genArgs args (empty()) 0 [] fname
    (* flatten and generate expression using argument registers and place in return register *)
    let fCode = gen aN vtab Rret
    fname, (argRegs, [LABEL funLab] @ prologCode @ fCode @ [RET])

(* Each function returns an element of a map, a function name and argument registers with the function code *)
let genProg (prog : Prog) : RVProg =
    let rtab = getRetTab prog
    counter <- 1
    List.map (fun fn -> genFun fn rtab) prog 
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