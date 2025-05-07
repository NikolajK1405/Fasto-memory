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
  | LenA of AVal
  | IfA of AVal * ANorm * ANorm

and ANorm =
  | ValueA of AVal
  | LetA of string * Type * AComp * ANorm
  | IncA of string * ANorm
  | DecA of string * ANorm

type AFunDec = AFunDec of string * Type * Param list * ANorm
type AProg = AFunDec list

(* Set of live variables, used to analyze when we can add drops *)
type LiveSet = Set<string>
(* Takes an AVal, returns a string option, Some for var, None for int *)
let getVar (v : AVal) : string option =
    match v with 
    | V var -> Some var
    | _ -> None

(* Returns all used variables in a ANorm, 
   since we get live sets in analyse its not used much, just for if-else statements
   Muligvis useless.*)
let rec getVarsANorm (a : ANorm) : LiveSet =
    match a with
    | ValueA v -> 
      match getVar v with
      | Some var -> Set.empty |> Set.add var
      | None -> Set.empty
    | LetA (_,_, c, body) ->
      let cVars = getVarsComp c
      let bVars = getVarsANorm body
      Set.union cVars bVars
    | IncA (_,_) -> Set.empty
    | DecA (_,_) -> Set.empty

(* Function for returning all the variable names of a computation *)
and getVarsComp (c : AComp) : LiveSet =
    match c with
    | IfA (g,b1,b2) -> [g]// Maybe no need for all this since they are given along as liveset in analyse.
      //(Set.union (getVarsANorm b1) (getVarsANorm b2) |> Set.toList |> List.map (fun v -> V v)) @ [g]
    | ApplyA (_,vals) -> vals 
    | AddA (v1,v2) -> [v1;v2]
    | ArrLitA(vals,_) -> vals
    | IndexA (arr,i,_) -> [arr;i]
    | LenA (arr) -> [arr]
    |> List.choose getVar 
    |> Set.ofList

(* Vartable map: SL id, (Anorm val, type)*)
type VarTableA = Map<string, AVal * Type>
let lookupVal (vn : string) (vtab : VarTableA) =
    match vtab |> Map.tryFind vn with
    | Some (v,_) -> v
    | None -> failwith (sprintf "Unkown variable in VarTableA: %s" vn)

let lookupType (vn : string) (vtab : VarTableA) =
    match vtab |> Map.tryFind vn with
    | Some (_,t) -> t
    | None -> failwith (sprintf "Unkown variable in VarTableA: %s" vn)

let bindVar (vn : string) (vt : AVal * Type) (vtab : VarTableA) =
    vtab |> Map.add vn vt

(* We create a function table to be used in flat, in order to get typed lets for the case of Apply
   As well as seeing what arguments are arrays, to be used in analyse *)
type FTTable = Map<string, Type * Type list>
let lookupFRet (fn : string) (ftab : FTTable) =
    match ftab |> Map.tryFind fn with
    | Some (t,_) -> t
    | None -> failwith (sprintf "Unkown function in FTable: %s" fn)
let lookupFParamTypes (fn : string) (ftab : FTTable) =
    match ftab |> Map.tryFind fn with
    | Some (_,ts) -> ts
    | None -> failwith (sprintf "Unkown function in FTable: %s" fn)
let getRetTab (prog:Prog) : FTTable =
    prog |> List.fold (fun tab (FunDec(fid, rt, parms, _)) ->
              let parmTps = parms |> List.map (fun (Param (_,t)) -> t) 
              Map.add fid (rt,parmTps) tab) Map.empty

(* Helper function to get type of an expression, 
   minimal since we can use the actual type checker of fasto when moving the implementation *)
let rec getTypeExp (e : TypedExp) (vtab : VarTableA) (rtab : FTTable) : Type =
    match e with
    | Constant v -> valueType v
    | Var v -> 
      lookupType v vtab
    | ArrayLit (_, tp) -> tp
    | Plus (_,_) ->  Int
    | Let (x,e1,e2) ->
      (* Just input an arbitrary AVal to the vtab, we jsut need types here, 
         and since this vtab will not be returned its fine*)
      let vtab1 = bindVar x (V x, getTypeExp e1 vtab rtab) vtab
      getTypeExp e2 vtab1 rtab
    | Index (_,_,tp) -> tp
    | Length (_) -> Int
    (* We assume bot branches of if return the same type*)
    | If (_,e1,_) -> getTypeExp e1 vtab rtab
    | Apply (f,_) -> lookupFRet f rtab

(* Flattes a function body expression to a-normal form and analyses it to include reference counting. 
   Arguments: 
   * a function body expression 
   * a SL variable table that already has the parameters of the function
   * a function table including all function from the program
   Contains local functions: flat, flatl, analyse *)
let flatAnalyse (exp : TypedExp) (varTab : VarTableA) (ftab : FTTable) =
    (* When we create an anfLet in flat, add the new variable to this Var tab along with its type
       To be used in analyse, so we only incr and decr arrays
       Start by copying each of the function arguments from varTab in *)
    let mutable anfVtab : SymTab<Type> = varTab |> Map.fold (fun tab id (_,t) -> bind id t tab) (empty()) 
    (* K is continuation function *)
    let rec flat (e : TypedExp) (vtab : VarTableA) (k : AVal -> ANorm): ANorm =
        match e with
        | Constant (IntVal n) ->
          k (N n)

        | Constant (ArrayVal (vs, tp)) ->
          (* Turn into arraylit to reuse code *)
          let arraylit = ArrayLit(List.map (fun v -> Constant v) vs, tp)
          flat arraylit vtab k

        | Var id ->
          lookupVal id vtab |> k

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
              anfVtab <- bind tArr (Array tp) anfVtab
              LetA (tArr, Array tp, comp, k (V tArr))

          flatl elems vtab letBase

        | Plus(e1, e2) ->
          (* we use the continuation function to recursivly compile each expression 
            and put it together in a let in the end 
            This is the only place we need to update the variable type table *)
          flat e1 vtab (fun l ->
              flat e2 vtab (fun r ->
                  let t = newVar "t" 
                  anfVtab <- bind t Int anfVtab
                  LetA (t, Int, AddA (l, r), k (V t))))

        | Let(id, e1, e2) ->
          (* call recursivly on e2 with the value from e1, no actual
            LetA types to ensure copy and constant propagation *)
          flat e1 vtab (fun v1 ->
              let idtp = getTypeExp e1 vtab ftab
              let vtab1 = bindVar id (v1,idtp) vtab
              flat e2 vtab1 (fun v2 -> k v2))

        | Index(id, e, tp) -> 
          let arr = lookupVal id vtab

          flat e vtab (fun i ->
              let t = newVar "i"
              anfVtab <- bind t tp anfVtab
              LetA (t, tp, IndexA(arr, i, tp), k (V t)))

        | Length(e) ->
          flat e vtab (fun arr ->
              let t = newVar "len"
              anfVtab <- bind t Int anfVtab
              LetA (t, Int, LenA arr, k (V t)))

        | Apply (f, es) -> 
          (* Simmilar approach as array lit, use flatl accumulator function to flatten each element
              make a base function for the final step *)
          let tp = lookupFRet f ftab
          let letBase (vs : AVal list) =
            let t = newVar "fRes"
            let applComp = ApplyA (f, vs)
            anfVtab <- bind t tp anfVtab
            LetA (t, tp, applComp, k (V t))
          
          flatl es vtab letBase

        | If (e1, e2, e3) -> 
        (* We assume both branches of the if return the same type, so only check the type of e2 *)
          flat e1 vtab (fun v ->
            let tp = getTypeExp e2 vtab ftab
            (* Evaluate each branch with its own k function, stopping when we get a value *)
            let a2 = flat e2 vtab (fun v -> ValueA v)
            let a3 = flat e3 vtab (fun v -> ValueA v)
            let ifComp = IfA (v, a2, a3)
            let t = newVar "if"
            anfVtab <- bind t tp anfVtab
            LetA (t, tp, ifComp, k (V t))
            )

    (* accumulator function to flatten a list of expression *)
    and flatl (es : TypedExp list) (vtab : VarTableA) (acc : AVal list -> ANorm): ANorm =
        match es with 
        | [] -> acc []
        | e::rst ->
          flat e vtab (fun v ->
              flatl rst vtab  (fun vs ->
                  acc (v::vs)))

    (* Analyze A-normal form and insert drops when arrays are no longer used 
      Bottom up approach, use a liveset to keep track of live variables *)
    let rec analyse (a : ANorm) (live : LiveSet) : ANorm * LiveSet =
        match a with
        | ValueA (N i) -> a, live
        | ValueA (V x) -> 
          (* Only add arrays as live variables *)
          match lookup x anfVtab with
          | Some tp -> if tp <> Int then a, live |> Set.add x else a, live
          | None -> failwith (sprintf "Unkown variable in anfvtab in analyse: %s" x)
        | LetA (id, tp, c, body) -> 
          (* Get the liveSet from the body of the let, if the variables from the let's computation 
            dont exist in the body's liveset, we can drop them*)
          let body1, liveIn = analyse body live
          (* Get the variables used in computations of the let
            If the type is an array, we also want to check if the id is used, so we can free it otherwise *)
          let compLive = 
            if tp <> Int 
              then getVarsComp c |> Set.add id
              else getVarsComp c
            (* Sort out any variables that are not arrays *)
            |> Set.filter (fun x -> 
                            match lookup x anfVtab with
                            | Some tp -> tp <> Int
                            | None -> failwith (sprintf "Unkown variable in anfvtab in analyse: %s" x))
          (* The variables who was last used in the computation (Not present in the body) *)
          let dead = Set.difference compLive liveIn
          (* Insert a drop for each of these variables *)
          let decpbody = dead |> Set.fold (fun a v -> DecA (v,a)) body1

          (* the new let with drops, and the body's live variable + comp's variables *)
          let Let = LetA (id, tp, c, decpbody) 
          (* All live variables in this let's comp + the ones in the body *)
          let liveAfter = Set.union liveIn compLive

          (* In anf form, function calls is the only term that can copy pointers, 
              and such is the place we need to insert increment statements*)
          match c with
          | ApplyA (_,vals) ->
            (* List containing each variable that is used as function argument, may include duplicates *)
            let args = vals |> List.choose getVar 
            (* Insert an incr statement for each function argument, before the let
              If same argument is used more times we insert multiple increment, as there will be multiple copies*)
            let incrLet = args |> List.fold (fun A v -> IncA (v, A)) Let
            incrLet, liveAfter
          | IfA (g,a1,a2) ->
            (* Analyse each branch, and get the used variables*)
            let b1body, b1Live = analyse a1 live // x, y
            let b2body, b2Live = analyse a2 live // z, x

            (* Variables used last time in each branch *)
            let b1Last = Set.difference b1Live liveIn
            let b2Last = Set.difference b2Live liveIn

            (* Last variables only used in the other branch, 
               these need to be decremented immediately in the other branch *)
            let b1dead = Set.difference b2Last b1Last // z
            let b2dead = Set.difference b1Last b2Last // y

            let b1dec = b1dead |> Set.fold (fun a v -> DecA (v,a)) b1body
            let b2dec = b2dead |> Set.fold (fun a v -> DecA (v,a)) b2body

            let decIf = IfA (g, b1dec, b2dec)
            let liveAfterIf = liveAfter |> Set.union b1Live |> Set.union b2Live
            LetA (id, tp, decIf, decpbody), liveAfterIf
          | _ ->
            Let, liveAfter
        | IncA (_,_) -> a, live
        | DecA (_,_) -> a, live
    
    (* Flatten expression to anf*)
    let anf = flat exp varTab (fun v -> ValueA v)

    (* Analyse *)
    let anf1, live = analyse anf Set.empty

    (* Check if function arguments of array type are used in the body, otherwise decrement them
    Necesarry since analyse only decrements variables that are used in computations or assigned in a let *)

    (* Convert the Vartab to a list of arguments, also filter out non-array vars, 
       as flatAnalyse contract states, Vartab should only contain the function argument variables*)
    let args = varTab |> Map.fold (fun lst id (_,t) -> if t <> Int then id::lst else lst) []
    (* Get the set of all the unused variables *)
    let dead = Set.difference (args |> Set.ofList) live
    (* Insert a decr for each dead var *)
    let anf2 = dead |> Set.fold (fun a v -> DecA (v,a)) anf1
    anf2 

(* Make a FunDec into anf (AFunDec) *)
let anfFun (fn : FunDec) (ftab : FTTable): AFunDec =
    let (FunDec (fname, tp, args, exp)) = fn

    (* Make sure each function parameter exists in a vtable before flattening the expression to a normal form *)
    let argVtab =
      args |> List.fold (fun vtab (Param(arg,t)) ->
              (bindVar arg (V arg, t) vtab)) Map.empty
    
    (* Flatten to anf, and analyse *)
    let anf = flatAnalyse exp argVtab ftab

    (* fun name * arg names * analyzed anf body *)
    AFunDec (fname, tp, args, anf) 

(* Make program into AProg *)
let anfProg (prog : Prog) : AProg =
    let ftab = getRetTab prog
    counter <- 1
    List.map (fun fn -> anfFun fn ftab) prog 


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

    | LenA (arr) -> 
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

and gen (a : ANorm) (vtab : VarTableC) (place : reg) : PseudoRV list =
    match a with
    | ValueA v -> genVal v vtab place
    (* Do the same for both let types *)
    | LetA (id, tp, c, a) -> 
      let t = newReg ("let_" + id)
      (* Gen c and put result in t *)
      let code1 = genComp c vtab t
      let vtab1 = bind id t vtab
      (* Gen a with new vtable *)
      let code2 = gen a vtab1 place
      code1 @ code2

    (* Not implemented *)
    | IncA (id, a) -> 
      let code = gen a vtab place
      code
    | DecA (id, a) ->
      let code = gen a vtab place
      code

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

let genFun (fn : AFunDec) : (string * (reg list * PseudoRV list)) =
    let (AFunDec (fname, tp, args, A)) = fn
    (* Get function label *)
    let funLab = getFname fname

    (* Bind each formal argument to the symbolic argument registers 
       We use a list.fold to accumulate the vtable along with a counter 
       for generation of argument registers *)
    let (prologCode, argRegs,vtab) = genArgs args (empty()) 0 [] fname
    (* flatten and generate expression using argument registers and place in return register *)
    let fCode = gen A vtab Rret
    fname, (argRegs, [LABEL funLab] @ prologCode @ fCode @ [RET])

(* Each function returns an element of a map, a function name and argument registers with the function code *)
let genProg (prog : AProg) : RVProg =
    counter <- 1
    List.map (fun fn -> genFun fn) prog 
    |> Map.ofList

(* Take SL prog, flatten to anf, analyze and genereate risc-v prog*)
let flatGen (prog : Prog) : RVProg =
    prog |> anfProg |> genProg
let flatGenSimulate (prog : Prog) : (int * Heap) =
    prog |> flatGen |> simulate

let runTestA (prog : Prog) (pname : string) =
    let (resC, _) = flatGenSimulate prog
    let resI = match evalProg prog with
               | IntVal v -> v
               | ArrayVal (_,_) -> 0 (* TODO, implement array testing*)
    if resC = resI then
         printfn "Sucess! Test: %s, result: %i" pname resC
    else printfn "Fail... Test: %s, expected %i, got %i" pname resI resC
    

(* Turns an expression into RVProg, for testing small expressions with no functions *)
(*
let anfe (e : TypedExp) =
    flat e (Map.empty) Map.empty (fun v -> ValueA v)
let flatGene (e : TypedExp) : RVProg =
    let a = anfe e
    let rv = gen a (empty()) Rret
    Map [("main", ([], [LABEL "f.main"]@rv@[RET]))]
*)