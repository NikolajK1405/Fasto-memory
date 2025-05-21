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
  | MapA of Param * ANorm * AVal * Type * Type (* Same meanings as the miniFasto absyn for map *)

and ANorm =
  | ValueA of AVal
  | LetA of string * Type * AComp * ANorm
  | IncA of string  * ANorm
  | DecA of string * int * ANorm (* the int represents the dimensions of array we are decrementing*)

type AFunDec = AFunDec of string * Type * Param list * ANorm
type AProg = AFunDec list

(* Check if a computation may contain IO (side effects). 
   Computations with side effects can not be removed in dead binding elimination*)
let cIO (c : AComp) : bool =
  match c with
  | ApplyA(_,_) | IfA(_,_,_) | MapA(_,_,_,_,_) -> true
  | _ -> false


(* Set of live variables, used to analyze when we can add drops *)
type LiveSet = Set<string>
(* Available variables now and variables we need to keep alive for later,
   when entering this node. *)
type LiveSets = LiveSet * LiveSet

(* Takes an AVal, returns a string option, Some for var, None for int *)
let getVar (v : AVal) : string option =
    match v with 
    | V var -> Some var
    | _ -> None

(* Get the free variables of an anf branch, will only be used in loops such as map*)
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
    | _ -> Set.empty

(* Function for returning all the variable names of a computation *)
and getVarsComp (c : AComp) : LiveSet =
    (* First get all the free variables from any possible branches such as map or if-else *)
    match c with
    | IfA (_,b1,b2) -> Set.union (getVarsANorm b1) (getVarsANorm b2)
    | MapA (_,body,_,_,_) -> getVarsANorm body
    | _ -> Set.empty
    (* Then union with any vars used directly *)
    |> Set.union ( 
      match c with
      | IfA (g,_,_) -> [g]
      | ApplyA (_,vals) -> vals 
      | AddA (v1,v2) -> [v1;v2]
      | ArrLitA(vals,_) -> vals
      | IndexA (arr,i,_) -> [arr;i]
      | LenA (arr) -> [arr]
      | MapA (_,_,arr,_,_) -> [arr] 
      |> List.choose getVar 
      |> Set.ofList
    )


(* Go truough an anf and get all the variables on the left side of each Let node
   Used for if-else in analyse so we dont decrement variables only in the scope of one branch *)
let rec getDefs (a : ANorm) : LiveSet=
    match a with
    | ValueA _ -> Set.empty
    | LetA (id,_,_,body) -> getDefs body |> Set.add id
    | IncA (_,body) | DecA (_,_,body) -> getDefs body

(* Check if a variable name is an array, used in analyse to sort out non array variables *)
let isArray (v : string) (tab : SymTab<Type>) : bool =
    match lookup v tab with
    | Some tp -> tp <> Int
    | None -> failwith (sprintf "Unkown variable in anfvtab in analyse: %s" v)

(* Get the dimensions of an array type *)
let rec getDims (t : Type) : int =
    match t with 
    | Int -> 0
    | Array tp -> 1 + getDims tp

(* Insert decrements for each array variable in the set, around an anf node *)
let insertDecs (dead : LiveSet) (anfVtab : SymTab<Type>) (body : ANorm) : ANorm =
    dead
    |> Set.fold (fun a v ->
        let dims = match lookup v anfVtab with
                    | Some tp -> getDims tp
                    | None -> failwith (sprintf "Unkown variable in anfvtab in analyse: %s" v)
        if dims = 0 then a (* Dont decrement integer variables *)
        else DecA (v,dims,a)) body

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
    | ArrayLit (_, tp) -> Array tp
    | Plus (_,_) ->  Int
    | Let (x,e1,e2) ->
      (* Just input an arbitrary AVal to the vtab, we just need types here, 
         and since this vtab will not be returned its fine*)
      let vtab1 = bindVar x (V x, getTypeExp e1 vtab rtab) vtab
      getTypeExp e2 vtab1 rtab
    | Index (_,_,tp) -> tp
    | Length (_) -> Int
    (* We assume bot branches of if return the same type*)
    | If (_,e1,_) -> getTypeExp e1 vtab rtab
    | Apply (f,_) -> lookupFRet f rtab
    | Map (_,_,_,_,tp) -> tp

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
          flat e1 vtab (fun v ->
            (* If the guard is an integer we can remove the branch *)
            match v with
            | N 0 ->
              flat e3 vtab k
            | N _ -> 
              flat e2 vtab k
            | V _ ->
            (* Evaluate each branch with its own k function, stopping when we get a value *)
              let a2 = flat e2 vtab (fun v -> ValueA v)
              let a3 = flat e3 vtab (fun v -> ValueA v)
              let ifComp = IfA (v, a2, a3)
            (* We assume both branches of the if return the same type, so only check the type of e2 *)
              let t = newVar "if"
              let tp = getTypeExp e2 vtab ftab
              anfVtab <- bind t tp anfVtab
              LetA (t, tp, ifComp, k (V t))
            )
            
        | Map(Param(arg,argtp), fbody, arre, arInT, arOutT) ->
          flat arre vtab (fun arr ->
            //Det er arInt's element type vi skal tjekke...
            //if argtp <> arInT then failwith "Type mismatch in map function argument"
            
            (* Create a new name for the lambda parameter, and add it to the anfvtab *)
            let arg1 = newVar "mapx"
            anfVtab <- bind arg1 argtp anfVtab

            (* Bind the argument of the anonymus function in a new vtab and flatten function body *)
            let vtab1 = bindVar arg (V arg1,argtp) vtab
            let fbodya = flat fbody vtab1 (fun v -> ValueA v)

            (* Wrap the map in a let with a new var *)
            let t = newVar "map"
            anfVtab <- bind t arOutT anfVtab

            let mapComp = MapA (Param(arg1,argtp), fbodya, arr, arInT, arOutT)
            LetA(t, arOutT, mapComp, k (V t))
            )

    (* accumulator function to flatten a list of expression *)
    and flatl (es : TypedExp list) (vtab : VarTableA) (acc : AVal list -> ANorm): ANorm =
        match es with 
        | [] -> acc []
        | e::rst ->
          flat e vtab (fun v ->
              flatl rst vtab  (fun vs ->
                  acc (v::vs)))

(* Analyse an anf, inserting inc and dec*)
    let rec analyse (a : ANorm) (liveOut : LiveSet) (useFlag : bool) : ANorm * LiveSet =
      match a with 
      | ValueA (N _) -> a, liveOut
      | ValueA (V x) -> 
        let isLiveAfter = liveOut.Contains x
        (* We only add x to the liveset if its live after, an array and the result is used *)
        match lookup x anfVtab with
        | Some tp ->  
          match tp <> Int, isLiveAfter, useFlag with
          (* The array is copied as the result of the branch, and the array is still live, so we need to increment*)
          | true,true,true -> IncA (x,a), liveOut
          (* Live after but not copied. Already present in the liveset, no change needed *)
          | true,true,false -> a, liveOut
          (* Dead after, but is copied, so we have to keep it live. Add to liveset *)
          | true,false,true -> a, liveOut.Add x
          (* Not copied and also dead after, value is never used so can just replace with zero *)
          | true,false,false -> ValueA(N 0), liveOut
          (* Not an array, but we still add to liveset in order to ensure we dont remove bindings that create this *)
          | _ -> a, liveOut.Add x
        | None -> failwith (sprintf "Unkown variable in anfvtab in analyse: %s" x)
      | LetA (id, tp, c, body) ->
          (* Get the liveSet from the body of the let, if the variables from the let's computation 
            dont exist in the body's liveset, they are used for the last time here and we can drop them*)
          let body1, liveAfter = analyse body liveOut useFlag
          (* Check if the assigned variable is used, otherwise we can remove this node. Dead binding elimination *)
          let deadBinding = not (liveAfter.Contains id)
          (* If we have a dead binding and our computation is sure to have no side effects, 
             we can remove this node form the tree*)
          if deadBinding && not (cIO c) then
            body1, liveAfter
          else
          (* Get vars used in the computation *)
          let compLive = getVarsComp c
          (* Dead variables are the ones used in this node, but not live after*)
          let dead = Set.difference compLive liveAfter

          (* Insert a dec for each of these variables *)
          let decbody = insertDecs dead anfVtab body1
          (* the new let with drops, and the body's live variable + comp's variables *)
          let Let = LetA (id, tp, c, decbody) 
          (* All live variables in this let's comp + the ones in the body *)
          let liveBefore = Set.union liveAfter compLive
          (* If the computation is NOT an if or apply, and we have a dead binding, remove this let *)
          match c with
          | IfA (g,b1,b2) -> 
            (* Call recursivly on each branch *)
            let b1body, b1Live = analyse b1 liveAfter (not deadBinding) // Give the flag depending on weather id is used after
            let b2body, b2Live = analyse b2 liveAfter (not deadBinding)
            (* filter out all variables that are defined in each branch from the liveset, 
               we should not decrement these in the other branch as they are out of scope there*)
            let b1LiveUsed = getDefs b1 |> Set.difference b1Live
            let b2LiveUsed = getDefs b2 |> Set.difference b2Live
            (* Variables only live in one branch has to die, filter out variabels that are not arrays *)
            let b1dead = Set.difference b2LiveUsed b1Live |> Set.filter (fun x -> isArray x anfVtab)
            let b2dead = Set.difference b1LiveUsed b2Live |> Set.filter (fun x -> isArray x anfVtab)

            let b1dec = insertDecs b1dead anfVtab b1body
            let b2dec = insertDecs b2dead anfVtab b2body
            let decIf = IfA (g, b1dec, b2dec)

            (* Get the guard variable in a set, may be empty if we have an int*)
            let guardSet = [g] |> List.choose getVar |> Set.ofList
            (* Check if the guard is dead, if so insert a dec *)
            let decBodyGuard = Set.difference guardSet liveAfter |> fun gSet -> insertDecs gSet anfVtab body1 
            (* Also add it to the liveset*)
            let liveBeforeIf = Set.union b1Live b2Live |> Set.union guardSet

            LetA (id, tp, decIf, decBodyGuard), liveBeforeIf

      (* Insert increments around an anf node containing a function call,
        for each argument used n times, increment it n-1 times + 1 more if its still live after *)
          | ApplyA (_,args) 
          | ArrLitA (args,_) -> (* We treat ArrayLit {a,b,c} as a function mkrArr(a,b,c)*)
            let incrLet =
              args
              (* Filter out all non variables and convert these AVals to strings*)
              |> List.choose getVar 
              (*Create a map that keeps track of how many times each of these variables are used as function arguments*)
              |> List.fold (fun map arg -> 
                  map |> Map.change arg (function
                    | Some n -> Some (n+1)
                    | None -> 
                      (* We only care about array variables *)
                      if isArray arg anfVtab
                      then Some 1 else None)) Map.empty

              (* If the argument is dead after, decrement the count *)
              |> Map.map (fun arg n -> if dead.Contains arg then n-1 else n) 
              (* Turn this back into a list, with duplicates corresponding to the count*)
              |> Map.fold (fun lst arg n -> lst @ List.replicate n arg) []
              (* Add increments *)
              |> List.fold (fun A v -> 
                  match lookup v anfVtab with 
                  | Some t -> IncA (v, A)
                  | None -> failwith (sprintf "Unkown variable in anfvtab in analyse: %s" v) ) 
                  (LetA (id, tp, c, body1))
            incrLet, liveBefore

          (* For index we might need to increment if we are accessing an element in a 2d array*)
          | IndexA(_,_,t) when t<>Int ->
            (* Increment the new variable that was assigned in the let, 
                as this contains the copied pointer to the array*)
            let incrBody = IncA(id, decbody)
            LetA (id, tp, c, incrBody), liveBefore
          | MapA(Param(arg,argtp), fbody, arr, arInT, arOutT) ->
            (* Gather all variables used in the body of the lambda function, also add the array its applied to *)
            let allVars = Set.union (getVarsANorm fbody) (getVar arr |> Option.toList |> Set.ofList)
            (* Get the local variables, 
               these are the only ones we want to decrement in every iteration of the loop*)
            let localVars = getDefs fbody
            (* allVars - localVars = the variables from the outer scope, 
               the lambda function's parameter and the map's paramater will also be here, which is intentional so it dosent get decremented *)
            let outerVars = Set.difference allVars localVars 
            (* Analyse the body, but add the outerVars to the liveAfter set to ensure they dont get decremented
               We also give a true useFlag as any array pointers might be copied to a new array *)
            let fbody1, mapLive = analyse fbody (Set.union liveAfter outerVars) true 
            (* After the map, we want to decrement any of the outervars that was used last time in the lambda func 
               We also remove the formal parameter so it dosent get decremented *)
            let deadMap = Set.difference outerVars liveAfter |> Set.remove arg
            (* Insert a dec for each of these variables *)
            let decbodyMap = insertDecs deadMap anfVtab body1
            let mapComp = MapA(Param(arg,argtp), fbody1, arr, arInT, arOutT)
            LetA(id, tp, mapComp, decbodyMap), liveBefore

          | _ -> Let, liveBefore
      | DecA (_,_,_) -> failwith "Decrement already present in anf in analyse"
      | IncA (_,_) -> failwith "Increment already present in anf in analyse"

    (* Flatten expression to anf*)
    let anf = flat exp varTab (fun v -> ValueA v)

    (* Analyse *)
    let anf1, live = analyse anf Set.empty true

    (* Convert the Vartab to a list of arguments, also filter out non-array vars, 
       as the flatAnalyse contract states, Vartab should only contain the function argument variables*)
    let args = varTab |> Map.fold (fun lst id (_,t) -> if t <> Int then id::lst else lst) []
    (* Get the set of all the unused variables *)
    let dead = Set.difference (args |> Set.ofList) live
    (* Insert a decr for each dead var *)
    let anf2 = insertDecs dead anfVtab anf1
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
    | MapA (Param(arg,argtp), fbody, arr, arInT, arOutT) ->
      let sizeReg = newReg "size" (* size of input/output array *)
      let arrReg  = newReg "arr"  (* address of array *)
      let elm1Reg = newReg "elm1" (* address of current element in input array *)
      let elm2Reg = newReg "elm2" (* address of current element in output array*) 
      let iReg    = newReg "i" 
      let resReg  = newReg "res"

      let arrCode   = genVal arr vtab arrReg  (* Place pointer of input array in arrReg *)
      let sizeCode  = [ LW (sizeReg, arrReg, 0) ] (* Load the size into sizeReg *)
      let allocCode = [ ALLOC (place, sizeReg) ]  (* Allocate the new array, put pointer in place *)
      (* Put addresses into each of our counter regs, and 0 in i reg *)
      let initCode  = [ ADDI (elm2Reg, place, 1)   
                      ; ADDI (elm1Reg, arrReg, 1)
                      ; MV (iReg, R0 )
                      ]
      let loopBeg = newLab "loop_beg"
      let loopEnd = newLab "loop_end"
      let loopHeader = [ LABEL loopBeg
                        ; BEQ (iReg, sizeReg, loopEnd) (* We dont have BGE in miniFasto, so we just use BEQ *)
                        ]
      let loadElm = [ LW (resReg, elm1Reg, 0) (* Load arrIn[i] into resreg *)
                    ; ADDI (elm1Reg, elm1Reg, 1)
                    ]
      (* Inline the lambda function, add the formal parameter to the vtab *)
      let vtab1  = bind arg resReg vtab
      let bodyCode  = gen fbody vtab1 resReg

      let storeRes = [ SW (resReg, elm2Reg, 0)
                     ; ADDI (elm2Reg, elm2Reg, 1) ]
      
      let loopFooter = [ ADDI (iReg, iReg, 1)
                        ; J loopBeg
                        ; LABEL loopEnd
                        ]
      arrCode 
        @ sizeCode
        @ allocCode
        @ initCode
        @ loopHeader
        @ loadElm
        @ bodyCode
        @ storeRes
        @ loopFooter


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

    | IncA (id, a) -> 
      let incCode = match lookup id vtab with
                    | Some r -> [ INC r ]
                    | None -> failwith (sprintf "Unkown variable to increment %s" id)
      let code = gen a vtab place
      incCode @ code
    | DecA (id, d, a) ->
      let decCode = match lookup id vtab with
                    | Some r -> [ DEC (r,d) ]
                    | None -> failwith (sprintf "Unkown variable to decrement %s" id)
      let code = gen a vtab place
      decCode @ code

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
let flatGenSimulate (prog : Prog) : (int * Heap * Type) =
    let (FunDec(_,tp,_,_)) = lookupFunDec prog "main" (* Get main function return type *)
    let rvProg = flatGen prog
    //printfn "%A" rvProg
    let res,heap = simulate rvProg
    res,heap,tp

(* Extract multi dimensional array from Value list *)
(*
let rec extractI (vs : Value list) (dims : int) =
    match dims with
    | 1 -> 
      vs |> List.map (function 
                      | IntVal i -> i
                      | _ -> failwith "Unexpected array element in 1D array")
    | n when n > 1 ->
      vs.Tail |> List.map (function
                            | ArrayVal (vals,_) -> extractI vals (dims-1)
                            | _ -> failwith "Int found in multidimensional array")
*)

let rec dimsToType (dims : int) : Type =
    match dims with
    | 1 -> Int
    | n when n > 1 -> Array (dimsToType (dims-1))
    | _ -> failwith "Invalid dimension argument in dimsToType"

(* Turn the output of the compiler into an ArrayVal, to be compared with interpretor res*)
let rec rebuildCRes (arrP : int) (hp : Heap) (dims : int) =
    match dims with
    | 1 -> 
      lookUpHeap arrP hp 
      |> fun (arr,_,_) -> arr 
      |> Array.toList 
      |> List.map (fun x -> IntVal x)
      |> fun vals -> ArrayVal (vals, Int)
    | n when n > 1 ->
      let arr =
        lookUpHeap arrP hp 
        |> fun (arr,_,_) -> arr 
        |> Array.toList 
      let size = arr |> List.head |> fun x -> IntVal x
  
      arr
      |> List.tail
      |> List.map (fun x -> rebuildCRes x hp (dims-1))
      |> fun vals -> ArrayVal (size::vals, dimsToType n)
    | _ -> failwith "Invalid dimension argument in rebuildCRes"

let rec compareVal (v1 : Value) (v2 : Value) =
    match v1, v2 with
    | IntVal i, IntVal j -> i = j
    | ArrayVal (a1, t1), ArrayVal (a2, t2) when t1 = t2 ->
      List.forall2 compareVal a1 a2
    | _ -> false

let rec countArrs (v : Value) : int =
    match v with
    | IntVal _ -> 0
    | ArrayVal (_, t) when t = Int -> 1
    | ArrayVal (vs, t) when t = Array Int -> 
      match vs.Head with
      | IntVal n -> n + 1
      | ArrayVal _ -> failwith (sprintf "Non integer in array header: %A" v)
    | ArrayVal (vs, _) -> vs.Tail |> List.fold (fun acc v -> acc + countArrs v) 1

let runTestA (prog : Prog) (pname : string) =
    let (resC, hp, tp) = flatGenSimulate prog
    (* Number of arrays left in the heap *)
    let hpCount = hp.Count
    (* We have an array*)
    if tp <> Int then
      let arrI =  match evalProg prog with
                  | IntVal v -> failwith "Type mismatch in return values, interpretor: int, compiler: array"
                  | ArrayVal (vs,t) -> ArrayVal (vs,t)
      let arrC = rebuildCRes resC hp (getDims tp)
      if compareVal arrI arrC then
        let resArrCount = countArrs arrC
        if resArrCount = hpCount then
          printfn "Sucess! Test: %s, result: %A, heap count: %i" pname arrC hpCount
        else printfn "Fail... Test: %s, expected heap count %i, got %i: %A" pname resArrCount hpCount arrC
      else printfn "Fail... Test: %s, expected res %A, got %A" pname arrI arrC
    (* We have an integer *)
    else 
      let resI =  match evalProg prog with
                  | IntVal v -> v
                  | ArrayVal _ -> failwith "Type mismatch in return values, interpretor: array, compiler: int"
      if resI = resC then
        if hpCount = 0 then
          printfn "Sucess! Test: %s, result: %A, heap count: %i" pname resC hpCount
        else printfn "Fail... Test: %s, expected heap count %i, got %i" pname 0 hpCount
      else printfn "Fail... Test: %s, expected res %A, got %A" pname resI resC
    