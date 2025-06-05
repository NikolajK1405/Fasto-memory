module ANorm
open MiniFasto

type AVal =
  | N of int
  | V of vname

type AComp = 
  | ApplyA of fname * AVal list
  | AddA of AVal * AVal
  | ArrLitA of AVal list * Type
  | IndexA of AVal * AVal * Type
  | LenA of AVal
  | IfA of AVal * ANorm * ANorm
  | MapA of Param * ANorm * AVal * Type * Type (* Same meanings as the miniFasto absyn for map *)

and ANorm =
  | ValueA of AVal
  | LetA of vname * Type * AComp * ANorm
  | IncA of vname  * ANorm
  | DecA of vname * int * ANorm (* the int represents the dimensions of array we are decrementing*)

type AFunDec = AFunDec of fname * Type * Param list * ANorm
type AProg = AFunDec list

(* ----------------------------------------------------------------------- *)

(* Flattener, flattens an expression into A-Normal form*)

(* Vartable map: SL id, (Anorm val, type)*)
type VarTableA = Map<vname, AVal * Type>
(* We create a function table to be used in flat, in order to get typed lets for the case of Apply.
   As well as looking up the type of expressions. *)
type FunTableA = Map<fname, Type>
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

let lookupFRet (fn : string) (ftab : FunTableA) =
    match ftab |> Map.tryFind fn with
    | Some t -> t
    | None -> failwith (sprintf "Unkown function in FTable: %s" fn)
let getRetTab (prog:Prog) : FunTableA =
    prog |> List.fold (fun tab (FunDec(fid, rt, _, _)) ->
              Map.add fid rt tab) Map.empty

(* Helper function to get type of an expression, 
   minimal since we can use the actual type checker of fasto when moving the implementation *)
let rec getTypeExp (e : Exp) (vtab : VarTableA) (ftab : FunTableA) : Type =
    match e with
    | Constant v -> valueType v
    | Var v -> 
      lookupType v vtab
    | ArrayLit (_, tp) -> Array tp
    | Plus (_,_) ->  Int
    | Let (x,e1,e2) ->
      (* Just input an arbitrary AVal to the vtab, we just need types here, 
         and since this vtab will not be returned its fine*)
      let vtab1 = bindVar x (V x, getTypeExp e1 vtab ftab) vtab
      getTypeExp e2 vtab1 ftab
    | Index (_,_,tp) -> tp
    | Length (_) -> Int
    (* We assume bot branches of if return the same type*)
    | If (_,e1,_) -> getTypeExp e1 vtab ftab
    | Apply (f,_) -> lookupFRet f ftab
    | Map (_,_,_,_,tp) -> tp

(* flatten an expression, arguments:
   e: an expression
   vtab: SL variable table with an anf value and type
   ftab: function table with return and argument types
   k: a continuation function
   returns an anf tree*)
let rec flat (e : Exp) (vtab : VarTableA) (ftab : FunTableA) (k : AVal -> ANorm): ANorm =
    match e with
    | Constant (IntVal n) ->
      k (N n)

    | Constant (ArrayVal (vs, tp)) ->
      (* Turn into arraylit to reuse code *)
      let arraylit = ArrayLit(List.map (fun v -> Constant v) vs, tp)
      flat arraylit vtab ftab k

    | Var id ->
      lookupVal id vtab |> k

    | ArrayLit (elems, tp) -> 
      (* Takes a list of values, makes it into a computation and puts in a let, 
        final step in arraylit, used as base for flatl accumulator function *)
      let letBase (vals : AVal list) : ANorm =
          let tArr = newVar "arr"
          let comp  = ArrLitA (vals, tp)
          LetA (tArr, Array tp, comp, k (V tArr))

      flatl elems vtab ftab letBase

    | Plus(e1, e2) ->
      (* we use the continuation function to recursivly compile each expression 
        and put it together in a let in the end *)
      flat e1 vtab ftab (fun l ->
          flat e2 vtab ftab (fun r ->
              let t = newVar "t" 
              LetA (t, Int, AddA (l, r), k (V t))))

    | Let(id, e1, e2) ->
      (* call recursivly on e2 with the value from e1, no actual
        LetA nodes to ensure copy and constant propagation *)
      flat e1 vtab ftab (fun v1 ->
          let idtp = getTypeExp e1 vtab ftab
          let vtab1 = bindVar id (v1,idtp) vtab
          flat e2 vtab1 ftab (fun v2 -> k v2))

    | Index(id, e, tp) -> 
      let arr = lookupVal id vtab

      flat e vtab ftab (fun i ->
          let t = newVar "i"
          LetA (t, tp, IndexA(arr, i, tp), k (V t)))

    | Length(e) ->
      flat e vtab ftab (fun arr ->
          let t = newVar "len"
          LetA (t, Int, LenA arr, k (V t)))

    | Apply (f, es) -> 
      (* Simmilar approach as array lit, use flatl accumulator function to flatten each element
          make a base function for the final step *)
      let letBase (vs : AVal list) =
        let tp = lookupFRet f ftab
        let t = newVar "fRes"
        let applComp = ApplyA (f, vs)
        LetA (t, tp, applComp, k (V t))
      
      flatl es vtab ftab letBase

    | If (e1, e2, e3) -> 
      flat e1 vtab ftab (fun v ->
        (* If the guard is an integer we can remove the branch *)
        match v with
        | N 0 ->
          flat e3 vtab ftab k
        | N _ -> 
          flat e2 vtab ftab k
        | V _ ->
        (* Evaluate each branch with its own k function, stopping when we get a value *)
          let a2 = flat e2 vtab ftab (fun v -> ValueA v)
          let a3 = flat e3 vtab ftab (fun v -> ValueA v)
          let ifComp = IfA (v, a2, a3)
        (* We assume both branches of the if return the same type, so only check the type of e2 *)
          let t = newVar "if"
          let tp = getTypeExp e2 vtab ftab
          LetA (t, tp, ifComp, k (V t))
        )
        
    | Map(Param(arg,argtp), fbody, arre, arInT, arOutT) ->
      flat arre vtab ftab (fun arr ->
        //Det er arInt's element type vi skal tjekke...
        //if argtp <> arInT then failwith "Type mismatch in map function argument"
        
        (* Create a new name for the lambda parameter*)
        let arg1 = newVar "mapx"

        (* Bind the argument of the anonymus function in a new vtab and flatten function body *)
        let vtab1 = bindVar arg (V arg1,argtp) vtab
        let fbodya = flat fbody vtab1 ftab (fun v -> ValueA v)

        (* Wrap the map in a let with a new var *)
        let t = newVar "map"

        let mapComp = MapA (Param(arg1,argtp), fbodya, arr, arInT, arOutT)
        LetA(t, arOutT, mapComp, k (V t))
        )

(* accumulator function to flatten a list of expression *)
and flatl (es : Exp list) (vtab : VarTableA) (ftab : FunTableA) (acc : AVal list -> ANorm): ANorm =
    match es with 
    | [] -> acc []
    | e::rst ->
      flat e vtab ftab (fun v ->
          flatl rst vtab ftab (fun vs ->
              acc (v::vs)))

(* ----------------------------------------------------------------------- *)
(* Live set calculation functions *)

(* Set of live variables, used to analyze when we can add drops *)
type LiveSet = Set<vname>

(* Make a single AVal into a liveset, numeric constants will result in an empty liveset *)
let varsAVal (v : AVal) : LiveSet =
    match v with
    | V vn -> Set.singleton vn
    | N _ -> Set.empty

let varsAValList (vs : AVal list) : LiveSet =
    vs |> List.fold (fun acc v -> Set.union acc (varsAVal v)) Set.empty

(* Get the free variables of an anf branch, 
  used for generating the live variables in branch computations such as if-else and map*)
let rec getFreeVarsANorm (a : ANorm) : LiveSet =
    match a with
    | ValueA v -> varsAVal v(* Dont add constants *)
    (* fv(let x = a1 in a2) = fv(a1) U (fv(a2) \ {x}) *)
    | LetA (id,_, c, body) ->
      let cVars = getVarsComp c
      let bFree = getFreeVarsANorm body |> Set.remove id
      Set.union cVars bFree
    | IncA (_,body) | DecA (_,_,body) -> failwith "Inc or Dec present in anf tree, getFreeVarsANorm"

(* Function for returning all the used variable names of a computation for generating livesets
   in the case of branching computations we only add the free variables *)
and getVarsComp (c : AComp) : LiveSet =
    match c with
    | ApplyA (_,vals)  -> varsAValList vals 
    | AddA (v1,v2)     -> varsAValList [v1;v2]
    | ArrLitA(vals,_)  -> varsAValList vals
    | IndexA (arr,i,_) -> varsAValList [arr;i]
    | LenA (arr)       -> varsAVal arr

    (* Look for free variables in the bodies of control flow nodes, 
       in addition to immediate AVals *)
    | IfA (g,b1,b2) -> Set.union (getFreeVarsANorm b1) (getFreeVarsANorm b2) 
                        |> Set.union (varsAVal g)
    | MapA (_,body,arr,_,_) -> Set.union (getFreeVarsANorm body) (varsAVal arr)

(* ----------------------------------------------------------------------- *)
(* Dead binding elimination helper functions *)

(* Check if an anf contains any function calls, helper function for cIO*)
let rec hasApply (a : ANorm) : bool =
    match a with
    | LetA (_,_,ApplyA(_,_),_) -> true
    | LetA (_,_,_,body) -> hasApply body
    | IncA _ | DecA _ -> failwith "Inc or Dec present in anf tree, hasApply"
    | ValueA _ -> false

(* Check if a computation may contain IO (side effects). 
   Computations with side effects cannot be removed in dead binding elimination*)
let cIO (c : AComp) : bool =
  match c with
  | ApplyA(_,_) -> true (* Functions may contain calls to IO functions in real fasto*)
  | IfA(_,b1,b2) -> hasApply b1 || hasApply b2 (* IfA only contains IO if at least one branch does *)
  | MapA(_,body,_,_,_) -> hasApply body
  | _ -> false

(* ----------------------------------------------------------------------- *)
(* Helper functions for annotating anf tree with decrements*)

(* Variable type table, used to propperly decrement variables *)
type TypeTable = Map<vname,Type>

(* Get the dimensions of an array type *)
let rec getDims (t : Type) : int =
    match t with 
    | Int -> 0
    | Array tp -> 1 + getDims tp

(* Insert decrements for each array variable in the set, around an anf node
   Dont insert decs for integeres *)
let insertDecs (dead : LiveSet) (ttab : TypeTable) (body : ANorm) : ANorm =
    dead
    |> Set.fold (fun a v ->
        let dims = match Map.tryFind v ttab with
                    | Some tp -> getDims tp
                    | None -> failwith (sprintf "Unkown variable in anfvtab in analyse: %s" v)
        if dims = 0 then a (* Dont decrement integer variables *)
        else DecA (v,dims,a)) body

let insertIncs (copied : LiveSet) (ttab : TypeTable) (A : ANorm) : ANorm =
    copied
    |> Set.fold (fun a v ->
        let dims = match Map.tryFind v ttab with
                    | Some tp -> getDims tp
                    | None -> failwith (sprintf "Unkown variable in anfvtab in analyse: %s" v)
        if dims = 0 then a (* Dont decrement integer variables *)
        else IncA (v,a)) A

(* ----------------------------------------------------------------------- *)
(* Analyser, annotate anf tree with decrements and increments *)

(* Analyse an anf, arguments:
   a : Anf tree/subtree
   liveAfter : Variables that are live after the entire subtree
   ttab : A type table, used to add dimensions to the decrements as well as filtering out integers 
   useFlag : States wheather the result of the anf tree will be used
   returns the annotated subtree along with the corresponding liveset *)
let rec analyse (a : ANorm) (liveAbove : LiveSet) (ttab : TypeTable) (useFlag : bool) : ANorm * LiveSet =
  match a with 
  | ValueA (N _) -> a, liveAbove
  | ValueA (V x) -> 
    if useFlag then (* The value within the variable will be used after this sub-tree has finished executing *)
      (* the value within x is needed, add it to the liveset *)
      let liveIn = liveAbove.Add x
      (* If x is live after the sub-tree, and it's value is needed, increment if x is an array *)
      match Map.tryFind x ttab with
      | Some tp when tp <> Int && liveAbove.Contains x -> IncA (x,a), liveIn
      | _ -> a, liveIn
    else
      (* The result of the tree is unused, so we can replace the variable with 0
         Even if x is still live after, it is already included in the liveAfter set*)
      ValueA (N 0), liveAbove
  | LetA (id, tp, c, body) ->
      (* Build the type table while going top down trough the tree *)
      let ttab1 = Map.add id tp ttab
      (* Get the liveSet from the body of the let, if the variables from the let's computation 
        dont exist in the body's liveset, they are used for the last time here and we can drop them*)
      let body1, liveBelow = analyse body liveAbove ttab1 useFlag
      (* Check if the assigned variable is used, otherwise we can remove this node. Dead binding elimination *)
      let deadBinding = not (liveBelow.Contains id)
      (* If we have a dead binding and our computation is sure to have no side effects, 
          we can remove this node form the tree*)
      if deadBinding && not (cIO c) then
        body1, liveBelow
      else
      (* Compute sets: *)
      (* Get vars used in the computation (Gen set) *)
      let compLive = getVarsComp c
      (* Dead variables are the ones used in this node, but not live after*)
      let dead = Set.difference compLive liveBelow
      (* LiveIn is compLive U (liveOut \ kill)*)
      let liveIn = Set.union compLive (liveBelow |> Set.remove id)

      (* Some computations require extra steps:*)
      match c with
      (* If statements might need extra decrements, if a variable is used for the last time in only one branch*)
      | IfA (g,b1,b2) -> 
        (* Call recursivly on each branch *)
        let b1body, b1Live = analyse b1 liveBelow ttab1 (not deadBinding) // Give the flag depending on weather id is used after
        let b2body, b2Live = analyse b2 liveBelow ttab1 (not deadBinding)

        (* Get the free variables from each branch *)
        let b1Free = getFreeVarsANorm b1
        let b2Free = getFreeVarsANorm b2
        (* Free Variables used only in one branch and not needed afterwards
            won't be decremented by the recursive analyse call in the other branch.
            We compute them here to insert the missing decrements explicitly.*)
        let b1dead = Set.difference (Set.difference b1Free b2Free) liveBelow
        let b2dead = Set.difference (Set.difference b2Free b1Free) liveBelow

        let b1dec = insertDecs b2dead ttab1 b1body
        let b2dec = insertDecs b1dead ttab1 b2body
        let decIf = IfA (g, b1dec, b2dec)

        (* Check if the guard is dead, if so insert a dec *)
        let decBodyGuard = Set.difference (varsAVal g) liveBelow |> fun gSet -> insertDecs gSet ttab body1 

        LetA (id, tp, decIf, decBodyGuard), liveIn

  (* Insert increments around an anf node containing a function call,
    for each argument used n times, increment it n-1 times + 1 more if its still live after *)
      | ApplyA (_,args) 
      | ArrLitA (args,_) -> (* We treat ArrayLit {a,b,c} as a function mkrArr(a,b,c)*)
        let incrLet =
          args
          (* Filter out all non variables and convert these AVals to strings*)
          |> List.fold (fun lst v -> match v with
                                      | V vn -> vn::lst
                                      | N _ -> lst) []
          (*Create a map that keeps track of how many times each of these variables are used as function arguments*)
          |> List.fold (fun map arg -> 
              map |> Map.change arg (function
                | Some n -> Some (n+1)
                | None -> Some 1(*
                  (* We only care about array variables *)
                  match Map.tryFind arg ttab1 with
                  | Some tp when tp <> Int -> Some 1
                  | Some _ -> None
                  | None -> failwith (sprintf "Unkown variable in anfvtab in analyse: %s" arg)*)
                  )) Map.empty

          (* If the argument is dead after, decrement the count *)
          |> Map.map (fun arg n -> if dead.Contains arg then n-1 else n) 
          (* Turn this back into a list, with duplicates corresponding to the count*)
          |> Map.fold (fun lst arg n -> lst @ List.replicate n arg) []
          (* Add increments *)
          |> List.fold (fun A v -> insertIncs (Set.singleton v) ttab1 A (*List.fold (fun A v -> 
              match Map.tryFind v ttab1 with 
              | Some _ -> IncA (v, A)
              | None -> failwith (sprintf "Unkown variable in anfvtab in analyse: %s" v)*) ) 
              (LetA (id, tp, c, body1))
        incrLet, liveIn
      | MapA(Param(arg,argtp), fbody, arr, arInT, arOutT) ->
        (* Get the free variables used in the lambda function, we dont want these to be decremented in each iteration of the loop*)
        let freeVars = getFreeVarsANorm fbody
        (* Analyse the body, but add the freeVars to the liveAfter set to ensure they dont get decremented
            We also give a true useFlag as any array pointers might be copied to a new array *)
        let ttab2 = ttab1 |> Map.add arg argtp (* Add the map's paramater to the type table *)
        let fbody1, _ = analyse fbody (Set.union liveBelow freeVars) ttab2 true 
        (* After the map, we want to decrement any of the freeVars that was used last time in the lambda func
            as well as the map's parameter. Remove the lambdas parameter, this should not be decremented. *)
        let deadMap = Set.difference (varsAVal arr |> Set.union freeVars) liveBelow |> Set.remove arg
        (* Insert a dec for each of these variables *)
        let decbodyMap = insertDecs deadMap ttab2 body1
        let mapComp = MapA(Param(arg,argtp), fbody1, arr, arInT, arOutT)
        LetA(id, tp, mapComp, decbodyMap), liveIn

      (* For index we might need to increment if we are accessing an element in a 2d array*)
      | IndexA(_,_,t) when t<>Int ->
        (* Decrement dead variables and increment the new variable that was assigned in the let, 
            as this contains the copied pointer to the array*)
        let incrBody = IncA(id, insertDecs dead ttab1 body1)
        LetA (id, tp, c, incrBody), liveIn
      | LenA(_) | AddA(_,_) | IndexA(_,_,_) -> (* Len, plus and index in 1d array requires no special steps. *)
        (* Insert a dec for each dead variable *)
        let decbody = insertDecs dead ttab1 body1
        (* the new let with drops, and the body's live variable + comp's variables *)
        LetA (id, tp, c, decbody) , liveIn

  | DecA (_,_,_) -> failwith "Decrement already present in anf in analyse"
  | IncA (_,_) -> failwith "Increment already present in anf in analyse"


(* Make a FunDec into anf (AFunDec) *)
let anfFun (fn : FunDec) (ftab : FunTableA): AFunDec =
    let (FunDec (fname, tp, args, exp)) = fn

    (* 1.  Generate fresh ANF names for every source parameter
       2.  Create two symbol tables while we go:
           argVtab: maps SL names to their fresh ANF AVals
           ttab:    maps fresh ANF names to their types
       We accumulate the freshly-named parameter list in Aargs.*)
    let Aargs, argVtab, ttab =
        args
        |> List.fold
            (fun (alst, vtab, ttab) (Param (arg, t)) ->
                let arg1 = newVar "arg"

                let alst1 = alst @ [ Param (arg1, t) ]
                let vtab1 = bindVar arg (V arg1, t) vtab
                let ttab1 = Map.add arg1 t ttab
                (alst1, vtab1, ttab1)
            ) ([], Map.empty, Map.empty)
    (* Flatten the expression to a-normal form *) 
    let anf = flat exp argVtab ftab (fun v -> ValueA v)

    (* Analyse *)
    let anf1, live = analyse anf Set.empty ttab true

    (* Get a set of all the function arguments *)
    let argSet = ttab |> Map.fold (fun set arg _ -> set |> Set.add arg) Set.empty 
    (* Check if any function arguments are unused *)
    let dead = Set.difference argSet live
    (* Insert a decr for each dead var *)
    let anf2 = insertDecs dead ttab anf1

    (* fun name * arg names * analyzed anf body *)
    AFunDec (fname, tp, Aargs, anf2) 

(* Make program into AProg *)
let anfProg (prog : Prog) : AProg =
    let ftab = getRetTab prog
    counter <- 1
    List.map (fun fn -> anfFun fn ftab) prog 

(* ----------------------------------------------------------------------- *)
(* Code generator, converting an anf tree into machine code *)

(* Generate Risc-v code from A normal form, split into three for the different types *)
let rec genVal (v : AVal) (vtab : VarTableC) (place : reg) : PseudoRV list =
    match v with 
    | N n -> [ LI (place, n)]
    | V var -> match Map.tryFind var vtab with
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
      (* Generate each argument into its own fresh register, then add them to the CALL union*)
      let argsCode,regs = args
                          |> List.fold (fun (code, argR) a -> 
                          let argReg = newReg (id+"arg")
                          code@(genVal a vtab argReg),argR@[argReg]) ([],[])
      argsCode @ [CALL (id,regs); MV(place, Rret)]

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
      let vtab1  = Map.add arg resReg vtab
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
      let vtab1 = Map.add id t vtab
      (* Gen a with new vtable *)
      let code2 = gen a vtab1 place
      code1 @ code2

    | IncA (id, a) -> 
      let incCode = match Map.tryFind id vtab with
                    | Some r -> [ INC r ]
                    | None -> failwith (sprintf "Unkown variable to increment %s" id)
      let code = gen a vtab place
      incCode @ code
    | DecA (id, d, a) ->
      let decCode = match Map.tryFind id vtab with
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
      let vtab1 = Map.add arg localReg vtab
      let (code2,regs,vtab2) = genArgs rst vtab1 (argNum + 1) (argRegs@[argReg]) fname
      [MV (localReg, argReg)] @ code2, regs,vtab2

let genFun (fn : AFunDec) : (string * (reg list * PseudoRV list)) =
    let (AFunDec (fname, tp, args, A)) = fn
    (* Get function label *)
    let funLab = getFname fname

    (* Bind each formal argument to the symbolic argument registers 
       We use a list.fold to accumulate the vtable along with a counter 
       for generation of argument registers *)
    let (prologCode, argRegs,vtab) = genArgs args Map.empty 0 [] fname
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
    let res,heap = simulate Map.empty rvProg
    res,heap,tp

(* ----------------------------------------------------------------------- *)
(* Testing functions *)

let rec dimsToType (dims : int) : Type =
    match dims with
    | 1 -> Int
    | n when n > 1 -> Array (dimsToType (dims-1))
    | _ -> failwith "Invalid dimension argument in dimsToType"

(* Turn the output of the compiler into an ArrayVal, to be compared with interpretor res*)
let rec rebuildCRes (arrP : int) (hp : Heap) (dims : int) =
    match dims with
    | 0 ->  IntVal arrP
    | n when n > 0 ->
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

(* Pretty printer for values *)
let rec ppValue (v : Value) : string =
    match v with
    | IntVal n -> string n
    | ArrayVal (vs, tp) ->
        let elems = 
            vs 
            |> List.map ppValue
            |> String.concat "; "
        match tp with
        | Int -> sprintf "[%s]" elems
        | _   -> sprintf "[%s]" (String.concat " " (vs |> List.map ppValue))

let runTestA (prog : Prog) (pname : string) =
    let (resC, hp, tp) = flatGenSimulate prog

    let dims = getDims tp
    let valC = rebuildCRes resC hp dims
    let valI = evalProg prog

    let hpCount = 
      if tp <> Int then (* If we have an array, decrement it from the heap *)
        let decProg : RVProg = Map.empty |> Map.add "main" ([],[LI ("a", resC) ; DEC ("a", dims) ; LI (Rret, 0) ; RET])
        simulate hp decProg |> snd
      else hp
      |> Map.count

    let ppC = ppValue valC

    if compareVal valI valC then
      if hpCount = 0 then
        printfn "Sucess! Test: %s, result: %s" pname ppC
      else printfn "Fail... Test: %s, expected heap count %i, got %i: %s" pname 0 hpCount ppC
    else 
      let ppI = ppValue valI
      printfn "Fail... Test: %s, expected res %s, got %s" pname ppI ppC
    