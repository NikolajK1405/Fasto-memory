(* Mini Fasto for testing and experimental purposes
   minimal error handling as this will be used carefully *)

(* Simple abstract syntax, subset of AbSyn.fs, only int, array, let and iotas*)
type Type =
    Int
  | Array of Type

type Value =
    IntVal   of int
  | ArrayVal of Value list * Type

let valueType = function
  | (IntVal _)         -> Int
  | (ArrayVal (_,tp))  -> Array tp

(* Should we have If statements? *)
type Exp<'T> =
    Constant  of Value
  | Var       of string
  | ArrayLit  of Exp<'T> list * 'T
  | Plus      of Exp<'T> * Exp<'T>
  | Let       of string * Exp<'T> * Exp<'T> // Let "a" = exp in exp 
  //Tilføj hvad er typen af den bundende variabel, er det en pointer? både i let og funktions argument
  | Index     of string * Exp<'T> * 'T
  | Length    of Exp<'T> * 'T
  | If        of Exp<'T> * Exp<'T> * Exp<'T> // If exp != 0 Then exp Else exp
  | Apply     of string * Exp<'T> list 

type TypedExp = Exp<Type>
type Param = Param of string * Type
type FunDec = FunDec of string * Type * Param list * Exp<Type>
type Prog = FunDec list

(* ----------------------------------------------------------------------- *)
(* Absyn of pseudo-Risc-V *)
type reg  = string
type imm  = int
type addr = string

let R0 : reg = "x_0"

(* Pseudo Risc-v instructions*)
type PseudoRV =
  | LABEL of addr

  | LI  of reg*imm      (* LI(rd,imm):      rd = imm *)
  | MV  of reg*reg      (* MV(rd,rs):       rd = rs *)

  | ADDI of reg*reg*imm (* ADDI(rd,rs,imm): rd = rs + imm*)
  | ADD  of reg*reg*reg (* ADD(rd,r1,r2):   rd = r1 + r2 *)

  | LW of reg*reg*imm   (* LW(rd,rs,imm):   rd = *(int* )(rs + imm) *)
  | SW of reg*reg*imm   (* SW(rd,rs,imm):   *(int* )(rs + imm) = rd *)

  | BEQ of reg*reg*addr (* BEQ(r1,r2,addr): if ($r1 == $r2) goto(addr) *)
  | J   of addr         (* J(addr):         goto(addr) *)

  | ALLOC of reg*reg    (* ALLOC(rd,rs):    rd = adress of rs allocated words *)
  | FREE  of reg        (* FREE(rs):        free allocated memory of rs adress *)

let mutable counter = 1
let newName name =
    counter <- counter + 1
    name + "_" +  string counter 

let newReg name = newName ("x" + name)
let newLab name = newName ("l." + name)

(* ----------------------------------------------------------------------- *)
(* Symbol table *)
type SymTab<'a> = SymTab of (string * 'a) list

let empty () = SymTab []

let rec lookup n tab =
  match tab with
    | SymTab [] -> None
    | SymTab ((n1,i1)::remtab) ->
        if n = n1
        then Some i1
        else lookup n (SymTab remtab)

let bind n i (SymTab stab) = SymTab ((n,i)::stab)
let combine (SymTab t1) (SymTab t2) = SymTab (t1 @ t2)

type VarTableI = SymTab<Value> (* Interpretor vartable *)
type VarTableC = SymTab<reg>   (* Compiler vartable *)
type FunTable  = SymTab<FunDec>

(* ----------------------------------------------------------------------- *)
(* Interpretor *)
let rec bindParams (fargs : Param list) (args : Value list) (fid : string)=
    match fargs, args with
    | [], [] -> empty ()
    | Param (n, t)::ps, v::vs ->
        let vtab = bindParams ps vs fid
        if (valueType v) = t then
          match lookup n vtab with
          | None -> bind n v vtab
          | Some _ -> failwith (sprintf "Tried to bind existing function argument %s in function: %s" n fid)
        else failwith (sprintf "Incorrect type for function argument\n
                        Function %s expected: %A, got: %A" fid t (valueType v))
    | _, _ -> failwith (sprintf "Mismatch in formal and actual parameters for function: %s" fid)

let rec evalExp (e : TypedExp) (vtab : VarTableI) (ftab : FunTable): Value =
    match e with
    Constant(v) -> v
  | Var(id) -> match (lookup id vtab) with
               | Some v -> v
               | None   -> failwith (sprintf "Unkown variable %s" id)
  | ArrayLit(l, t) ->
        let len = IntVal (List.length l)
        let els = (List.map (fun x -> evalExp x vtab ftab) l)
        ArrayVal (len::els, t)
  | Plus(e1, e2) ->
        let res1 = evalExp e1 vtab ftab
        let res2 = evalExp e2 vtab ftab
        match (res1, res2) with
          | (IntVal n1, IntVal n2) -> IntVal (n1+n2)
          | (_, _) -> failwith "Type mismatch in plus operation"
  | Let(id, e1, e2) ->
        let res   = evalExp e1 vtab ftab
        let nvtab = bind id res vtab
        evalExp e2 nvtab ftab
  | Index(id, e1, t) ->
        let indv = evalExp e1 vtab ftab
        let arr = lookup id vtab
        match (arr, indv) with
          | (None, _) -> failwith "Non existing array variable"
          | (Some (ArrayVal(lst, t)), IntVal ind) ->
                let len = List.length lst
                if 0 <= ind && ind < len
                then lst.Item (ind + 1) (* +1 to account for array header *)
                else failwith "Array index out of bounds"
          | (Some m, IntVal _) -> failwith "Indexing into non array"
          | (_, _) -> failwith "Indexing expression type error"
  | Length(e1, _) ->
        let arr = evalExp e1 vtab ftab
        match arr with
            | ArrayVal(lst, _) -> lst[0]
            | _ -> failwith "Length applied to non array"
  | If (e1, e2, e3) ->
        let guard = evalExp e1 vtab ftab
        match guard with
          | IntVal 0 -> evalExp e3 vtab ftab (* Else *)
          | _        -> evalExp e2 vtab ftab (* Then *)
  | Apply (f, es) ->
        match (lookup f ftab) with
          | None -> failwith (sprintf "No such function: %s" f)
          | Some fn -> 
            let args = List.map (fun e -> evalExp e vtab ftab) es
            callFun fn args vtab ftab

and callFun (fn : FunDec) (args : Value list) (vtab : VarTableI) (ftab : FunTable) =
    let (FunDec(fid, t, fargs, e)) = fn
    let nvtab = combine (bindParams fargs args fid) vtab
    let res = evalExp e nvtab ftab
    if (valueType res) = t then res
    else failwith (sprintf "Result of function %s: %A, does not match expected output: %A" fid (valueType res) t)
(* ----------------------------------------------------------------------- *)
(* Code generator *)

let rec compileExp  (e      : TypedExp)
                    (vtable : VarTableC)
                    (place  : reg)
                  : PseudoRV list =
  match e with
  | Constant (IntVal n) ->
    [ LI (place, n) ]

  | Constant (ArrayVal (vs, tp)) ->
    (* Turn into arraylit to reuse code *)
    let arraylit = ArrayLit(List.map (fun v -> Constant (v)) vs, tp)
    compileExp arraylit vtable place
  | Var (id) -> []
  | ArrayLit (elems, tp) ->
    let sizeReg = newReg "size"
    let addrReg = newReg "addr"
    let tmpReg  = newReg "tmp"

    (* Store size in size_reg, allocate, put addres of first elm in addr reg *)
    let header = [ LI (sizeReg, List.length elems)
                ; ALLOC (place, sizeReg)
                ; ADDI (addrReg, place, 1) ]
    
    (* Compile each expression from list, store it in addrReg addres, increment addrReg*)
    let compileElem elmExp =
        let elmCode = compileExp elmExp vtable tmpReg
        elmCode @
        [ SW (tmpReg, addrReg, 0)
        ; ADDI (addrReg, addrReg, 1) ]
    
    let elemsCode = List.collect compileElem elems
    header @ elemsCode
  | Plus(e1, e2) ->
    let t1 = newReg "plus_L"
    let t2 = newReg "plus_R"
    let code1 = compileExp e1 vtable t1
    let code2 = compileExp e2 vtable t2
    code1 @ code2 @ [ADD (place,t1,t2)]
  | Let(id, e1, e2) ->
    let t = newReg ("let_" + id)
    (* Compile e1 and put result into t *)
    let code1 = compileExp e1 vtable t
    let vtab1 = bind id t vtable
    (* Compile e2 with the new vtable*)
    let code2 = compileExp e2 vtab1 place
    code1 @ code2

  | Index(id, e, t) ->
    (* Compute index *)
    let indReg  = newReg "arr_ind"
    let indCode = compileExp e vtable indReg

    (* Computer pointer to start of array data segment *)
    let arrReg = newReg "arr_data" // Data pointer
    let arrBeg = match lookup id vtable with // Array pointer
                 | None -> failwith (sprintf "Array %s not found" id)
                 | Some regName -> regName
    let initCode = [ ADDI (arrReg, arrBeg, 1) ] // arrReg = arrBeg + 4

    (* TODO Check bounds... *)

    (* Since we only have int arrays and 2D arrays no type checking needed
      all adresses are word sized so no need to =*4 *)
    let loadCode = [ ADD (arrReg, arrReg, indReg)
                  ; LW  (place, arrReg, 0) ]
    
    indCode @ initCode @ loadCode
  | Length(e, _) ->
    (* load word from arr pointer *)
    let arrAddr = newReg "len_arr"
    let code1    = compileExp e vtable arrAddr
    code1 @ [ LW(place, arrAddr, 0) ]
  | If (e1, e2, e3) -> 
    let thenLabel = newLab "then"
    let elseLabel = newLab "else"
    let endLabel = newLab "endif"
    let cond = newReg "cond"
    let code1 = compileExp e1 vtable cond
    let code2 = compileExp e2 vtable place
    let code3 = compileExp e3 vtable place
    code1 @ [BEQ (cond, R0, elseLabel); LABEL thenLabel] @ code2  @
      [ J endLabel; LABEL elseLabel ] @
      code3 @ [LABEL endLabel]
  | Apply (f, es) -> []

(* ----------------------------------------------------------------------- *)
(* RISC-V simulator *)

type Registers = Map<reg, int>
let mutable regs: Registers = Map.empty |> Map.add R0 0 (* Init with R0 register *)

let lookupReg reg  = 
    let res = regs.TryFind reg
    match res with 
    | Some x -> x
    | None -> failwith (sprintf "No such register %s" reg)

let updateReg reg x =
    regs <- regs.Add (reg, x)

type Heap = Map<int, array<int>> (* each allocated array has an adress (int) and a corresponding array*)
let mutable heap: Heap = Map.empty
let mutable hp = 0
let offsetSize = 1000 (* Max array size 999 *)
let splitAddr addr = 
    let block  = addr / offsetSize (* strip of 3 least significant digits to get the Map key for the array *) 
    let offset = addr % offsetSize (* get three least significant digits to calculate offset *)
    (block, offset)

let joinAddr block offset = (* optional offset *)
    block * offsetSize + offset
let lookUpHeap addr =
    let (block, offset) = splitAddr addr
    let array = match heap.TryFind block with
                | Some arr -> arr
                | None -> failwith (sprintf "No allocated block: %i" block)
    (array, offset) (* return array and offset *)

let writeHeap addr word = 
    let (array, offset) = lookUpHeap addr
    array.[offset] <- word

let readHeap addr = 
    let (array, offset) = lookUpHeap addr
    array.[offset]


(* TODO add pc for future, needed for loops and if statements *)
let simulateInst (ins : PseudoRV) =
    //printfn "Simulating instruction: %A" ins
    match ins with
    | LI (r, i) -> 
      updateReg r i
    | MV (rd, rs) -> 
      let rsVal = lookupReg rs
      updateReg rd rsVal
    | ADDI (rd, rs, i) ->
      let rsVal = lookupReg rs
      updateReg rd (rsVal + i)
    | ADD (rd, r1, r2) ->
      let r1val = lookupReg r1
      let r2val = lookupReg r2
      updateReg rd (r1val + r2val)
    | LW (rd, rs, i) ->
      let rsVal = lookupReg rs
      let word = readHeap (rsVal + i)
      updateReg rd word
    | SW (rd, rs, i) -> 
      let rsVal = lookupReg rs
      let rdVal = lookupReg rd
      writeHeap (rsVal + i) rdVal
    | ALLOC (rd, rs) ->
      let rsVal = lookupReg rs
      (* create array in heap map object, init first word to store length *)
      heap <- heap.Add (hp, Array.init (rsVal + 1) (fun x -> if x = 0 then rsVal else 0))
      (* the adress is array number + 3 zeroes *)
      updateReg rd (joinAddr hp 0)
      hp <- hp + 1
    | FREE (rs) ->
      let (blockKey, _) = splitAddr (lookupReg rs)
      if heap.ContainsKey blockKey then
        heap <- heap.Remove blockKey
      else failwith (sprintf "Tried freeing non allocated block: %i" blockKey)

let compileSimulate (e : TypedExp) : int =
    (* compile expression and place result in "res" register *)
    let resReg = "res"
    let vtab: VarTableC = empty()
    let code = compileExp e vtab resReg

    (* Reset all global states *)
    regs <- Map.empty
    heap <- Map.empty
    hp <- 0
    counter <- 0

    (* simulate code *)
    for ins in code do
      simulateInst ins
    
    (* return result *)
    lookupReg resReg

(* ----------------------------------------------------------------------- *)
(* Tests *)
let compareInterpCompArray (lst, t) c = 
    let interpArr = List.toArray lst
    let interpLen = match interpArr[0] with 
                    | IntVal i -> i 
                    | _ -> failwith (sprintf "Non integer in size header of array: %A" interpArr)

    let (compArr, _) = lookUpHeap c
    let compLen = compArr[0]

    if not (compLen = interpLen) then 
      printfn "Failed. Different length of arrays\nInterpretor: %A\nCompiler: %A" interpArr compArr
    else 
      for i = 1 to compLen do
        match (interpArr[i], compArr[i]) with
        | (IntVal i, c) when i = c -> ()
        | (IntVal i, c) -> printfn "Failed. Mismatch in arrays\nInterpretor: %A\nCompiler: %A" interpArr compArr
        | (ArrayVal (_,_), _) -> () (* TODO: implement compariosn of 2D arrays *)
      printfn "Succes! Result: %A" interpArr
      
(* Compare result from interpretor and compiler*)
let compareCompInterp (e : TypedExp) =
    let ivtab : VarTableI = empty()
    let iftab : FunTable  = empty()
    let interpRes = evalExp e ivtab iftab

    let compRes = compileSimulate e

    match (interpRes, compRes) with
    | (IntVal i, c) when i = c -> printfn "Succes! Result: %i" i
    | (IntVal i, c)            -> printfn "Failed. interpretor: %i, compiler: %i" i c
    | (ArrayVal (lst, t), c)   -> compareInterpCompArray (lst, t) c

let e1 = Plus(Constant(IntVal 4), Constant(IntVal 20))
let e2 = ArrayLit((List.init 5 (fun x -> Constant(IntVal x))), Int)
let e3 = Let("a", e2, Index("a", Constant(IntVal 3), Int))
let e4 = Length(e2, Array(Int))
let e5 = Let("a", e2, Let("b", e2, Plus(Index("a", Constant(IntVal 2), Int), Index("b", Constant(IntVal 4), Int) )))

compareCompInterp e1
compareCompInterp e2
compareCompInterp e3
compareCompInterp e4
compareCompInterp e5

let addFun = FunDec("add", Int, [Param("a", Int); Param("b", Int)],
                    Plus(Var("a"), Var("b")))

let eAdd = Apply("add", [Constant(IntVal 10); Constant(IntVal 32)])

let ftab = bind "add" addFun (empty())
let res = evalExp eAdd (empty()) ftab
printfn "Interpreter result: %A" res