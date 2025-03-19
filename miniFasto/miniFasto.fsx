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
  | ArrayLit  of Exp<'T> list * 'T
  | Plus      of Exp<'T> * Exp<'T>
  | Let       of string * Exp<'T> * Exp<'T> // Let "a" = exp in exp
  | Index     of string * Exp<'T> * 'T
  | Length    of Exp<'T> * 'T

type TypedExp = Exp<Type>

(* ----------------------------------------------------------------------- *)
(* Absyn of pseudo-Risc-V *)
type reg  = string
type imm  = int

(* Pseudo Risc-v instructions*)
type PseudoRV =
    LI  of reg*imm      (* LI(rd,imm):      rd = imm *)
  | MV of reg*reg       (* MV(rd,rs):       rd = rs *)

  | ADDI of reg*reg*imm (* ADDI(rd,rs,imm): rd = rs + imm*)
  | ADD  of reg*reg*reg (* ADD(rd,r1,r2):   rd = r1 + r2 *)

  | LW of reg*reg*imm   (* LW(rd,rs,imm):   rd = *(int* )(rs + imm) *)
  | SW of reg*reg*imm   (* SW(rd,rs,imm):   *(int* )(rs + imm) = rd *)

  | ALLOC of reg*reg    (* ALLOC(rd,rs):    rd = adress of rs allocated words *)
  | FREE  of reg        (* FREE(rs):        free allocated memory of rs adress *)

let mutable counter = 0
let newName name =
    counter <- counter + 1
    "x" + string counter + "_" + name

let newReg name = newName name

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

type VarTableI = SymTab<Value> (* Interpretor vartable *)
type VarTableC = SymTab<reg>   (* Compiler vartable *)
type VarTableS = SymTab<int>   (* Simulator vartable, our registers *)

(* ----------------------------------------------------------------------- *)
(* Interpretor *)
let rec evalExp (e : TypedExp, vtab : VarTableI) : Value =
    match e with
    Constant(v) -> v
  | ArrayLit(l, t) ->
        let els = (List.map (fun x -> evalExp(x, vtab)) l)
        ArrayVal (els, t)
  | Plus(e1, e2) ->
        let res1 = evalExp(e1, vtab)
        let res2 = evalExp(e2, vtab)
        match (res1, res2) with
          | (IntVal n1, IntVal n2) -> IntVal (n1+n2)
          | (_, _) -> failwith "Type mismatch in plus operation"
  | Let(id, e1, e2) ->
        let res   = evalExp(e1, vtab)
        let nvtab = bind id res vtab
        evalExp(e2, nvtab)
  | Index(id, e, t) ->
        let indv = evalExp(e, vtab)
        let arr = lookup id vtab
        match (arr, indv) with
          | (None, _) -> failwith "Non existing array variable"
          | (Some (ArrayVal(lst, t)), IntVal ind) ->
                let len = List.length lst
                if 0 <= ind && ind < len
                then lst.Item ind
                else failwith "Array index out of bounds"
          | (Some m, IntVal _) -> failwith "Indexing into non array"
          | (_, _) -> failwith "Indexing expression type error"
  | Length(e, _) ->
        let arr = evalExp(e, vtab)
        match arr with
            | ArrayVal(lst, _) -> IntVal (List.length lst)
            | _ -> failwith "Length applied to non array"


(* ----------------------------------------------------------------------- *)
(* Interpretor tests *)
let vtab = empty()
let e1 = Plus(Constant(IntVal 4), Constant(IntVal 20))
let e2 = ArrayLit((List.init 5 (fun x -> Constant(IntVal x))), Int)
let e3 = Let("a", e2, Index("a", Constant(IntVal 3), Int))
let e4 = Length(e2, Array(Int))

printfn "%A" (evalExp(e1, vtab))
printfn "%A" (evalExp(e2, vtab))
printfn "%A" (evalExp(e3, vtab))
printfn "%A" (evalExp(e4, vtab))

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

  | ArrayLit (elems, tp) ->
    let sizeReg = newReg "size"
    let addrReg = newReg "addr"
    let tmpReg  = newReg "tmp"

    (* Store size in size_reg, allocate, put addres of first elm in addr reg *)
    let header = [ LI (sizeReg, List.length elems)
                 ; ALLOC (place, sizeReg)
                 ; ADDI (addrReg, place, 4) ]
    
    (* Compile each expression from list, store it in addrReg addres, increment addrReg*)
    let compileElem elmExp =
        let elmCode = compileExp elmExp vtable tmpReg
        elmCode @
        [ SW (tmpReg, addrReg, 0)
        ; ADDI (addrReg, addrReg, 4) ]
    
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
    let code1 = compileExp e vtable t
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
    let initCode = [ ADDI (arrReg, arrBeg, 4) ] // arrReg = arrBeg + 4

    (* TODO Check bounds... *)

    (* Since we only have int arrays and 2D arrays no type checking needed
       just multiply index by 4 for word size, 
       due to limited RISC-V instructions, we use multiple add instructions *)
    let loadCode = [ ADD (indReg, indReg, indReg) 
                   ; ADD (indReg, indReg, indReg) 
                   ; ADD (arrReg, arrReg, indReg)
                   ; LW  (place, arrReg, 0) ]
    
    indCode @ initCode @ loadCode
  | Length(e, _) ->
    (* load word from arr pointer *)
    let arrAddr = newReg "len_arr"
    let code1    = compileExp e vtable arrAddr
    code1 @ [ LW(place, arrAddr, 0) ]

(* ----------------------------------------------------------------------- *)
(* RISC-V simulator *)

let heap = [| for i in 1..100 -> 0 |] 
let hp = 0

let lookupReg reg vtab = 
    let res = lookup reg vtab
    match res with 
    | Some x -> x
    | None -> failwith (sprintf "No such register %s" reg)

(* TODO add pc for future, needed for loops and if statements *)
let simulateInst (ins : PseudoRV, regs : VarTableS) : VarTableS =
    printfn "Simulating instruction: %A" ins
    match ins with
    | LI (r, i) -> 
      bind r i regs
    | MV (rd, rs) -> 
      let rsVal = lookupReg rs regs
      bind rd rsVal regs
    | ADDI (rd, rs, i) ->
      let rsVal = lookupReg rs regs
      bind rd (rsVal + i) regs
    | ADD (rd, r1, r2) ->
      let r1val = lookupReg r1 regs
      let r2val = lookupReg r2 regs
      bind rd (r1val + r2val) regs

    (* TODO: Implement*)
    | LW (rd, rs, i) -> regs
    | SW (rd, rs, i) -> regs
    | ALLOC (rd, rs) -> regs
    | FREE (rs)      -> regs