(* RISC-V Code generator for Fasto, adapted from MIPS *)

module CodeGen

(*
    compile : TypedProg -> Instruction list

    (* for debugging *)
    compileExp : TypedExp
              -> SymTab<reg>
              -> reg
              -> Instruction list
*)

open AbSyn
open RiscV

exception MyError of string * Position

type VarTable = SymTab.SymTab<reg>

(* Name generator for labels and temporary symbolic registers *)
(* Example usage: val tmp = newName "tmp"  (* might produce "tmp_5_" *) *)

let mutable counter = 0

let newName base_name =
    counter <- counter + 1
    base_name + "_" + string counter + "_"

(* Symbolic register names will never make it to the assember *)
(* (except as comments), so no particular lexical restrictions. *)
let newReg reg_name = RS (newName ("_" + reg_name))

(* Must generate a lexically valid assembler label *)
let newLab lab_name = newName ("l." + lab_name)

(* Table storing all string literals from source program, with labels *)
(* given to them *)
let stringTable : ((addr*string) list) ref = ref []

(* Building a string constant, including length word *)
let buildString  ( label : addr
                 , str   : string
                 ) : Instruction list =
    [ ALIGN 2     (* means: word-align *)
    ; LABEL label (* pointer *)
    ; WORD [String.length str]
    ; ASCII str]  (* don't need a NUL terminator; have explicit length *)

(* Register division. Note that this doesn't quite follow the
   standard RISC-V conventions. *)
let minReg = 10      (* lowest allocatable register *)
let maxCaller = 17   (* highest caller-saves/argument register *)
let maxReg = 31      (* highest allocatable register *)

let Rhp = Rgp        (* register dedicated to heap pointer *)

(* Syscall numbers for MARS/RARS, to be put in Rsn *)
let sysPrintInt =     1  (* print integer in Ra0 *)
let sysPrintString =  4  (* print NUL-terminated string starting at Ra0 *)
let sysReadInt =      5  (* read integer into Rrv *)
let sysPrintChar =   11  (* print character in Ra0 *)
let sysReadChar =    12  (* read character into Rrv *)
let sysExit2 =       93  (* terminate execution (w/ exit number) *)
                         (* RARS docs say NOT to use sysExit = 10 *)

(* Determine the size of an element in an array based on its type *)
type ElemSize = ESByte | ESWord

let getElemSize (tp : Type) : ElemSize =
    match tp with
      | Char  -> ESByte
      | Bool  -> ESByte
      | _     -> ESWord

let elemSizeToInt (elmsz : ElemSize) : int =
    match elmsz with
      | ESByte -> 1
      | ESWord -> 4

(* Pick the correct instruction from the element size. *)
let Load elem_size = match elem_size with
                            | ESByte -> LBU
                            | ESWord -> LW

let Store elem_size = match elem_size with
                            | ESByte -> SB
                            | ESWord -> SW

(* Generates the code to check that the array index is within bounds *)
let checkBounds  ( arr_beg : reg
                 , ind_reg : reg
                 , (line : int, c : int)
                 ) : Instruction list =
    let size_reg = newReg "size"
    let err_lab  = newLab "error"
    let safe_lab = newLab "nonneg"
    [ BGE (ind_reg, Rzero, safe_lab)  (* check that ind_reg >= 0 *)
    ; LABEL err_lab
    ; LI (Ra0, line)
    ; LA (Ra1, "m.BadIndex")
    ; J "p.RuntimeError"
    ; LABEL safe_lab
    ; LW (size_reg, arr_beg, 0)
    ; BGE (ind_reg, size_reg, err_lab) (* check that ind_reg < size_reg *)
    ]

(* dynalloc(size_reg, place, ty) generates code for allocating arrays of heap
   memory by incrementing the Rhp register (heap pointer) by a number of words.
   The arguments for this function are as follows:

     size_reg: contains the logical array size (number of array elements)
     place: will contain the address of new allocation (old Rhp)
     ty: char/bool elements take 1 byte, int/array elements take 4 bytes
 *)
let dynalloc (size_reg : reg,
              place    : reg,
              ty       : Type     )
            : Instruction list =
    let tmp_reg = newReg "tmp"

    (* Use current Rhp as allocation address. *)
    let code1 = [ MV (place, Rhp) ]

    (* For char/bool: Align address to 4-byte boundary by rounding up. *)
    (*                (By adding 3 and clearing 2 LSBs *)
    (* For int and arrays: Multiply logical size by 4, no alignment. *)
    let code2 =
        match getElemSize ty with
          | ESByte ->  [ ADDI (tmp_reg, size_reg, 3)
                       ; ANDI (tmp_reg, tmp_reg, -4) ]
          | ESWord -> [ SLLI (tmp_reg, size_reg, 2) ]

    (* Make space for array size (+4). Increase Rhp. *)
    (* Save element count in header. *)
    let code3 =
        [ ADDI (tmp_reg, tmp_reg, 4)
        ; ADD (Rhp, Rhp, tmp_reg)
        ; SW (size_reg, place, 0) ]

    code1 @ code2 @ code3

(* code for saving and restoring callee-saves registers 
    (moved up in code to use with callClang func, used to be between compileDec and getArgs) *)
let rec stackSave (currentReg  : int)
                  (maxReg      : int)
                  (savecode    : Instruction list)
                  (restorecode : Instruction list)
                  (offset      : int)
                : (Instruction list * Instruction list * int) =
    if currentReg > maxReg
    then (savecode, restorecode, offset)  (* done *)
    else let noffset = offset-4 (* adjust offset *)
         // printfn "saving %d at %d\n" currentReg noffset
         stackSave (currentReg+1)
                   maxReg
                   (SW (RN currentReg, Rsp, noffset)
                    :: savecode) (* save register *)
                   (LW (RN currentReg, Rsp, noffset)
                    :: restorecode) (* restore register *)
                   noffset (* next offset *)

(* Save registers x28 - x31, since these are caller saved in clang, but not in Fasto compiler*)
let callClang (clangFunc : Instruction list) : Instruction list =
    let (savecode, restorecode, offset) = stackSave 28 31 [] [] 0
    [SW (Rra, Rsp, -4)]        (* save return addr *)
  @ savecode
  @ [ADDI (Rsp,Rsp, offset-4)] (* Adjust Rsp*)
  @ clangFunc                  (* Call function *)
  @ [ADDI (Rsp,Rsp,-offset+4)] (* Adjust Rsp*)
  @ restorecode
  @ [LW (Rra, Rsp, -4)]        (* restore return addr *)

let allocate (size_reg : reg,
              place    : reg,
              ty       : Type     )
            : Instruction list =

    (* get element size *)
    let size =
        match getElemSize ty with
          | ESByte ->  1
          | ESWord -> 4

    (* Move function arguments into registers*)
    let code1 = [ MV (Ra0, size_reg) 
                ; LI (Ra1, size)]

    (* Call allocate *)
    let code2 = callClang [JAL ("allocate", [Ra0; Ra1])]

    (* Move returned pointer to return register *)
    let code3 = [ MV (place, Rrv) ]

    code1 @ code2 @ code3

(* Passing arguments to called function *)
(* For each register 'r' in 'args', copy them to registers from
   'minReg' and counting up. Assumes that all args will fit into
   caller-saves registers for simplicity *)

let applyRegs  ( fid  : addr
               , args : reg list
               , place: reg
               , pos  : Position )
             : Instruction list =
    let regs_num = List.length args
    let caller_regs = List.map (fun n -> RN (n + minReg)) [0..regs_num-1]
    let move_code = List.map MV (List.zip caller_regs args)
    if regs_num > maxCaller - minReg
    then raise (MyError("Number of arguments passed to " + fid +
                        " exceeds number of caller registers", pos))
    else move_code @ [ JAL ("f."+fid,caller_regs); MV (place, Rrv) ]

(* Compile 'e' under bindings 'vtable', putting the result in register 'place'. *)
let rec compileExp  (e      : TypedExp)
                    (vtable : VarTable)
                    (place  : reg)
                  : Instruction list =
  match e with
  | Constant (IntVal n, pos) ->
      [ LI (place, n) ] (* assembler will generate appropriate
                           instruction sequence for any value n *)
  | Constant (BoolVal p, pos) ->
      (* TODO project task 1: represent `true`/`false` values as `1`/`0` *)
      if p then [LI (place, 1)]
      else [LI (place, 0)]

  | Constant (CharVal c, pos) ->

      [ LI (place, int c) ]

  (* Create/return a label here, collect all string literals of the program
     (in stringTable), and create them in the data section before the heap *)

  | StringLit (strLit, pos) ->
    (* Convert string literal into label; only use valid characters. *)
    let normalChars =
        String.map (fun c -> if System.Char.IsLetterOrDigit c then c else 'X') strLit
    let str0  = normalChars.Substring(0, min 7 normalChars.Length)
    let label = newName ("s."+str0)
    stringTable := (label, strLit) :: !stringTable
    [ LA (place, label)
    ; COMMENT (label + ": string \"" + toCString strLit + "\"") ]

  | Constant (ArrayVal (vs, tp), pos) ->
    (* Create corresponding ArrayLit expression to re-use code. *)
    let arraylit = ArrayLit (List.map (fun v -> Constant (v, pos)) vs, tp, pos)
    compileExp arraylit vtable place

  | ArrayLit (elems, tp, pos) ->
    let elem_size = getElemSize tp
    let size_reg  = newReg "size"
    let addr_reg  = newReg "addr"
    let tmp_reg   = newReg "tmp"

    (* Store size of literal in size_reg, dynamically allocate that. *)
    (* Let addr_reg contain the address for the first array element. *)
    let header  = [ LI (size_reg, List.length elems) ]  @
                  dynalloc (size_reg, place, tp) @
                  [ ADDI (addr_reg, place, 4) ]

    let compileElem elem_exp =
            let elem_code = compileExp elem_exp vtable tmp_reg
            elem_code @
            [ Store elem_size (tmp_reg, addr_reg, 0)
            ; ADDI (addr_reg, addr_reg, elemSizeToInt elem_size) ]

    let elems_code = List.collect compileElem elems
    header @ elems_code

  | Var (vname, pos) ->
      match SymTab.lookup vname vtable with
        | None          -> raise (MyError ("Name " + vname + " not found", pos))
        | Some reg_name -> [MV (place, reg_name)]

  | Plus (e1, e2, pos) ->
      let t1 = newReg "plus_L"
      let t2 = newReg "plus_R"
      let code1 = compileExp e1 vtable t1
      let code2 = compileExp e2 vtable t2
      code1 @ code2 @ [ADD (place,t1,t2)]

  | Minus (e1, e2, pos) ->
      let t1 = newReg "minus_L"
      let t2 = newReg "minus_R"
      let code1 = compileExp e1 vtable t1
      let code2 = compileExp e2 vtable t2
      code1 @ code2 @ [SUB (place,t1,t2)]

  | Times (e1, e2, pos) ->
      let t1 = newReg "times_L"
      let t2 = newReg "times_R"
      let code1 = compileExp e1 vtable t1
      let code2 = compileExp e2 vtable t2
      code1 @ code2 @ [MUL (place,t1,t2)]

  | Divide (e1, e2, pos) ->
      let t1 = newReg "divide_L"
      let t2 = newReg "divide_R"
      let code1 = compileExp e1 vtable t1
      let code2 = compileExp e2 vtable t2

      let safe_lab = newLab "safe"
      let line, _ = pos
      let checkDivZero = [ BNE (t2, Rzero, safe_lab)
                         ; LI (Ra0, line)
                         ; LA (Ra1, "m.DivZero")
                         ; J "p.RuntimeError"
                         ; LABEL (safe_lab)
                         ]
    
      code1 @ code2 @ checkDivZero @ [DIV (place,t1,t2)]

  | Not (e1, pos) ->
      let t1 = newReg "bool_not"
      let code = compileExp e1 vtable t1

      code @ [XORI (place,t1,1)]

  | Negate (e1, pos) ->
      let t1 = newReg "int_neg"
      let code = compileExp e1 vtable t1

      code @ [SUB (place, Rzero, t1)]

  | Let (dec, e1, pos) ->
      let (code1, vtable1) = compileDec dec vtable
      let code2 = compileExp e1 vtable1 place
      code1 @ code2

  | If (e1, e2, e3, pos) ->
      let thenLabel = newLab "then"
      let elseLabel = newLab "else"
      let endLabel = newLab "endif"
      let code1 = compileCond e1 vtable thenLabel elseLabel
      let code2 = compileExp e2 vtable place
      let code3 = compileExp e3 vtable place
      code1 @ [LABEL thenLabel] @ code2  @
        [ J endLabel; LABEL elseLabel ] @
        code3 @ [LABEL endLabel]

  | Apply (f, args, pos) ->
      (* Convention: args in regs (minReg..maxCaller), result in Rrv *)
      let compileArg arg =
            let arg_reg = newReg "arg"
            (arg_reg, compileExp arg vtable arg_reg)
      let (arg_regs, argcode) = List.unzip (List.map compileArg args)
      let applyCode = applyRegs(f, arg_regs, place, pos)
      List.concat argcode @  (* Evaluate args *)
                  applyCode  (* Jump to function and store result in place *)

  (* dirty I/O. Read and Write: supported for basic types: Int, Char,
     via system calls. Write of Bool and Array(Chars) is also
     supported. The others are the user's responsibility.
  *)
  | Read(tp, pos) ->
      match tp with
        | Int  -> [ JAL ("p.getint", [])
                  ; MV (place, Rrv)
                  ]
        | Char -> [ JAL ("p.getchar", [])
                  ; MV (place, Rrv)
                  ]
        | _ -> raise (MyError("Read of an unsupported type: " + ppType tp, pos))

  | Write(e, tp, pos) ->
    let tmp = newReg "tmp"
    let codeexp = compileExp e vtable tmp @ [ MV (place, tmp) ]
    match tp with
      | Int  -> codeexp @ [ MV(Ra0,place); JAL("p.putint", [Ra0]) ]
      | Char -> codeexp @ [ MV(Ra0,place); JAL("p.putchar",[Ra0]) ]
      | Bool ->
          let tlab = newLab "wBoolF"
          codeexp @
           [ LA (Ra0, "s.true")
           ; BNE (place, Rzero, tlab)
           ; LA (Ra0, "s.false")
           ; LABEL tlab
           ; JAL ("p.putstring", [Ra0])
           ]

      | Array Char ->
           codeexp @ [ MV (Ra0, tmp)
                     ; JAL("p.putstring", [Ra0]) ]
      | _ -> raise (MyError("Write of an unsupported type: " + ppType tp, pos))

  (* Comparison checking, later similar code for And and Or. *)
  | Equal (e1, e2, pos) ->
      let t1 = newReg "eq_L"
      let t2 = newReg "eq_R"
      let code1 = compileExp e1 vtable t1
      let code2 = compileExp e2 vtable t2
      let falseLabel = newLab "false"
      code1 @ code2 @
       [ LI (place, 0)
       ; BNE (t1,t2,falseLabel)
       ; LI (place, 1)
       ; LABEL falseLabel
       ]

  | Less (e1, e2, pos) ->
      let t1 = newReg "lt_L"
      let t2 = newReg "lt_R"
      let code1 = compileExp e1 vtable t1
      let code2 = compileExp e2 vtable t2
      code1 @ code2 @ [SLT (place,t1,t2)]

  | And (e1, e2, pos) ->
      let t1 = newReg "and_L"
      let t2 = newReg "and_R"
      let code1 = compileExp e1 vtable t1
      let code2 = compileExp e2 vtable t2

      let falseLabel = newLab "false"
      let endLabel   = newLab "end"

      let check x    = [ BEQ (x, Rzero, falseLabel)]

      let determine  = [ LI (place, 1)
                       ; J (endLabel)
                       ; LABEL falseLabel
                       ; LI (place, 0)
                       ; LABEL endLabel
                       ]

      code1 @ check t1 @ code2 @ check t2 @ determine

  | Or (e1, e2, pos) ->
      let t1 = newReg "or_L"
      let t2 = newReg "or_R"
      let code1 = compileExp e1 vtable t1
      let code2 = compileExp e2 vtable t2

      let trueLabel  = newLab "true"
      let endLabel   = newLab "end"

      let check x    = [ BNE (x, Rzero, trueLabel)]

      let determine  = [ LI (place, 0)
                       ; J (endLabel)
                       ; LABEL trueLabel
                       ; LI (place, 1)
                       ; LABEL endLabel
                       ]

      code1 @ check t1 @ code2 @ check t2 @ determine

  (* Indexing:
     1. generate code to compute the index
     2. check index within bounds
     3. add the start address with the index
     4. get the element at that address
   *)
  | Index (arr_name, i_exp, ty, pos) ->
      let ind_reg  = newReg "arr_ind"
      let ind_code = compileExp i_exp vtable ind_reg
      let arr_reg  = newReg "arr_data"

      (* Let arr_reg be the start of the data segment *)
      let arr_beg =
            match SymTab.lookup arr_name vtable with
              | None -> raise (MyError ("Name " + arr_name + " not found", pos))
              | Some reg_name -> reg_name
      let init_code = [ ADDI (arr_reg, arr_beg, 4) ]

      (* code to check bounds *)
      let check_code = checkBounds(arr_beg, ind_reg, pos)

      (* for INT/ARRAY: ind *= 4 else ind is unchanged *)
      (* array_var += ind; place = *array_var *)
      let load_code =
         match getElemSize ty with
           | ESByte ->  [ ADD (arr_reg, arr_reg, ind_reg)
                        ; LBU (place, arr_reg, 0) ]
           | ESWord ->  [ SLLI (ind_reg, ind_reg, 2)
                        ; ADD (arr_reg, arr_reg, ind_reg)
                        ; LW (place, arr_reg, 0) ]
      ind_code @ init_code @ check_code @ load_code

  (* Second-Order Array Combinators (SOACs):
     iota, map, reduce
  *)
  | Iota (n_exp, (line, _)) ->
      let size_reg = newReg "size"
      let n_code = compileExp n_exp vtable size_reg
      (* size_reg is now the integer n. *)

      (* Check that array size N >= 0:
         if N >= 0 then jumpto safe_lab
         jumpto "_IllegalArrSizeError_"
         safe_lab: ...
      *)
      let safe_lab = newLab "safe"
      let checksize = [ BGE (size_reg, Rzero, safe_lab)
                      ; LI (Ra0, line)
                      ; LA (Ra1, "m.BadSize")
                      ; J "p.RuntimeError"
                      ; LABEL (safe_lab)
                      ]

      let addr_reg = newReg "addr"
      let i_reg = newReg "i"
      let init_regs = [ ADDI (addr_reg, place, 4)
                      ; MV (i_reg, Rzero) ]
      (* addr_reg is now the position of the first array element. *)

      (* Run a loop.  Keep jumping back to loop_beg until it is not the
         case that i_reg < size_reg, and then jump to loop_end. *)
      let loop_beg = newLab "loop_beg"
      let loop_end = newLab "loop_end"
      let loop_header = [ LABEL (loop_beg)
                        ; BGE (i_reg, size_reg, loop_end)
                        ]
      (* iota is just 'arr[i] = i'.  arr[i] is addr_reg. *)
      let loop_iota   = [ SW (i_reg, addr_reg, 0) ]
      let loop_footer = [ ADDI (addr_reg, addr_reg, 4)
                        ; ADDI (i_reg, i_reg, 1)
                        ; J loop_beg
                        ; LABEL loop_end
                        ]
      n_code
       @ checksize
// Testing allocate, replace with the following for tests to run propperly: 
//     @ dynalloc (size_reg, place, Int)
       @ allocate (size_reg, place, Int) 
       @ init_regs
       @ loop_header
       @ loop_iota
       @ loop_footer

  | Length(arr, _, pos) ->
      let arr_addr = newReg "len_arr"
      let code1    = compileExp arr vtable arr_addr
      code1 @ [ LW(place, arr_addr, 0) ]

  | Map (farg, arr_exp, elem_type, ret_type, pos) ->
      let size_reg = newReg "size" (* size of input/output array *)
      let arr_reg  = newReg "arr"  (* address of array *)
      let elem_reg = newReg "elem" (* address of current element *)
      let res_reg = newReg "res"
      let arr_code = compileExp arr_exp vtable arr_reg

      let get_size = [ LW (size_reg, arr_reg, 0) ]

      let addr_reg = newReg "addrg" (* address of element in new array *)
      let i_reg = newReg "i"
      let init_regs = [ ADDI (addr_reg, place, 4)
                      ; MV (i_reg, Rzero)
                      ; ADDI (elem_reg, arr_reg, 4)
                      ]
      let loop_beg = newLab "loop_beg"
      let loop_end = newLab "loop_end"
      let loop_header = [ LABEL (loop_beg)
                        ; BGE (i_reg, size_reg, loop_end)
                        ]
      (* map is 'arr[i] = f(old_arr[i])'. *)
      let src_size = getElemSize elem_type
      let dst_size = getElemSize ret_type
      let loop_map =
             [ Load src_size (res_reg, elem_reg, 0)
             ; ADDI (elem_reg, elem_reg, elemSizeToInt src_size)
             ]
             @ applyFunArg(farg, [res_reg], vtable, res_reg, pos)
             @
             [ Store dst_size (res_reg, addr_reg, 0)
             ; ADDI (addr_reg, addr_reg, elemSizeToInt dst_size)
             ]

      let loop_footer =
              [ ADDI (i_reg, i_reg, 1)
              ; J loop_beg
              ; LABEL loop_end
              ]
      arr_code
       @ get_size
       @ dynalloc (size_reg, place, ret_type)
       @ init_regs
       @ loop_header
       @ loop_map
       @ loop_footer

  (* reduce(f, acc, {x1, x2, ...xn}) = f(f(f(acc,x1),x2),...xn) *)
  | Reduce (binop, acc_exp, arr_exp, tp, pos) ->
      let arr_reg  = newReg "arr"   (* address of array *)
      let size_reg = newReg "size"  (* size of input array *)
      let i_reg    = newReg "ind_var"   (* loop counter *)
      let tmp_reg  = newReg "tmp"   (* several purposes *)
      let loop_beg = newLab "loop_beg"
      let loop_end = newLab "loop_end"

      let arr_code = compileExp arr_exp vtable arr_reg
      let header1 = [ LW(size_reg, arr_reg, 0) ]

      (* Compile initial value into place (will be updated below) *)
      let acc_code = compileExp acc_exp vtable place

      (* Set arr_reg to address of first element instead. *)
      (* Set i_reg to 0. While i < size_reg, loop. *)
      let loop_code =
              [ ADDI (arr_reg, arr_reg, 4)
              ; MV (i_reg, Rzero)
              ; LABEL (loop_beg)
              ; BGE (i_reg, size_reg, loop_end)
              ]
      (* Load arr[i] into tmp_reg *)
      let elem_size = getElemSize tp
      let load_code =
        [ Load elem_size (tmp_reg, arr_reg, 0)
        ; ADDI (arr_reg, arr_reg, elemSizeToInt elem_size)
        ]
      (* place := binop(place, tmp_reg) *)
      let apply_code =
            applyFunArg(binop, [place; tmp_reg], vtable, place, pos)

      arr_code @ header1 @ acc_code @ loop_code @ load_code @ apply_code @
         [ ADDI(i_reg, i_reg, 1)
         ; J loop_beg
         ; LABEL loop_end
         ]

  | Replicate (n_exp, a_exp, tp, (line, _)) ->
      (* load array size n into size_reg *)
      let size_reg = newReg "size"
      let n_code   = compileExp n_exp vtable size_reg

      (* Check that N >= 0 *)
      let safe_lab = newLab "safe"
      let checksize = [ BGE (size_reg, Rzero, safe_lab)
                      ; LI (Ra0, line)
                      ; LA (Ra1, "m.BadSize")
                      ; J "p.RuntimeError"
                      ; LABEL (safe_lab)
                      ]

      (* Load the expression to be replicated into register elm_reg*)
      let elm_reg  = newReg "element"
      let elm_code = compileExp a_exp vtable elm_reg

      (* Call to dynalloc, loads address of header into place *)

      (* Load the position of the first array element into addr_reg *)
      let addr_reg = newReg "addr"
      let i_reg = newReg "i"
      let init_regs   = [ ADDI (addr_reg, place, 4)
                        ; MV (i_reg, Rzero) ]

      (* Create while (i < n) loop *)
      let loop_beg = newLab "loop_beg"
      let loop_end = newLab "loop_end"
      let loop_header = [ LABEL (loop_beg)
                        ; BGE (i_reg, size_reg, loop_end)
                        ]
                        
      (* For the loop body, we need to know the size of the replicate element *)
      let elmSize = getElemSize tp
      let loop_replicate = [ Store elmSize (elm_reg, addr_reg, 0) ]

      (* In the loop footer we update i and the address register *)
      let loop_footer = [ ADDI (addr_reg, addr_reg, elemSizeToInt elmSize)
                        ; ADDI (i_reg, i_reg, 1)
                        ; J loop_beg
                        ; LABEL loop_end
                        ]
      n_code
       @ checksize
       @ elm_code
       @ dynalloc (size_reg, place, tp)
       @ init_regs
       @ loop_header
       @ loop_replicate
       @ loop_footer

  | Filter (pred, arr_exp, tp, pos) ->
      let size_reg = newReg "size"    (* size of input and output array *)
      let arr_reg  = newReg "arr"     (* address of array *)
      let elem_reg = newReg "elem"    (* address of current element *)
      let pred_reg = newReg "pred"    (* result of predicate*)
      let res_reg  = newReg "res"
      let cnt_reg  = newReg "counter" (* counts elements in return array *)
      let arr_code = compileExp arr_exp vtable arr_reg

      let get_size = [ LW (size_reg, arr_reg, 0) ]

      (* Call to dynalloc, loads address of return header into place.
         We comprise by alocating more space than needed, but saving runtime *)

      let addr_reg = newReg "addrg" (* address of element in new array *)
      let i_reg = newReg "i"
      let init_regs = [ ADDI (addr_reg, place, 4)
                      ; MV (i_reg, Rzero)
                      ; MV (cnt_reg, Rzero)
                      ; ADDI (elem_reg, arr_reg, 4)
                      ]

      let loop_beg   = newLab "loop_beg"
      let loop_end   = newLab "loop_end"
      let pred_false = newLab "pred_false"
      let loop_header = [ LABEL (loop_beg)
                        ; BGE (i_reg, size_reg, loop_end)
                        ]
      
      (* loop body *)
      let elm_size = getElemSize tp
      let loop_filter =
             [ Load elm_size (res_reg, elem_reg, 0)
             ; ADDI (elem_reg, elem_reg, elemSizeToInt elm_size)
             ]
             @ applyFunArg(pred, [res_reg], vtable, pred_reg, pos)
             @
             [ BEQ (pred_reg, Rzero, pred_false)
             ; Store elm_size (res_reg, addr_reg, 0)
             ; ADDI (cnt_reg, cnt_reg, 1)
             ; ADDI (addr_reg, addr_reg, elemSizeToInt elm_size)
             ; LABEL (pred_false)
             ]

      let loop_footer =
             [ ADDI (i_reg, i_reg, 1)
             ; J loop_beg
             ; LABEL loop_end
             ]
      arr_code
       @ get_size
       @ dynalloc (size_reg, place, tp)
       @ init_regs
       @ loop_header
       @ loop_filter
       @ loop_footer 
       @ [SW (cnt_reg, place, 0)] (* change size of array *)

  | Scan (binop, acc_exp, arr_exp, tp, pos) ->
      let arr_reg  = newReg "arr"   (* address of array *)
      let acc_reg  = newReg "acc"   (* accumulator *)
      let elm_reg  = newReg "elm"   (* output array address *)
      let size_reg = newReg "size"  (* size of input array *)
      let i_reg    = newReg "ind_var"   (* loop counter *)
      let tmp_reg  = newReg "tmp"   (* several purposes *)
      let loop_beg = newLab "loop_beg"
      let loop_end = newLab "loop_end"

      (* compile function arguments into registers *)
      let arr_code = compileExp arr_exp vtable arr_reg
      let acc_code = compileExp acc_exp vtable acc_reg

      let allocate = [ LW (size_reg, arr_reg, 0) ] @ dynalloc (size_reg, place, tp)

      (* initialize registers *)
      let init_regs = [ ADDI (arr_reg, arr_reg, 4)
                      ; ADDI (elm_reg, place, 4)
                      ; MV (i_reg, Rzero)
                      ]

      (* Start loop *)
      let header = [ LABEL (loop_beg)
                   ; BGE (i_reg, size_reg, loop_end)
                   ]

      (* Load arr[i] *)
      let elem_size = getElemSize tp
      let elem_int  = elemSizeToInt elem_size
      let load_code = [ Load elem_size (tmp_reg, arr_reg, 0)
                      ; ADDI (arr_reg, arr_reg, elem_int)
                      ]

      (* Update accumulator *)
      let apply_code = applyFunArg(binop, [acc_reg; tmp_reg], vtable, acc_reg, pos)

      let footer = [ Store elem_size (acc_reg, elm_reg, 0)
                   ; ADDI (elm_reg, elm_reg, elem_int)
                   ; ADDI (i_reg, i_reg, 1)
                   ; J loop_beg
                   ; LABEL loop_end
                   ]

      arr_code 
       @ acc_code 
       @ allocate
       @ init_regs
       @ header 
       @ load_code 
       @ apply_code 
       @ footer


and applyFunArg ( ff     : TypedFunArg
                , args   : reg list
                , vtable : VarTable
                , place  : reg
                , pos    : Position
                ) : Instruction list =
  match ff with
    | FunName s ->
          let tmp_reg = newReg "tmp"
          applyRegs(s, args, tmp_reg, pos) @ [MV(place, tmp_reg)]

    | Lambda (_, parms, body, lampos) ->
          let rec bindParams parms args vtable' =
              match (parms, args) with
                | (Param (pname,_)::parms', arg::args') ->
                      bindParams parms' args' (SymTab.bind pname arg vtable')
                | _ -> vtable'
          let vtable' = bindParams parms args vtable
          let t = newReg "fun_arg_res"
          compileExp body vtable' t @ [ MV(place, t) ]

(* compile condition *)
and compileCond  (c      : TypedExp)
                 (vtable : VarTable)
                 (tlab   : addr)
                 (flab   : addr)
               : Instruction list =
  let t1 = newReg "cond"
  let code1 = compileExp c vtable t1
  code1 @ [BNE (t1, Rzero, tlab); J flab]

(* compile let declaration *)
and compileDec  (dec : TypedDec)
                (vtable : VarTable)
              : (Instruction list * VarTable) =
      let (Dec (s,e,pos)) = dec
      let t = newReg ("let_" + s)
      let code = compileExp e vtable t
      let new_vtable = SymTab.bind s t vtable
      (code, new_vtable)

(* add function arguments to symbol table *)
and getArgs  (parms   : Param list)
             (vtable  : VarTable)
             (nextReg : int)
           : (Instruction list * VarTable) =
  match parms with
    | [] -> ([], vtable)
    | (Param (v,_)::vs) ->
       if nextReg > maxCaller
       then raise (MyError ("Passing too many arguments!", (0,0)))
       else let vname = newReg ("param_" + v)
            let vtable1 = SymTab.bind v vname vtable (*   (v,vname)::vtable   *)
            let (code2,vtable2) = getArgs vs vtable1 (nextReg + 1)
            ([MV (vname, RN nextReg)] @ code2, vtable2)

(* compile function declaration *)
and compileFun (fundec : TypedFunDec) : Prog =
    let (FunDec (fname, resty, args, exp, (line,col))) = fundec
    (* make a vtable from bound formal parameters,
         then evaluate expression in this context, return it *)
    (* arguments passed in registers, "move" into local vars. *)
    let (argcode, vtable_local) = getArgs args (SymTab.empty ()) minReg
    (* function return value *)
    let rtmp = newReg (fname + "res")
    let returncode  = [MV (Rrv,rtmp)] (* move return val to Rrv *)
    let body = compileExp exp vtable_local rtmp (* target expr *)
    let (body1, _, maxr, spilled) =
            RegAlloc.registerAlloc   (* call register allocator *)
                       (argcode @ body @ returncode)
                       (Set.singleton (Rrv)) minReg maxCaller maxReg 0
    let (savecode, restorecode, offset) = (* save/restore callee-saves *)
            stackSave (maxCaller+1) maxr [] [] (-4(*Rra*) )
    [COMMENT ("Function " + fname);
     LABEL ("f."+fname);  (* function label, prevent clashes with internals *)
     SW (Rra, Rsp, -4)]   (* save return address *)
  @ savecode              (* save callee-saves registers *)
  @ [ADDI (Rsp,Rsp,offset-4*spilled)]   (* Rsp adjustment *)
  @ body1                 (* code for function body *)
  @ [ADDI (Rsp,Rsp,-offset+4*spilled)]  (* move Rsp up *)
  @ restorecode           (* restore callee-saves registers *)
  @ [LW (Rra, Rsp, -4);   (* restore return addr *)
     JR (Rra, [])]        (* return *)

(* compile program *)
let compile (funs : TypedProg) : Instruction list =
  stringTable := [("s.true","true"); ("s.false","false")]
  let funsCode = List.concat (List.map compileFun funs)
  let stringdata = List.map buildString (!stringTable)
  let main_prog =
      [TEXT "0x00400000"]
      (* initialize heap pointer *)
    @ [LA (Rhp, "d.heap")]
      (* call main function *)
    @ [JAL ("f.main",[])]
    @ (* terminate execution *)
      [JAL ("p.stop", [])]
      (* code for user functions *)
    @ [COMMENT "User functions"]
    @ funsCode
      (* pre-defined ord: char -> int and chr: int -> char *)
    @ [COMMENT "Library functions in Fasto namespace"]
    @ [LABEL "f.ord"; (* char returned unmodified, interpreted as int *)
       MV (Rrv, RN minReg);
       JR (Rra, [Rrv]);
       LABEL "f.chr"; (* int values are truncated to 8 LSBs *)
       ANDI (Rrv, RN minReg, 255);
       JR (Rra, [Rrv])]
      (* built-in read and write procedures *)
    @ [COMMENT "Internal procedures (for syscalls, etc.)"]
    @ [LABEL "p.putint";
       LI (Rsn, sysPrintInt);
       ECALL;              (* int to print is already in a0 *)
       LI (Rsn, sysPrintString);
       LA (Ra0,"m.space");
       ECALL;              (* follow with a space *)
       JR (Rra,[])]
    @ [LABEL "p.getint";
       LI (Rsn,sysReadInt);
       ECALL;
       JR (Rra,[Rrv])]
    @ [LABEL "p.putchar";
       LI (Rsn, sysPrintChar);
       ECALL;              (* char to print is already in a0 *)
       LI (Rsn, sysPrintString);
       LA (Ra0, "m.space"); (* follow with a space *)
       ECALL;
       JR (Rra,[])
      ]
    @ [LABEL "p.getchar";
       LI (Rsn, sysReadChar);
       ECALL;
       JR (Rra,[Rrv])
      ]
    @ [LABEL "p.putstring";
       LW  (Rt2, Ra0, 0);           (* t2 := M[a0], size of string *)
       ADDI(Rt1, Ra0, 4);           (* t1 := a0 + 4, addr of 1st char      *)
       ADD (Rt2, Rt1, Rt2);         (* t2 := t1 + t2, addr after last char *)
       LI  (Rsn, sysPrintChar);     (* set up syscall # *)
       LABEL "l.ps_begin";
       BGE (Rt1, Rt2, "l.ps_done"); (* while (t1 < t2) { *)
       LBU (Ra0, Rt1, 0);           (*   a0 := M[t1]     *)
       ECALL;                       (*   printchar(a0)   *)
       ADDI(Rt1, Rt1, 1);           (*   t1 = t1 + 1     *)
       J "l.ps_begin";              (* }                 *)
       LABEL "l.ps_done";
       JR (Rra,[])
      ]
    (* Stop execution (normally or due to an error *)
    @ [LABEL "p.stop"
       LI (Rsn, sysExit2);
       LI (Ra0, 0);                 (* successful exit *)
       ECALL                        (* does not return! *)
      ]
    @ (* Fixed code for reporting runtime errors.
         expects source line number in Ra0, pointer to error message in Ra1 *)
      [LABEL "p.RuntimeError";
       MV (Rt1, Ra0); // stash a0 for later
       LI (Rsn, sysPrintString);
       LA (Ra0, "m.RunErr"); (* intro text *)
       ECALL;
       LI (Rsn, sysPrintInt);
       MV (Ra0, Rt1); (* line number *)
       ECALL;
       LI (Rsn, sysPrintString);
       LA (Ra0, "m.colon_space");
       ECALL;
       MV (Ra0, Ra1); (* message string *)
       ECALL;
       LA (Ra0, "m.nl");
       ECALL;
       JAL ("p.stop", [])] (* doesn't return *)
    @
       [DATA "";
        COMMENT "Fixed strings for runtime I/O";
        LABEL "m.RunErr";
        ASCIIZ "Runtime error at line ";
        LABEL "m.colon_space";
        ASCIIZ ": ";
        LABEL "m.nl";
        ASCIIZ "\n";
        LABEL "m.space";
        ASCIIZ " "]
    @
       [COMMENT "Message strings for specific errors";
        LABEL "m.BadSize";
        ASCIIZ "negative array size";
        LABEL "m.BadIndex";
        ASCIIZ "array index out of bounds";
        LABEL "m.DivZero";
        ASCIIZ "division by zero"
       ]
    @ (* Collected string literals from Fasto program *)
       (COMMENT "String literals (including lengths) from program" ::
        List.concat stringdata)
      (* Heap (to allocate arrays in, word-aligned) *)
      (* Note: no overflow checks... *)
    @ [ALIGN 2;
       COMMENT "Space for Fasto heap";
       LABEL "d.heap";
       SPACE 100000]
  main_prog
