(* Mini Fasto for testing and experimental purposes*)

(* Simple abstract syntax, subset of AbSyn.fs, only int, array, let and iotas*)
type Type =
    Int
  | Array of Type

type Value =
    IntVal   of int
  | ArrayVal of Value list * Type

(* Should we have If statements? *)
type Exp<'T> =
    Constant  of Value
  | ArrayLit  of Exp<'T> list * 'T
  | Plus  of Exp<'T> * Exp<'T>
  | Minus of Exp<'T> * Exp<'T>
  | Times  of Exp<'T> * Exp<'T>
  | Divide of Exp<'T> * Exp<'T> 
  | Let   of Dec<'T> * Exp<'T>
  | Index of string * Exp<'T> * 'T
  | Length of Exp<'T> * 'T
  | Iota   of Exp<'T>

and Dec<'T> =
    Dec of string * Exp<'T>

type UntypedExp = Exp<unit>
type TypedExp = Exp<Type>

(* ----------------------------------------------------------------------- *)
(* Absyn of pseudo-Risc-V *)
type reg  = RN of int | RS of string
type imm  = int
type addr = string

type Instruction =
    LABEL   of addr     (* Define a label (for code or data) *)
  | COMMENT of string   (* Add a comment in the assembler code *)

  (* Data movement *)
  | LA   of reg*addr    (* LA($rd,addr):    $rd = addr (label) pseudoinstr] *)
  | LI   of reg*imm     (* LI($rd,imm):     $rd = imm  [pseudoinstr] *)
  | MV   of reg*reg     (* MV($rd,$rs):     $rd = $rs  [pseudoinstr] *)
  // could add: LUI (but subsumed by LI)

  | LW   of reg*reg*imm (* LW($rd,$rs,imm): $rd = *(int * )($rs + imm) *)
  | LB   of reg*reg*imm (* LB($rd,$rs,imm): $rd = *(signed char * )($rs + imm) *)
  | LBU  of reg*reg*imm (* LBU($rd,$rs,imm): $rd = *(unsigned char * )($rs + imm) *)
  | SW   of reg*reg*imm (* SW($rw,$rm,imm): *(int * )($rm + imm) = $rw *)
  | SB   of reg*reg*imm (* SB($rb,$rm,imm): *(char * )($rm + imm) = $rb *)
  // could add: LH, SH, LHU

  (* Arithmetic instructions *)
  | ADD  of reg*reg*reg (* ADD($rd,$rs,$rt): $rd = $rs + $rt *)
  | ADDI of reg*reg*imm (* ADDI($rd,$rs,imm): $rd = $rs + imm *)
  | SUB  of reg*reg*reg (* SUB($rd,$rs,$rt): $rd = $rs - $rt *)
  | MUL  of reg*reg*reg (* MUL($rd,$rs,$rt): $rd = $rs * $rt *)
  | DIV  of reg*reg*reg (* DIV($rd,$rs,$rt): $rd = $rd / $rs *)
  // could add: MULH, MULHSU, MULHU, DIVU, REM, REMU

  (* Bitwise operations *)
  | AND  of reg*reg*reg (* AND($rd,$rs,$rt):  $rd = $rs & $rt *)
  | ANDI of reg*reg*imm (* ANDI($rd,$rs,imm): $rd = $rs & imm *)
  | OR   of reg*reg*reg (* OR($rd,$rs,$rt):   $rd = $rs | $rt *)
  | ORI  of reg*reg*imm (* ORI($rd,$rs,imm):  $rd = $rs | imm *)
  | XOR  of reg*reg*reg (* XOR($rd,$rs,$rt):  $rd = $rs ^ $rt *)
  | XORI of reg*reg*imm (* XORI($rd,$rs,imm): $rd = $rs ^ imm *)

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

let remove n (SymTab stab) =
    SymTab (List.filter (fun (x, _) -> x <> n) stab)

type VarTable = SymTab<reg>

(* ----------------------------------------------------------------------- *)
(* Code generator *)
let rec compileExp  (e      : TypedExp)
                    (vtable : VarTable)
                    (place  : reg)
                  : Instruction list =
  match e with
  | Constant (IntVal n) ->
    [LI (place, n)]
  | Constant (ArrayVal (vs, tp)) ->
    let arraylit = ArrayLit(List.map (fun v -> Constant (v)) vs, tp)
    compileExp arraylit vtable
  | ArrayLit (elems, tp) ->
    

(*
    Constant  of Value
  | ArrayLit  of Exp<'T> list * 'T
  | Plus  of Exp<'T> * Exp<'T>
  | Minus of Exp<'T> * Exp<'T>
  | Times  of Exp<'T> * Exp<'T>
  | Divide of Exp<'T> * Exp<'T> 
  | Let   of Dec<'T> * Exp<'T>
  | Index of string * Exp<'T> * 'T
  | Length of Exp<'T> * 'T
  | Iota   of Exp<'T>
*)