open! Containers

type funcAddr = int
type tableAddr = int
type memAddr = int
type typeIdx = int
type funcIdx = int
type tableIdx = int
type memIdx = int
type globalIdx = int
type localIdx = int
type labelIdx = int

(* ## Types *)

(* valtype::=i32 | i64 | f32 | f64 *)
type valtype = TI32 | TI64 | TF32 | TF64

(* resulttype::=[valtype?] *)
type resulttype = valtype option list

(* functype::=[vec(valtype)]->[vec(valtype)] *)
type functype = valtype list * valtype list

(* memtype::={min u32, max u32?} *)
type memtype = int * int option

(* tabletype::={min u32, max u32?} elemtype *)
type tabletype = int * int option * elemtype

and elemtype = FUNCREF

(* mut ::= const | var *)
(* globaltype ::= mut valtype *)
type globaltype = mut * valtype

and mut = CONSTANCE | VARIABLE

type externtype =
    | FUNC of functype
    | TABLE of tabletype
    | MEM of memtype
    | GLOBAL of globaltype

(* ## Instructions *)

(* memarg::={offset u32,align u32} *)
type memarg = int * int

type unop =
    | I_CLZ
    | I_CTZ
    | I_POPCONT
    | F_ABS
    | F_NEG
    | F_SQRT
    | F_CEIL
    | F_FLOOR
    | F_TRUNC
    | F_NEAREST

type binop =
    | I_ADD
    | I_SUB
    | I_MUL
    | I_DIV_S
    | I_DIV_U
    | I_REM_S
    | I_REM_U
    | I_AND
    | I_OR
    | I_XOR
    | I_SHL
    | I_SHR_S
    | I_SHR_U
    | I_ROTL
    | I_ROTR
    | F_ADD
    | F_SUB
    | F_MUL
    | F_DIV
    | F_MIN
    | F_MAX
    | F_COPYSIGN

type testop = I_EQZ

type relop =
    | I_EQ
    | I_NE
    | I_LT_S
    | I_LT_U
    | I_GT_S
    | I_GT_U
    | I_LE_S
    | I_LE_U
    | I_GE_S
    | I_GE_U
    | F_EQ
    | F_NE
    | F_LT
    | F_GT
    | F_LE
    | F_GE

type cvtop =
    | CVT_WRAP
    | CVT_EXTEND_S
    | CVT_EXTEND_U
    | CVT_TRUNC_S
    | CVT_TRUNC_U
    | CVT_CONVERT_S
    | CVT_CONVERT_U
    | CVT_DEMOTE
    | CVT_PROMOTE
    | CVT_REINTERPRET

type value =
    | I32 of Int32.t
    | I64 of Int64.t
    | F32 of Float32.t
    | F64 of Float64.t

type instr =
    (* Numeric Instructions *)
    | Const of value
    | UnOp of unop * valtype
    | BinOp of binop * valtype
    | TestOp of testop * valtype
    | RelOp of relop * valtype
    | CvtOp of valtype * cvtop * valtype (*i32.wrap_i64 -> I32.CVT_WRAP_I64*)
    (* Parametric Instructions *)
    | Drop
    | Select
    (* Variable Instructions *)
    | LocalGet of localIdx
    | LocalSet of localIdx
    | LocalTee of localIdx
    | GlobalGet of globalIdx
    | GlobalSet of globalIdx
    (* Memory Instructions *)
    | Load of memarg * valtype (* inn.load | fnn.load *)
    | Store of memarg * valtype (* inn.store | fnn.store *)
    | Load8S of memarg * valtype (* inn.load8_sx *)
    | Load8U of memarg * valtype (* inn.load8_sx *)
    | Load16S of memarg * valtype (* inn.load16_sx *)
    | Load16U of memarg * valtype (* inn.load16_sx *)
    | Load32S of memarg (* i64.load32_sx *)
    | Load32U of memarg (* i64.load32_sx *)
    | Store8 of memarg * valtype (* inn.store8 *)
    | Store16 of memarg * valtype (* inn.store16 *)
    | Store32 of memarg (* i64.store32 *)
    | MemortSize
    | MemortGrow
    (* Control Instructions *)
    | Nop
    | Unreachable
    | Block of resulttype * instr list
    | Loop of resulttype * instr list
    | If of resulttype * instr list * instr list
    | Br of labelIdx
    | BrIf of labelIdx
    | BrTable of labelIdx list * labelIdx
    | Return
    | Call of funcIdx
    | CallIndirect of typeIdx
    (* Administrative Instructions *)
    | Trap
    | Invoke of funcAddr
    | InitElem of tableAddr * int * funcIdx
    | InitData of memAddr * int * char
    | Label of instr list
    | Frame of instr list

type expr = instr list

(* ## Modules *)

(* func::={type typeidx,locals vec(valtype),body expr} *)
type func = typeIdx * valtype list * expr

(* table::={type tabletype} *)
type table = tabletype

(* mem::={type memtype} *)
type mem = memtype

(* global::={type globaltype,init expr} *)
type global = globaltype * expr

(* elem::={table tableidx,offset expr,init vec(funcidx)} *)
type elem = tableIdx * expr * funcIdx list

(* data::={data memidx,offset expr,init vec(byte)} *)
type data = memIdx * expr * char list

(* start::={func funcidx} *)
type start = funcIdx

(* export::={name name,desc exportdesc} *)
type export = string * exportdesc

and exportdesc =
    | Efunc of funcIdx
    | Etable of tableIdx
    | Emem of memIdx
    | Eglobal of globalIdx

(* import::={module name,name name,desc importdesc} *)
type import = string * string * importdesc

and importdesc =
    | Ifunc of funcIdx
    | Itable of tableIdx
    | Imem of memIdx
    | Iglobal of globalIdx

type modules =
    (* definitions, types | functions |tables | memories | globals *)
    functype list
    * func list
    * table list
    * mem list
    * global list
    (* initialization logic, data | element | start function *)
    * elem list
    * data list
    * start option
    (* declare imports and exports *)
    * import list
    * export list
