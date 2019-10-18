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
type valtype = I32 | I64 | F32 | F64

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
    (* iunop *)
    | CLZ
    | CTZ
    | POPCONT
    (* funop *)
    | ABS
    | NEG
    | SQRT
    | CEIL
    | FLOOR
    | TRUNC
    | NEAREST

type binop =
    (* ibinop *)
    (* | ADD *)
    (* | SUB *)
    (* | MUL *)
    (* | DIV *)
    | DIVu
    | REM
    | REMu
    | AND
    | OR
    | XOR
    | SHL
    | SHR
    | SHRu
    | ROTL
    | ROTR
    (* fbinop *)
    | ADD
    | SUB
    | MUL
    | DIV
    | MIN
    | MAX
    | COPYSIGN

type testop = (* itestop *) EQZ

type relop =
    (* irelop *)
    (* | EQ *)
    (* | NE *)
    (* | LT *)
    (* | LE *)
    (* | GT *)
    (* | GE *)
    | LTu
    | GTu
    | LEu
    | GEu
    (* frelop *)
    | EQ
    | NE
    | LT
    | GT
    | LE
    | GE

type value =
    | I32 of Int32.t
    | I64 of Int64.t
    | F32 of Float.t
    | F64 of Float.t

type instr =
    (* Numeric Instructions *)
    | Const of value
    | UnOp of unop * value
    | BinOp of binop * value * value
    | TestOp of testop * value
    | RelOp of relop * value
    | I32WrapI64
    | I64ExtendI32s
    | I64ExtendI32u
    | Trunc of value * value
    | Truncu of value * value
    | F32DemoteF64
    | F64PromoteF32
    | Convert of value * value
    | Convertu of value * value
    | Reinterpret of value * value
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
    | Load8s of memarg * valtype (* inn.load8_sx *)
    | Load8u of memarg * valtype (* inn.load8_sx *)
    | Load16s of memarg * valtype (* inn.load16_sx *)
    | Load16u of memarg * valtype (* inn.load16_sx *)
    | Load32s of memarg (* i64.load32_sx *)
    | Load32u of memarg (* i64.load32_sx *)
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
