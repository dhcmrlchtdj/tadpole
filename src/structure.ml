type u32 = Uint32.t

type typeidx = u32

type funcidx = u32

type tableidx = u32

type memidx = u32

type globalidx = u32

type localidx = u32

type labelidx = u32

type funcaddr = u32

type tableaddr = u32

type memaddr = u32

type globaladdr = u32

(* type *)

type valtype =
    | I32
    | I64
    | F32
    | F64

type resulttype = valtype option list

type functype = valtype list * valtype list

type limits = {
    min : u32;
    max : u32 option;
  }

type memtype = limits

type tabletype = limits * elemtype

and elemtype = FUNCREF

type globaltype = mut * valtype

and mut =
    | CONST
    | VAR

type externtype =
    | ET_func of functype
    | ET_table of tabletype
    | ET_mem of memtype
    | ET_global of globaltype

(* instruction *)

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

type memarg = {
    offset : u32;
    align : u32;
  }

type instr =
    (* Numeric Instructions *)
    | I32Const of Int32.t
    | I64Const of Int64.t
    | F32Const of Float32.t
    | F64Const of Float64.t
    | UnOp of valtype * unop
    | BinOp of valtype * binop
    | TestOp of valtype * testop
    | RelOp of valtype * relop
    | CvtOp of valtype * cvtop * valtype (*i32.wrap_i64 -> t1.op_t2 *)
    (* Parametric Instructions *)
    | Drop
    | Select
    (* Variable Instructions *)
    | LocalGet of localidx
    | LocalSet of localidx
    | LocalTee of localidx
    | GlobalGet of globalidx
    | GlobalSet of globalidx
    (* Memory Instructions *)
    | Load of valtype * memarg
    | Store of valtype * memarg
    | Load8S of valtype * memarg
    | Load8U of valtype * memarg
    | Load16S of valtype * memarg
    | Load16U of valtype * memarg
    | Load32S of valtype * memarg
    | Load32U of valtype * memarg
    | Store8 of valtype * memarg
    | Store16 of valtype * memarg
    | Store32 of valtype * memarg
    | MemortSize
    | MemortGrow
    (* Control Instructions *)
    | Nop
    | Unreachable
    | Block of resulttype * instr list
    | Loop of resulttype * instr list
    | If of resulttype * instr list * instr list
    | Br of labelidx
    | BrIf of labelidx
    | BrTable of labelidx list * labelidx
    | Return
    | Call of funcidx
    | CallIndirect of typeidx
    (* Administrative Instructions *)
    | Trap
    | Invoke of funcaddr
    | InitElem of tableaddr * u32 * funcidx
    | InitData of memaddr * u32 * char
    | Label of int * instr list
    | Frame of int * instr list

type expr = instr list

(* module *)

type func = {
    type_ : typeidx;
    locals : valtype list;
    body : expr;
  }

type table = { type_ : tabletype }

type mem = { type_ : memtype }

type global = {
    type_ : globaltype;
    init : expr;
  }

type elem = {
    table : tableidx;
    offset : expr;
    init : funcidx list;
  }

type data = {
    data : memidx;
    offset : expr;
    init : char list;
  }

type start = { func : funcidx }

type export = {
    name : string;
    desc : exportdesc;
  }

and exportdesc =
    | ED_func of funcidx
    | ED_table of tableidx
    | ED_mem of memidx
    | ED_global of globalidx

type import = {
    module_ : string;
    name : string;
    desc : importdesc;
  }

and importdesc =
    | ID_func of funcidx
    | ID_table of tabletype
    | ID_mem of memtype
    | ID_global of globaltype

type module_ = {
    types : functype list;
    funcs : func list;
    tables : table list;
    mems : mem list;
    globals : global list;
    elem : elem list;
    data : data list;
    start : start option;
    imports : import list;
    exports : export list;
  }
