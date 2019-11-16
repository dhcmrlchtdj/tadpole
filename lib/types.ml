open! Containers

type u32 = int [@@deriving show]

(* ******** *)

type idx = u32 [@@deriving show]

and typeidx = idx [@@deriving show]

and funcidx = idx [@@deriving show]

and tableidx = idx [@@deriving show]

and memidx = idx [@@deriving show]

and globalidx = idx [@@deriving show]

and localidx = idx [@@deriving show]

and labelidx = idx [@@deriving show]

(* ******** *)

type addr = u32 [@@deriving show]

and funcaddr = addr [@@deriving show]

and tableaddr = addr [@@deriving show]

and memaddr = addr [@@deriving show]

and globaladdr = addr [@@deriving show]

(* ******** *)

type value =
  | I32 of Nint32.t
  | I64 of Nint64.t
  | F32 of Nfloat32.t
  | F64 of Nfloat64.t
[@@deriving show]

type valtype =
  | TI32
  | TI64
  | TF32
  | TF64
[@@deriving show, eq]

type resulttype = valtype list [@@deriving show]

type functype = valtype list * valtype list [@@deriving show]

type limits = {
  min: u32;
  max: u32 option;
}
[@@deriving show]

type memtype = limits [@@deriving show]

type tabletype = limits * elemtype [@@deriving show]

and elemtype = FUNCREF [@@deriving show]

type mut =
  | CONST
  | VAR
[@@deriving show]

type globaltype = mut * valtype [@@deriving show]

type externtype =
  | ET_func of functype
  | ET_table of tabletype
  | ET_mem of memtype
  | ET_global of globaltype
[@@deriving show]

(* ******** *)

type table = { ttype: tabletype } [@@deriving show]

type mem = { mtype: memtype } [@@deriving show]

type export = {
  name: string;
  desc: exportdesc;
}
[@@deriving show]

and exportdesc =
  | ED_func of funcidx
  | ED_table of tableidx
  | ED_mem of memidx
  | ED_global of globalidx
[@@deriving show]

type import = {
  modname: string;
  name: string;
  desc: importdesc;
}
[@@deriving show]

and importdesc =
  | ID_func of typeidx
  | ID_table of tabletype
  | ID_mem of memtype
  | ID_global of globaltype
[@@deriving show]

(* ******** *)

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
[@@deriving show]

and binop =
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
[@@deriving show]

and testop = I_EQZ [@@deriving show]

and relop =
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
[@@deriving show]

and cvtop =
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
[@@deriving show]

(* ******** *)

type memarg = {
  align: u32;
  offset: u32;
}
[@@deriving show]

(* ******** *)

type exportinst = {
  name: string;
  value: externval;
}
[@@deriving show]

and externval =
  | EV_func of funcaddr
  | EV_table of tableaddr
  | EV_mem of memaddr
  | EV_global of globaladdr
[@@deriving show]

type tableinst = {
  elem: funcaddr option array;
  max: u32 option;
}
[@@deriving show]

type meminst = {
  data: bytes;
  max: u32 option;
}
[@@deriving show]

type globalinst = {
  value: value;
  mut: mut;
}
[@@deriving show]

type moduleinst = {
  mutable types: functype array;
  mutable funcaddrs: funcaddr array;
  mutable tableaddrs: tableaddr array;
  mutable memaddrs: memaddr array;
  mutable globaladdrs: globaladdr array;
  mutable exports: exportinst array;
}
[@@deriving show]

type start = { func: funcidx } [@@deriving show]

type moduledef = {
  types: functype array;
  funcs: func array;
  tables: table array;
  mems: mem array;
  globals: global array;
  elem: elem array;
  data: data array;
  start: start option;
  imports: import array;
  exports: export array;
}
[@@deriving show]

and global = {
  gtype: globaltype;
  init: expr;
}
[@@deriving show]

and elem = {
  table: tableidx;
  offset: expr;
  init: funcidx list;
}
[@@deriving show]

and data = {
  data: memidx;
  offset: expr;
  init: bytes;
}
[@@deriving show]

(* FIXME *)
and hostfunc = int [@@deriving show]

and func = {
  typei: typeidx;
  locals: valtype list;
  body: expr;
}
[@@deriving show]

and funcinst =
  | Func of {
      functype: functype;
      moduleinst: moduleinst;
      func: func;
    }
  | HostFunc of {
      functype: functype;
      hostfunc: hostfunc;
    }
[@@deriving show]

(* ******** *)
and instr =
  | Inumeric of numeric_instr
  | Iparametric of parametric_instr
  | Ivariable of variable_instr
  | Imemory of memory_instr
  | Icontrol of control_instr
  | Iadmin of admin_instr
[@@deriving show]

and numeric_instr =
  | Const of value
  | UnOp of valtype * unop
  | BinOp of valtype * binop
  | TestOp of valtype * testop
  | RelOp of valtype * relop
  (*i32.wrap_i64 -> t1.op_t2 *)
  | CvtOp of valtype * cvtop * valtype
[@@deriving show]

and parametric_instr =
  | Drop
  | Select
[@@deriving show]

and variable_instr =
  | LocalGet of localidx
  | LocalSet of localidx
  | LocalTee of localidx
  | GlobalGet of globalidx
  | GlobalSet of globalidx
[@@deriving show]

and memory_instr =
  | Load of valtype * memarg
  | Load8S of valtype * memarg
  | Load8U of valtype * memarg
  | Load16S of valtype * memarg
  | Load16U of valtype * memarg
  | Load32S of valtype * memarg
  | Load32U of valtype * memarg
  | Store of valtype * memarg
  | Store8 of valtype * memarg
  | Store16 of valtype * memarg
  | Store32 of valtype * memarg
  | MemorySize
  | MemoryGrow
[@@deriving show]

and control_instr =
  | Unreachable
  | Nop
  | Block of resulttype * instr list
  | Loop of resulttype * instr list
  | If of resulttype * instr list * instr list
  | Br of labelidx
  | BrIf of labelidx
  | BrTable of labelidx array * labelidx
  | Return
  | Call of funcidx
  | CallIndirect of typeidx
[@@deriving show]

and admin_instr =
  | Trap
  | Invoke of funcaddr
  | InitElem of tableaddr * instr list * funcidx list
  | InitData of memaddr * instr list * bytes
  | Label of int * instr list * instr list
  | Frame of int * frame * instr list
[@@deriving show]

and expr = instr list

(* ******** *)
and frame = {
  locals: value array;
  moduleinst: moduleinst;
}
[@@deriving show]

(* ******** *)

type store = {
  funcs: funcinst array;
  tables: tableinst array;
  mems: meminst array;
  globals: globalinst array;
}

(* ******** *)

type context = {
  store: store;
  frame: frame;
  evaluated: value list;
  cont: instr list;
}

(* ******** *)

let moduledef_to_string (m : moduledef) : string = show_moduledef m
