type u32 = int

(* ******** *)

type idx = u32

and typeidx = idx

and funcidx = idx

and tableidx = idx

and memidx = idx

and globalidx = idx

and localidx = idx

and labelidx = idx

(* ******** *)

type addr = u32

and funcaddr = addr

and tableaddr = addr

and memaddr = addr

and globaladdr = addr

(* ******** *)

type value =
    | I32 of Int32.t
    | I64 of Int64.t
    | F32 of Float32.t
    | F64 of Float64.t

type valtype =
    | TI32
    | TI64
    | TF32
    | TF64
[@@deriving eq]

type resulttype = valtype option list

type functype = valtype list * valtype list

type limits = {
    min : u32;
    max : u32 option;
  }

type memtype = limits

type tabletype = limits * elemtype

and elemtype = FUNCREF

type mut =
    | CONST
    | VAR

type globaltype = mut * valtype

type externtype =
    | ET_func of functype
    | ET_table of tabletype
    | ET_mem of memtype
    | ET_global of globaltype

(* ******** *)

type table = { ttype : tabletype }

type mem = { mtype : memtype }

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
    modname : string;
    name : string;
    desc : importdesc;
  }

and importdesc =
    | ID_func of funcidx
    | ID_table of tabletype
    | ID_mem of memtype
    | ID_global of globaltype

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

and testop = I_EQZ

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

(* ******** *)

type memarg = {
    offset : u32;
    align : u32;
  }

(* ******** *)

type exportinst = {
    name : string;
    value : externval;
  }

and externval =
    | EV_func of funcaddr
    | EV_table of tableaddr
    | EV_mem of memaddr
    | EV_global of globaladdr

type tableinst = {
    elem : funcaddr option array;
    max : u32 option;
  }

type meminst = {
    data : char array;
    max : u32 option;
  }

type globalinst = {
    value : value;
    mut : mut;
  }

type moduleinst = {
    mutable types : functype array;
    mutable funcaddrs : funcaddr array;
    mutable tableaddrs : tableaddr array;
    mutable memaddrs : memaddr array;
    mutable globaladdrs : globaladdr array;
    mutable exports : exportinst array;
  }

type start = { func : funcidx }

type moduledef = {
    types : functype array;
    funcs : func array;
    tables : table array;
    mems : mem array;
    globals : global array;
    elem : elem array;
    data : data array;
    start : start option;
    imports : import array;
    exports : export array;
  }

and global = {
    gtype : globaltype;
    init : expr;
  }

and elem = {
    table : tableidx;
    offset : expr;
    init : funcidx list;
  }

and data = {
    data : memidx;
    offset : expr;
    init : char list;
  }

(* FIXME *)
and hostfunc = int

and func = {
    typei : typeidx;
    locals : valtype list;
    body : expr;
  }

and funcinst =
    | Func of {
        functype : functype;
        moduleinst : moduleinst;
        func : func;
      }
    | HostFunc of {
        functype : functype;
        hostfunc : hostfunc;
      }

(* ******** *)
and instr =
    | Inumeric of numeric_instr
    | Iparametric of parametric_instr
    | Ivariable of variable_instr
    | Imemory of memory_instr
    | Icontrol of control_instr
    | Iadmin of admin_instr

and numeric_instr =
    | Const of value
    | UnOp of valtype * unop
    | BinOp of valtype * binop
    | TestOp of valtype * testop
    | RelOp of valtype * relop
    (*i32.wrap_i64 -> t1.op_t2 *)
    | CvtOp of valtype * cvtop * valtype

and parametric_instr =
    | Drop
    | Select

and variable_instr =
    | LocalGet of localidx
    | LocalSet of localidx
    | LocalTee of localidx
    | GlobalGet of globalidx
    | GlobalSet of globalidx

and memory_instr =
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
    | MemorySize
    | MemoryGrow

and control_instr =
    | Nop
    | Unreachable
    | Block of resulttype * instr list
    | Loop of resulttype * instr list
    | If of resulttype * instr list * instr list
    | Br of labelidx
    | BrIf of labelidx
    | BrTable of labelidx array * labelidx
    | Return
    | Call of funcidx
    | CallIndirect of typeidx

and admin_instr =
    | Trap
    | Invoke of funcaddr
    | InitElem of tableaddr * int * funcidx list
    | InitData of memaddr * int * char list
    | Label of int * instr list * instr list
    | Frame of int * frame * instr list

and expr = instr list

(* ******** *)
and frame = {
    locals : value array;
    moduleinst : moduleinst;
  }

(* ******** *)

type store = {
    funcs : funcinst array;
    tables : tableinst array;
    mems : meminst array;
    globals : globalinst array;
  }

(* ******** *)

type stack = stack_entry list

and stack_entry =
    | Value of value
    | Instr of instr

(* ******** *)

type context = {
    store : store;
    frame : frame;
    stack : stack;
    cont : stack;
  }
