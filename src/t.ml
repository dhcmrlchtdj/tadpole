open! Containers

let page_size = 0x10000

type typeidx = int

and funcidx = int

and tableidx = int

and memidx = int

and globalidx = int

and localidx = int

and labelidx = int

and funcaddr = int

and tableaddr = int

and memaddr = int

and globaladdr = int

(* type *)
and valtype =
    | TI32
    | TI64
    | TF32
    | TF64

and resulttype = valtype option list

and functype = valtype list * valtype list

and limits = {
    min : int;
    max : int option;
  }

and memtype = limits

and tabletype = limits * elemtype

and elemtype = FUNCREF

and globaltype = mut * valtype

and mut =
    | CONST
    | VAR

and externtype =
    | ET_func of functype
    | ET_table of tabletype
    | ET_mem of memtype
    | ET_global of globaltype

(* instruction *)
and unop =
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

and memarg = {
    offset : int;
    align : int;
  }

and numeric_instr =
    | I32Const of Int32.t
    | I64Const of Int64.t
    | F32Const of Float32.t
    | F64Const of Float64.t
    | UnOp of valtype * unop
    | BinOp of valtype * binop
    | TestOp of valtype * testop
    | RelOp of valtype * relop
    | CvtOp of valtype * cvtop * valtype

(*i32.wrap_i64 -> t1.op_t2 *)
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
    | BrTable of labelidx list * labelidx
    | Return
    | Call of funcidx
    | CallIndirect of typeidx

and admin_instr =
    | Trap
    | Invoke of funcaddr
    | InitElem of tableaddr * int * funcidx
    | InitData of memaddr * int * char
    | Label of int * instr list * instr list
    | Frame of int * frame * instr list

and instr =
    | Inumeric of numeric_instr
    | Iparametric of parametric_instr
    | Ivariable of variable_instr
    | Imemory of memory_instr
    | Icontrol of control_instr
    | Iadmin of admin_instr

and expr = instr list

(* module *)
and func = {
    type_ : typeidx;
    locals : valtype list;
    body : expr;
  }

and table = { ttype : tabletype }

and mem = { mtype : memtype }

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

and start = { func : funcidx }

and export = {
    name : string;
    desc : exportdesc;
  }

and exportdesc =
    | ED_func of funcidx
    | ED_table of tableidx
    | ED_mem of memidx
    | ED_global of globalidx

and import = {
    module_ : string;
    name : string;
    desc : importdesc;
  }

and importdesc =
    | ID_func of funcidx
    | ID_table of tabletype
    | ID_mem of memtype
    | ID_global of globaltype

and module_ = {
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

and val_ =
    | I32 of Int32.t
    | I64 of Int64.t
    | F32 of Float32.t
    | F64 of Float64.t

and result_ =
    | Rval of val_
    | Rtrap

(* --- *)
and exportinst = {
    name : string;
    value : externval;
  }

and externval =
    | EV_func of funcaddr
    | EV_table of tableaddr
    | EV_mem of memaddr
    | EV_global of globaladdr

(* --- *)
and moduleinst = {
    mutable types : functype list;
    mutable funcaddrs : funcaddr list;
    mutable tableaddrs : tableaddr list;
    mutable memaddrs : memaddr list;
    mutable globaladdrs : globaladdr list;
    mutable exports : exportinst list;
  }

(* --- *)
and funcinst =
    | Func of {
        type_ : functype;
        module_ : moduleinst;
        func : func;
      }
    | HostFunc of {
        type_ : functype;
        hostfunc : hostfunc;
      }

(* XXX *)
and hostfunc = int

(* --- *)
and tableinst = {
    elem : funcelem list;
    max : int option;
  }

and funcelem = funcaddr option

(* --- *)
and meminst = {
    data : char array;
    max : int option;
  }

(* --- *)
and globalinst = {
    value : val_;
    mut : mut;
  }

(* --- *)
and store = {
    funcs : funcinst list;
    tables : tableinst list;
    mems : meminst list;
    globals : globalinst list;
  }

(* --- *)
and label = int * instr list

(* --- *)
and activation = int * frame

and frame = {
    locals : val_ list;
    module_ : moduleinst;
  }

(* --- *)
and stack = stackEntry list

and stackEntry =
    | Value of val_
    | SLabel of label
    | SFrame of activation

(* --- *)
and config = store * thread

and thread = frame * instr list
