type valtype = I32 | I64 | F32 | F64
type resulttype = valtype option list
type functype = valtype list * valtype list
type limits = {min: Int32.t; max: Int32.t option}
type memtype = limits

type tabletype = limits * elemtype

and elemtype = FUNCREF

type globaltype = mut * valtype

and mut = CONST | VAR

type externtype =
    | FUNC of functype
    | TABLE of tabletype
    | MEM of memtype
    | GLOBAL of globaltype
