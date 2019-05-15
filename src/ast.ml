type t =
    | MODULE of
          name option
          * typedef list
          * func list
          * import list
          * export list
          * table option
          * memory option
          * global list
          * elem list
          * data list
          * start option

(* id *)
and name = string

and typedef = Typedef of name option * (param list * result list)

and func =
    | Func of name option * func_type * local list * instr list
    | FuncExport of name option * func_type * local list * instr list * string
    | FuncImport of name option * func_type * (string * string)

and global =
    | Global of name option * global_type * instr list
    | GlobalExport of name option * global_type * instr list * string
    | GlobalImport of name option * global_type * (string * string)

and table =
    | Table of name option * table_type
    | TableExport of name option * table_type * string
    | TableImport of name option * table_type * (string * string)
    | TableExport2 of name option * string * elem_type * var list

and memory =
    | Memory of name option * memory_type
    | MemoryExport of name option * memory_type * string
    | MemoryImport of name option * memory_type * (string * string)
    | MemoryExport2 of name option * string * string list

and import = Import of string * string * imkind

and imkind =
    | ImFunc of name option * func_type
    | ImGlobal of name option * global_type
    | ImTable of name option * table_type
    | ImMemory of name option * memory_type

and export = Export of string * exkind

and exkind =
    | ExFunc of var
    | ExGlobal of var
    | ExTable of var
    | ExMemory of var

and elem =
    | ElemInstr of var option * instr list * string list
    | ElemExpr of var option * expr * string list

and data =
    | DataInstr of var option * instr list * string list
    | DataExpr of var option * expr * string list

and start = Start of var

and func_type = var option * param list * result list

and global_type = var * bool option

and table_type = int * int option * elem_type

and memory_type = int * int option

and elem_type = Funcref

and block_type = val_type list list

and val_type =
    | ValI32
    | ValI64
    | ValF32
    | ValF64

and var =
    | Nat of int
    | Name of name

and param =
    | ParamList of val_type list
    | ParamPair of name * val_type

and result = val_type list

and local =
    | LocalList of val_type list
    | LocalPair of name * val_type

and instr =
    | IExpr of expr
    | IOp of op
    | IBlock of name option * block_type * instr list
    | ILoop of name option * block_type * instr list
    | IIf of name option * block_type * instr list * instr list option
    | IIfExpr of
          name option * block_type * expr list * instr list * instr list option

and expr =
    | EOp of op
    | EOpExpr of op * expr list
    | EBlock of name option * block_type * instr list
    | ELoop of name option * block_type * instr list
    | EIf of name option * block_type * instr list * instr list option
    | EIfExpr of
          name option * block_type * expr list * instr list * instr list option

and op =
    | Unreachable
    | Nop
    | Br of var
    | BrIf of var
    | BrTable of var list
    | Return
    | Call of var
    | CallIndirect of func_type
    | Drop
    | Select
    | LocalGet of var
    | LocalSet of var
    | LocalTee of var
    | GlobalGet of var
    | GlobalSet of var
    | Load of val_type * (int * sign) option * offset option * align option
    | Store of val_type * int option * offset option * align option
    | MemorySize
    | MemoryGrow
    | Const of val_type * value
    | Unop of val_type * unop
    | Binop of val_type * binop
    | Testop of val_type * testop
    | Relop of val_type * relop
    | Cvtop of val_type * cvtop * val_type * sign option

and sign =
    | U
    | S

and offset = Offset of int

and align = Align of int

and value =
    | ValueI of int
    | ValueF of float

and unop =
    | Ctz
    | Clz
    | Popcnt

and binop =
    | Add
    | Sub
    | Mul

and relop =
    | Eq
    | Ne
    | Lt

and cvtop =
    | Trunc
    | Extend
    | Wrap

and testop = Test
