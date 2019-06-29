module Datum = struct
    type t =
        | INT of int
        | FLOAT of float
        | STRING of string
        | ID of string
        | KEYWORD of string
        | RESERVED of string
        | LIST of t list
    [@@deriving show, eq]
end

module Expression = struct
    type t = int [@@deriving show]
end

module Exp = struct
    module Op = struct
        type unop =
            | Clz
            | Ctz
            | Popcnt

        type binop =
            | Add
            | Sub
            | Mul
            | DivS
            | DivU
            | RemS
            | RemU
            | And
            | Or
            | Xor
            | Shl
            | ShrS
            | ShrU
            | Rotl
            | Rotr

        type testop = Eqz

        type relop =
            | Eq
            | Ne
            | LtS
            | LtU
            | GtS
            | GtU
            | LeS
            | LeU
            | GeS
            | GeU

        type cvtop =
            | ExtendSI32
            | ExtendUI32
            | WrapI64
            | TruncSF32
            | TruncUF32
            | TruncSF64
            | TruncUF64
            | ReinterpretFloat
    end

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
        | Drop
        | Select
        | Return
        | Br of var
        | BrIf of var
        | BrTable of var list
        | Call of var
        | CallIndirect of func_type
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
        | Test of val_type * Op.testop
        | Compare of val_type * Op.relop
        | Unary of val_type * Op.unop
        | Binary of val_type * Op.binop
        | Convert of val_type * Op.cvtop * val_type * sign option

    and sign =
        | U
        | S

    and offset = Offset of int

    and align = Align of int

    and value =
        | ValueI of int
        | ValueF of float
end
