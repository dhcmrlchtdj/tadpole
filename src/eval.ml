open! Containers

type values =
    | I32 of Int32.t
    | I64 of Int64.t
    | F32 of Float.t
    | F64 of Float.t

(* --- *)

type results = Rval of values | Rtrap

(* --- *)

type memInst = char list * int option

type tableInst = funcElem list * int option

and funcElem = Types.funcAddr option

type globalInst = values * Types.mut

type exportInst = string * externval

and externval =
    | FuncAddr of Types.funcAddr
    | TableAddr of Types.tableAddr
    | MemAddr of Types.memAddr
    | GlobalAddr of Types.globalAddr

type moduleInst =
    Types.functype list
    * Types.funcAddr list
    * Types.tableAddr list
    * Types.memAddr list
    * Types.globalAddr list
    * exportInst list

type funcInst =
    | Func of Types.functype * moduleInst * Types.func
    | FuncHost of Types.functype * hostFunc

and hostFunc = int

type store = {
    funcs: funcInst list;
    tables: tableInst list;
    mems: memInst list;
    globals: globalInst list;
  }

(* --- *)

type label = int * Types.instr list

type actiation = int * frame

and frame = values * moduleInst

type stack = stackEntry list

and stackEntry = Value of values | Label of label | Activation of actiation
