type funcAddr = int
type tableAddr = int
type memAddr = int
type globalAddr = int

type results = Val of Ast.value | Trap

(* Store *)

type memInst = char list * int option

type tableInst = funcElem list * int option

and funcElem = funcAddr option

type globalInst = Ast.value * Ast.mut

type exportInst = string * externval

and externval =
    | FuncAddr of funcAddr
    | TableAddr of tableAddr
    | MemAddr of memAddr
    | GlobalAddr of globalAddr

type moduleInst =
    Ast.functype list
    * funcAddr list
    * tableAddr list
    * memAddr list
    * globalAddr list
    * exportInst list

type funcInst =
    | Func of Ast.functype * moduleInst * Ast.func
    | FuncHost of Ast.functype * hostFunc

and hostFunc = int

type store = funcInst list * tableInst list * memInst list * globalInst list

(* Stack *)

type label = int * Ast.instr list

type actiation = int * frame

and frame = Ast.value * moduleInst

type stack = stackEntry list

and stackEntry =
    | Value of Ast.value
    | Label of label
    | Activation of actiation
