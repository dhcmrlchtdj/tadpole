type val_ = Ast.value

(* --- *)

type result_ =
    | Rval of val_
    | Rtrap

(* --- *)

type addr = int

type funcaddr = addr

type tableaddr = addr

type memaddr = addr

type globaladdr = addr

(* --- *)

type exportinst = {
    name : string;
    value : externval;
  }

and externval =
    | EFunc of funcaddr
    | ETable of tableaddr
    | EMem of memaddr
    | EGlobal of globaladdr

(* --- *)

type moduleinst = {
    types : Ast.functype list;
    funcaddrs : funcaddr list;
    tableaddrs : tableaddr list;
    memaddrs : memaddr list;
    globaladdrs : globaladdr list;
    exports : exportinst list;
  }

(* --- *)

type funcinst =
    | Func of {
        type_ : Ast.functype;
        module_ : moduleinst;
        code : Ast.func;
      }
    | HostFunc of {
        type_ : Ast.functype;
        hostcode : hostfunc;
      }

and hostfunc = int

(* --- *)

type tableinst = {
    elem : funcelem list;
    max : Int32.t option;
  }

and funcelem = funcaddr option

(* --- *)

type meminst = {
    data : char list;
    max : Int32.t option;
  }

(* --- *)

type globalinst = {
    value : val_;
    mut : Ast.mut;
  }

(* --- *)

type store = {
    funcs : funcinst;
    tables : tableinst;
    mems : meminst;
    globals : globalinst;
  }

(* --- *)

type labels = int * Ast.instr list

(* --- *)

type activations = int * frame

and frame = {
    locals : val_ list;
    module_ : moduleinst;
  }

(* --- *)

type stack = stackEntry list

and stackEntry =
    | Svalue of val_
    | Slabel of labels
    | Sactivition of activations

(* --- *)

type config = store * thread

and thread = frame * Ast.instr list
