module S = Structure

(* --- *)

type val_ =
    | I32 of Int32.t
    | I64 of Int64.t
    | F32 of Float32.t
    | F64 of Float64.t

type result_ =
    | Rval of val_
    | Rtrap

(* --- *)

type exportinst = {
    name : string;
    value : externval;
  }

and externval =
    | EV_func of S.funcaddr
    | EV_table of S.tableaddr
    | EV_mem of S.memaddr
    | EV_global of S.globaladdr

(* --- *)

type moduleinst = {
    types : S.functype list;
    funcaddrs : S.funcaddr list;
    tableaddrs : S.tableaddr list;
    memaddrs : S.memaddr list;
    globaladdrs : S.globaladdr list;
    exports : exportinst list;
  }

(* --- *)

type funcinst =
    | Func of {
        type_ : S.functype;
        module_ : moduleinst;
        code : S.func;
      }
    | HostFunc of {
        type_ : S.functype;
        hostcode : hostfunc;
      }

(* XXX *)
and hostfunc = int

(* --- *)

type tableinst = {
    elem : funcelem list;
    max : int option;
  }

and funcelem = S.funcaddr option

(* --- *)

type meminst = {
    data : char array;
    max : int option;
  }

let page_size = 65536

(* --- *)

type globalinst = {
    value : val_;
    mut : S.mut;
  }

(* --- *)

type store = {
    funcs : funcinst list;
    tables : tableinst list;
    mems : meminst list;
    globals : globalinst list;
  }

(* --- *)

type label = int * S.instr list

(* --- *)

type activation = int * frame

and frame = {
    locals : val_ list;
    module_ : moduleinst;
  }

(* --- *)

type stack = stackEntry list

and stackEntry =
    | Value of val_
    | Label of label
    | Activition of activation

(* --- *)

type config = store * thread

and thread = frame * S.instr list
