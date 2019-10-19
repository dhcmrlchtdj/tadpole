open Structure

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
    | EV_func of funcaddr
    | EV_table of tableaddr
    | EV_mem of memaddr
    | EV_global of globaladdr

(* --- *)

type moduleinst = {
    types : functype list;
    funcaddrs : funcaddr list;
    tableaddrs : tableaddr list;
    memaddrs : memaddr list;
    globaladdrs : globaladdr list;
    exports : exportinst list;
  }

(* --- *)

type funcinst =
    | Func of {
        type_ : functype;
        module_ : moduleinst;
        code : func;
      }
    | HostFunc of {
        type_ : functype;
        hostcode : hostfunc;
      }

(* XXX *)
and hostfunc = int

(* --- *)

type tableinst = {
    elem : funcelem list;
    max : int option;
  }

and funcelem = funcaddr option

(* --- *)

type meminst = {
    data : char array;
    max : int option;
  }

let page_size = 65536

(* --- *)

type globalinst = {
    value : val_;
    mut : mut;
  }

(* --- *)

type store = {
    funcs : funcinst list;
    tables : tableinst list;
    mems : meminst list;
    globals : globalinst list;
  }

(* --- *)

type label = int * instr list

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
    | SLabel of label
    | SFrame of activation

(* --- *)

type config = store * thread

and thread = frame * instr list
