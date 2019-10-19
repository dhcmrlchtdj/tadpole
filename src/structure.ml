type typeIdx = int

type funcIdx = int

type tableIdx = int

type memIdx = int

type globalIdx = int

type localIdx = int

type labelIdx = int

(* --- *)

type module_ = {
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
