open! Containers
open Types

let page_size = 0x10000

let alloc_func (s : store) (moduleinst : moduleinst) (func : func)
    : store * funcaddr
  =
    let a = Array.length s.funcs in
    let functype = moduleinst.types.(func.typei) in
    let func_inst = Func { functype; moduleinst; func } in
    let funcs = Array.append s.funcs [| func_inst |] in
    let s2 = { s with funcs } in
    (s2, a)


let alloc_host_func (s : store) (functype : functype) (hostfunc : hostfunc)
    : store * funcaddr
  =
    let a = Array.length s.funcs in
    let func_inst = HostFunc { functype; hostfunc } in
    let funcs = Array.append s.funcs [| func_inst |] in
    let s2 = { s with funcs } in
    (s2, a)


let alloc_table (s : store) ((limit, _) : tabletype) : store * tableaddr =
    let a = Array.length s.tables in
    let elem = Array.make limit.min None in
    let table_inst = { elem; max = limit.max } in
    let tables = Array.append s.tables [| table_inst |] in
    let s2 = { s with tables } in
    (s2, a)


let alloc_mem (s : store) ({ min; max } : memtype) : store * memaddr =
    let a = Array.length s.mems in
    let data = Array.make min '\000' in
    let mem_inst = { data; max } in
    let mems = Array.append s.mems [| mem_inst |] in
    let s2 = { s with mems } in
    (s2, a)


let alloc_global (s : store) ((mut, _) : globaltype) (value : value)
    : store * globaladdr
  =
    let a = Array.length s.globals in
    let glob_inst = { value; mut } in
    let globals = Array.append s.globals [| glob_inst |] in
    let s2 = { s with globals } in
    (s2, a)


let grow_table (tbl : tableinst) (n : int) : tableinst option =
    let len = n + Array.length tbl.elem in
    if len >= 0xFFFFFFFF (* 2^32*)
    then None
    else
      match tbl.max with
          | Some limit when limit < len -> None
          | _ ->
              let empty = Array.make n None in
              let elem = Array.append tbl.elem empty in
              Some { tbl with elem }


let grow_mem (mem : meminst) (n : int) : meminst option =
    let size = Array.length mem.data in
    let curr_len = size / page_size in
    let len = curr_len + n in
    if len <= 0x10000 (* 2^16 *)
    then None
    else
      match mem.max with
          | Some limit when limit < len -> None
          | _ ->
              let empty = Array.make n '\000' in
              let data = Array.append mem.data empty in
              Some { mem with data }


let alloc_module
    (s : store)
    (m : module_)
    (ext : externval list)
    (values : value list)
    : store * moduleinst
  =
    let mod_inst =
        {
          types = [||];
          funcaddrs = [||];
          tableaddrs = [||];
          memaddrs = [||];
          globaladdrs = [||];
          exports = [||];
        }
    in
    failwith "TODO"


let instantiate (s : store) (m : module_) (externs : externval list) : context =
    failwith "TODO"


let rec invoke (store : store) (f : funcaddr) (values : value list)
    : value list
  =
    let func_inst = store.funcs.(f) in
    let params, rets =
        match func_inst with
            | Func f -> f.functype
            | HostFunc f -> f.functype
    in
    let len_eq = List.length params = List.length values in
    let type_eq = aux_type_eq params values in
    if len_eq && type_eq
    then
      let moduleinst =
          {
            types = [||];
            funcaddrs = [||];
            tableaddrs = [||];
            memaddrs = [||];
            globaladdrs = [||];
            exports = [||];
          }
      in
      let frame = { locals = []; moduleinst } in
      let stack = values |> List.map (fun v -> Value v) |> List.rev in
      let stack = Instr (Iadmin (Invoke f)) :: stack in
      let ctx = { store; frame; stack } in
      let ctx2 = Wa.eval_instr ctx in
      let vs = aux_take_m [] (List.length rets) ctx2.stack in
      vs
    else failwith "TODO: argument error"


and aux_type_eq params args =
    let test (p : valtype) (a : value) =
        match (p, a) with
            | I32, I32 _ | I64, I64 _ | F32, F32 _ | F64, F64 _ -> true
            | _ -> false
    in
    List.for_all2 test params args


and aux_take_m acc m stack =
    if m = 0
    then List.rev acc
    else
      match stack with
          | Value v :: t -> aux_take_m (v :: acc) (m - 1) t
          | _ -> failwith "assert failure"
