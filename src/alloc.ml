open! Containers
open Types

let alloc_func (s : store) (func : func) (module_ : moduleinst)
    : store * funcaddr
  =
    let a = List.length s.funcs in
    let type_ = List.get_at_idx_exn func.type_ module_.types in
    let func_inst = Func { type_; module_; func } in
    let funcs = s.funcs @ [ func_inst ] in
    let s2 = { s with funcs } in
    (s2, a)


let alloc_host_func (s : store) (type_ : functype) (hostfunc : hostfunc)
    : store * funcaddr
  =
    let a = List.length s.funcs in
    let func_inst = HostFunc { type_; hostfunc } in
    let funcs = s.funcs @ [ func_inst ] in
    let s2 = { s with funcs } in
    (s2, a)


let alloc_table (s : store) ((limit, _) : tabletype) : store * tableaddr =
    let a = List.length s.tables in
    let elem = List.map (fun _ -> None) List.(1 -- limit.min) in
    let table_inst = { elem; max = limit.max } in
    let tables = s.tables @ [ table_inst ] in
    let s2 = { s with tables } in
    (s2, a)


let alloc_mem (s : store) ({ min; max } : memtype) : store * memaddr =
    let a = List.length s.mems in
    let data = Array.make min '\000' in
    let mem_inst = { data; max } in
    let mems = s.mems @ [ mem_inst ] in
    let s2 = { s with mems } in
    (s2, a)


let alloc_global (s : store) ((mut, _) : globaltype) (value : val_)
    : store * globaladdr
  =
    let a = List.length s.globals in
    let glob_inst = { value; mut } in
    let globals = s.globals @ [ glob_inst ] in
    let s2 = { s with globals } in
    (s2, a)


let grow_table (tbl : tableinst) (n : int) : tableinst option =
    let curr_len = List.length tbl.elem in
    let len = n + curr_len in
    if len >= 0xFFFFFFFF (* 2^32*)
    then None
    else
      match tbl.max with
          | Some limit when limit < len -> None
          | _ ->
              let empty = List.map (fun _ -> None) List.(1 -- n) in
              let elem = tbl.elem @ empty in
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


