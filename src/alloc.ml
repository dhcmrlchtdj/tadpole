open! Containers
open Types

let page_size = 0x10000

let alloc_func (moduleinst : moduleinst) (s : store) (func : func)
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
    (m : moduledef)
    (values : value array)
    (externs : externval list)
    : store * moduleinst
  =
    let mod_inst =
        {
          types = m.types;
          funcaddrs = [||];
          tableaddrs = [||];
          memaddrs = [||];
          globaladdrs = [||];
          exports = [||];
        }
    in
    let aux_alloc_addr store alloc arr =
        let aux (store, addrs) elem =
            let s, addr = alloc store elem in
            (s, addr :: addrs)
        in
        arr |> Array.fold_left aux (store, [])
    in
    let s1, faddrs = aux_alloc_addr s (alloc_func mod_inst) m.funcs in
    let s2, taddrs =
        aux_alloc_addr s1 alloc_table (Array.map (fun x -> x.ttype) m.tables)
    in
    let s3, maddrs =
        aux_alloc_addr s2 alloc_mem (Array.map (fun x -> x.mtype) m.mems)
    in
    let s4, gaddrs =
        aux_alloc_addr
          s3
          (fun s (g, v) -> alloc_global s g v)
          (Array.map2 (fun x v -> (x.gtype, v)) m.globals values)
    in
    let faddrs, taddrs, maddrs, gaddrs =
        let aux (f, t, m, g) = function
            | EV_func i -> (i :: f, t, m, g)
            | EV_table i -> (f, i :: t, m, g)
            | EV_mem i -> (f, t, i :: m, g)
            | EV_global i -> (f, t, m, i :: g)
        in
        externs |> List.fold_left aux (faddrs, taddrs, maddrs, gaddrs)
    in
    let exports =
        let aux ({ name; desc } : export) =
            let value =
                match desc with
                    | ED_func i -> EV_func i
                    | ED_table i -> EV_table i
                    | ED_mem i -> EV_mem i
                    | ED_global i -> EV_global i
            in
            { name; value }
        in
        Array.map aux m.exports
    in
    let revlist l = l |> List.rev |> Array.of_list in
    mod_inst.funcaddrs <- revlist faddrs ;
    mod_inst.tableaddrs <- revlist taddrs ;
    mod_inst.memaddrs <- revlist maddrs ;
    mod_inst.globaladdrs <- revlist gaddrs ;
    mod_inst.exports <- exports ;
    (s4, mod_inst)


let instantiate (s : store) (m : moduledef) (externs : externval list)
    : context
  =
    failwith "TODO"
