open! Containers
open Types

let page_size = (* 64K *) 0x1_0000

let grow_table (tbl : tableinst) (n : int) : tableinst option =
    let len = n + Array.length tbl.elem in
    let limit =
        match tbl.max with
            | Some l -> l
            | None -> (* 2^32 *) 0xFFFF_FFFF
    in
    if len >= limit
    then None
    else
      let empty = Array.make n None in
      let elem = Array.append tbl.elem empty in
      Some { tbl with elem }


let grow_mem (mem : meminst) (n : int) : meminst option =
    let size = Bytes.length mem.data in
    let len = (size / page_size) + n in
    let limit =
        match mem.max with
            | Some l -> l
            | None -> (* 2^16 *) 0x1_0000
    in
    if len >= limit
    then None
    else
      let empty = Bytes.make n '\000' in
      let data = Bytes.cat mem.data empty in
      Some { mem with data }


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
    let data = Bytes.make min '\000' in
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


let alloc_module
    (s : store)
    (m : moduledef)
    (externs : externval list)
    (values : value array)
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
            let (s, addr) = alloc store elem in
            (s, addr :: addrs)
        in
        arr |> Array.fold_left aux (store, [])
    in
    let (s1, faddrs) = aux_alloc_addr s (alloc_func mod_inst) m.funcs in
    let (s2, taddrs) =
        aux_alloc_addr s1 alloc_table (Array.map (fun x -> x.ttype) m.tables)
    in
    let (s3, maddrs) =
        aux_alloc_addr s2 alloc_mem (Array.map (fun x -> x.mtype) m.mems)
    in
    let (s4, gaddrs) =
        aux_alloc_addr
          s3
          (fun s (g, v) -> alloc_global s g v)
          (Array.map2 (fun x v -> (x.gtype, v)) m.globals values)
    in
    let (faddrs, taddrs, maddrs, gaddrs) =
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


(* ******** *)

let aux_trap (ctx : context) = { ctx with cont = [ Iadmin Trap ] }

let aux_get_functype = function
    | Func { functype; _ } -> functype
    | HostFunc { functype; _ } -> functype


(* ******** *)

let rec eval_expr (context : context) : value * context =
    let ctx = eval_instr context in
    match ctx.evaluated with
        | v :: evaluated -> (v, { ctx with evaluated })
        | _ -> failwith "assert failure"


and eval_instr (ctx : context) : context =
    match ctx.cont with
        | [] -> ctx
        | Icontrol Return :: _ -> { ctx with cont = [ Icontrol Return ] }
        | Icontrol (Br 0) :: _ -> { ctx with cont = [ Icontrol (Br 0) ] }
        | Icontrol (Br l) :: _ -> { ctx with cont = [ Icontrol (Br (l - 1)) ] }
        | _ ->
            let next =
                match ctx.cont with
                    | [] -> ctx
                    | Inumeric i :: cont ->
                        eval_numeric_instr { ctx with cont } i
                    | Iparametric i :: cont ->
                        eval_parametric_instr { ctx with cont } i
                    | Ivariable i :: cont ->
                        eval_variable_instr { ctx with cont } i
                    | Imemory i :: cont ->
                        eval_memory_instr { ctx with cont } i
                    | Icontrol i :: cont ->
                        eval_control_instr { ctx with cont } i
                    | Iadmin i :: cont -> eval_admin_instr { ctx with cont } i
            in
            eval_instr next


and eval_numeric_instr (ctx : context) = function
    | Const v ->
        let evaluated = v :: ctx.evaluated in
        { ctx with evaluated }
    | UnOp (t, op) -> (
        match ctx.evaluated with
            | c :: tail -> (
                match EvalNum.unop (t, op, c) with
                    | Some v ->
                        let evaluated = v :: tail in
                        { ctx with evaluated }
                    | None -> aux_trap ctx )
            | _ -> failwith "assert failure" )
    | BinOp (t, op) -> (
        match ctx.evaluated with
            | c2 :: c1 :: tail -> (
                match EvalNum.binop (t, op, c1, c2) with
                    | Some v ->
                        let evaluated = v :: tail in
                        { ctx with evaluated }
                    | None -> aux_trap ctx )
            | _ -> failwith "assert failure" )
    | TestOp (t, op) -> (
        match ctx.evaluated with
            | c :: tail -> (
                match EvalNum.testop (t, op, c) with
                    | Some true ->
                        let evaluated = I32 1l :: tail in
                        { ctx with evaluated }
                    | Some false ->
                        let evaluated = I32 0l :: tail in
                        { ctx with evaluated }
                    | None -> aux_trap ctx )
            | _ -> failwith "assert failure" )
    | RelOp (t, op) -> (
        match ctx.evaluated with
            | c2 :: c1 :: tail -> (
                match EvalNum.relop (t, op, c1, c2) with
                    | Some true ->
                        let evaluated = I32 1l :: tail in
                        { ctx with evaluated }
                    | Some false ->
                        let evaluated = I32 0l :: tail in
                        { ctx with evaluated }
                    | None -> aux_trap ctx )
            | _ -> failwith "assert failure" )
    | CvtOp (t2, op, t1) -> (
        match ctx.evaluated with
            | c :: tail -> (
                match EvalNum.cvtop (t2, op, t1, c) with
                    | Some v ->
                        let evaluated = v :: tail in
                        { ctx with evaluated }
                    | None -> aux_trap ctx )
            | _ -> failwith "assert failure" )


and eval_parametric_instr (ctx : context) = function
    | Drop -> (
        match ctx.evaluated with
            | _ :: evaluated -> { ctx with evaluated }
            | _ -> failwith "assert failure" )
    | Select -> (
        match ctx.evaluated with
            | I32 c :: v2 :: v1 :: tail ->
                let h = if Int32.equal c 0l then v2 else v1 in
                let evaluated = h :: tail in
                { ctx with evaluated }
            | _ -> failwith "assert failure" )


and eval_variable_instr (ctx : context) = function
    | LocalGet x ->
        let v = ctx.frame.locals.(x) in
        let evaluated = v :: ctx.evaluated in
        { ctx with evaluated }
    | LocalSet x -> (
        match ctx.evaluated with
            | v :: evaluated ->
                let () = ctx.frame.locals.(x) <- v in
                { ctx with evaluated }
            | _ -> failwith "assert failure" )
    | LocalTee x -> (
        match ctx.evaluated with
            | h :: t ->
                let cont = Ivariable (LocalSet x) :: ctx.cont in
                let evaluated = h :: h :: t in
                { ctx with evaluated; cont }
            | _ -> failwith "assert failure" )
    | GlobalGet x ->
        let addr = ctx.frame.moduleinst.globaladdrs.(x) in
        let glob = ctx.store.globals.(addr) in
        let evaluated = glob.value :: ctx.evaluated in
        { ctx with evaluated }
    | GlobalSet x -> (
        match ctx.evaluated with
            | value :: evaluated ->
                let addr = ctx.frame.moduleinst.globaladdrs.(x) in
                let glob = ctx.store.globals.(addr) in
                let new_glob = { glob with value } in
                let () = ctx.store.globals.(addr) <- new_glob in
                { ctx with evaluated }
            | _ -> failwith "assert failure" )


and eval_memory_instr =
    let rec aux_memory_load ctx (memarg : memarg) len to_value =
        let addr = ctx.frame.moduleinst.memaddrs.(0) in
        let mem = ctx.store.mems.(addr) in
        match ctx.evaluated with
            | I32 i :: tail ->
                let ii = Int32.to_int i in
                let ea = ii + memarg.offset in
                if ea + len <= Bytes.length mem.data
                then
                  let b = Bytes.make 8 '\000' in
                  let () = Bytes.blit mem.data ea b (8 - len) 8 in
                  let value = to_value b in
                  let evaluated = value :: tail in
                  { ctx with evaluated }
                else aux_trap ctx
            | _ -> failwith "assert failure"
    and aux_char8_to_int64 b = Bytes.get_int64_le b 0
    and aux_char8_to_int64_s n arr =
        if Sys.big_endian then failwith "FIXME: deal with big_endian" ;
        let pos =
            match n with
                | 8 -> 7
                | 16 -> 6
                | 32 -> 4
                | _ -> failwith "never"
        in
        let sign = Char.code (Bytes.get arr pos) in
        if sign lsr 7 = 1
        then (
          Bytes.set arr pos (Char.chr (sign land 0x7F)) ;
          let x = aux_char8_to_int64 arr in
          Int64.neg x )
        else aux_char8_to_int64 arr
    in
    let rec aux_memory_store ctx (memarg : memarg) len =
        let addr = ctx.frame.moduleinst.memaddrs.(0) in
        let mem = ctx.store.mems.(addr) in
        match ctx.evaluated with
            | c :: I32 i :: evaluated ->
                let ii = Int32.to_int i in
                let ea = memarg.offset + ii in
                if ea + len <= Bytes.length mem.data
                then
                  let b = aux_bytes len c in
                  let () = Bytes.blit b 0 mem.data ea len in
                  { ctx with evaluated }
                else aux_trap ctx
            | _ -> failwith "assert failure"
    and aux_bytes len value =
        if Sys.big_endian then failwith "FIXME: deal with big_endian" ;
        let i64 =
            match value with
                | I32 x -> Int64.of_int32 x
                | I64 x -> x
                | F32 x -> x |> Float32.int32_of_bits |> Int64.of_int32
                | F64 x -> Int64.bits_of_float x
        in
        let arr = Bytes.make 8 '\000' in
        let x = ref i64 in
        for i = 7 downto 0 do
          let data = !x |> Int64.logand 0xFFL |> Int64.to_int |> Char.chr in
          Bytes.set arr i data ;
          x := Int64.shift_right_logical !x 8
        done ;
        Bytes.sub arr (8 - len) len
    in
    fun (ctx : context) -> function
        | Load (TI32, memarg) ->
            aux_memory_load ctx memarg 4 (fun arr ->
                I32 (aux_char8_to_int64 arr |> Int64.to_int32))
        | Load (TI64, memarg) ->
            aux_memory_load ctx memarg 8 (fun arr ->
                I64 (aux_char8_to_int64 arr))
        | Load (TF32, memarg) ->
            aux_memory_load ctx memarg 4 (fun arr ->
                F32
                  ( aux_char8_to_int64 arr
                  |> Int64.to_int32
                  |> Float32.bits_of_int32 ))
        | Load (TF64, memarg) ->
            aux_memory_load ctx memarg 8 (fun arr ->
                F64 (aux_char8_to_int64 arr |> Int64.float_of_bits))
        | Load8S (TI32, memarg) ->
            aux_memory_load ctx memarg 1 (fun arr ->
                I32 (aux_char8_to_int64_s 8 arr |> Int64.to_int32))
        | Load8S (TI64, memarg) ->
            aux_memory_load ctx memarg 1 (fun arr ->
                I64 (aux_char8_to_int64_s 8 arr))
        | Load8U (TI32, memarg) ->
            aux_memory_load ctx memarg 1 (fun arr ->
                I32 (aux_char8_to_int64 arr |> Int64.to_int32))
        | Load8U (TI64, memarg) ->
            aux_memory_load ctx memarg 1 (fun arr ->
                I64 (aux_char8_to_int64 arr))
        | Load16S (TI32, memarg) ->
            aux_memory_load ctx memarg 2 (fun arr ->
                I32 (aux_char8_to_int64_s 16 arr |> Int64.to_int32))
        | Load16S (TI64, memarg) ->
            aux_memory_load ctx memarg 2 (fun arr ->
                I64 (aux_char8_to_int64_s 16 arr))
        | Load16U (TI32, memarg) ->
            aux_memory_load ctx memarg 2 (fun arr ->
                I32 (aux_char8_to_int64 arr |> Int64.to_int32))
        | Load16U (TI64, memarg) ->
            aux_memory_load ctx memarg 2 (fun arr ->
                I64 (aux_char8_to_int64 arr))
        | Load32S (TI64, memarg) ->
            aux_memory_load ctx memarg 4 (fun arr ->
                I64 (aux_char8_to_int64_s 32 arr))
        | Load32U (TI64, memarg) ->
            aux_memory_load ctx memarg 4 (fun arr ->
                I64 (aux_char8_to_int64 arr))
        | Store (TI32, memarg) -> aux_memory_store ctx memarg 4
        | Store (TI64, memarg) -> aux_memory_store ctx memarg 8
        | Store (TF32, memarg) -> aux_memory_store ctx memarg 4
        | Store (TF64, memarg) -> aux_memory_store ctx memarg 8
        | Store8 (TI32, memarg) -> aux_memory_store ctx memarg 1
        | Store8 (TI64, memarg) -> aux_memory_store ctx memarg 1
        | Store16 (TI32, memarg) -> aux_memory_store ctx memarg 2
        | Store16 (TI64, memarg) -> aux_memory_store ctx memarg 2
        | Store32 (TI64, memarg) -> aux_memory_store ctx memarg 4
        | MemorySize ->
            let addr = ctx.frame.moduleinst.memaddrs.(0) in
            let mem = ctx.store.mems.(addr) in
            let sz = Bytes.length mem.data / page_size in
            let evaluated = I32 (Int32.of_int sz) :: ctx.evaluated in
            { ctx with evaluated }
        | MemoryGrow -> (
            let addr = ctx.frame.moduleinst.memaddrs.(0) in
            let mem = ctx.store.mems.(addr) in
            let sz = Bytes.length mem.data / page_size in
            match ctx.evaluated with
                | I32 n :: tail ->
                    let nsz =
                        match grow_mem mem (Int32.to_int n) with
                            | Some m ->
                                let () = ctx.store.mems.(addr) <- m in
                                I32 (Int32.of_int sz)
                            | None -> I32 (-1l)
                    in
                    let evaluated = nsz :: tail in
                    { ctx with evaluated }
                | _ -> failwith "assert failure" )
        | _ -> failwith "never"


and eval_control_instr (ctx : context) = function
    | Nop -> ctx
    | Unreachable -> aux_trap ctx
    | Block (rtypes, instrs) ->
        let n = List.length rtypes in
        let l = Iadmin (Label (n, [], instrs)) in
        let cont = l :: ctx.cont in
        { ctx with cont }
    | Loop (_, instrs) as sub ->
        let l = Iadmin (Label (0, [ Icontrol sub ], instrs)) in
        let cont = l :: ctx.cont in
        { ctx with cont }
    | If (rtypes, instrs1, instrs2) -> (
        match ctx.evaluated with
            | I32 c :: evaluated ->
                let n = List.length rtypes in
                let cont = if Int32.equal c 0l then instrs2 else instrs1 in
                let l = Iadmin (Label (n, [], cont)) in
                let cont = l :: ctx.cont in
                { ctx with evaluated; cont }
            | _ -> failwith "assert failure" )
    | Br _ as sub -> { ctx with cont = [ Icontrol sub ] }
    | BrIf l -> (
        match ctx.evaluated with
            | I32 c :: evaluated ->
                let cont =
                    if Int32.equal c 0l
                    then ctx.cont
                    else Icontrol (Br l) :: ctx.cont
                in
                { ctx with evaluated; cont }
            | _ -> failwith "assert failure" )
    | BrTable (ls, l) -> (
        match ctx.evaluated with
            | I32 i :: evaluated ->
                let ii = Int32.to_int i in
                let br =
                    if ii < Array.length ls
                    then Icontrol (Br ls.(ii))
                    else Icontrol (Br l)
                in
                let cont = br :: ctx.cont in
                { ctx with evaluated; cont }
            | _ -> failwith "assert failure" )
    | Return -> { ctx with cont = [ Icontrol Return ] }
    | Call x ->
        let addr = ctx.frame.moduleinst.funcaddrs.(x) in
        let cont = Iadmin (Invoke addr) :: ctx.cont in
        { ctx with cont }
    | CallIndirect x -> (
        let aux_ft_eq (expect : functype) (actual : functype) : bool =
            let (e_param, e_arg) = expect in
            let (a_param, a_arg) = actual in
            let p = List.for_all2 equal_valtype e_param a_param in
            let a = List.for_all2 equal_valtype e_arg a_arg in
            p && a
        in
        let taddr = ctx.frame.moduleinst.tableaddrs.(0) in
        let tab = ctx.store.tables.(taddr) in
        let ft_expect = ctx.frame.moduleinst.types.(x) in
        match ctx.evaluated with
            | I32 i :: evaluated ->
                let ii = Int32.to_int i in
                if ii < Array.length tab.elem
                then
                  match tab.elem.(ii) with
                      | Some faddr ->
                          let f = ctx.store.funcs.(faddr) in
                          let ft_actual = aux_get_functype f in
                          if aux_ft_eq ft_expect ft_actual
                          then
                            let cont = Iadmin (Invoke faddr) :: ctx.cont in
                            { ctx with evaluated; cont }
                          else aux_trap ctx
                      | None -> aux_trap ctx
                else aux_trap ctx
            | _ -> failwith "assert failure" )


and eval_admin_instr (ctx : context) = function
    (* TODO *)
    | Trap -> failwith "trap"
    | Invoke a -> (
        let f = ctx.store.funcs.(a) in
        match f with
            | Func func ->
                let (params, rets) = func.functype in
                let val0 =
                    List.map
                      (function
                            | TI32 -> I32 0l
                            | TI64 -> I64 0L
                            | TF32 -> F32 Float32.zero
                            | TF64 -> F64 0.0)
                      func.func.locals
                in
                let n = List.length params in
                let vals = List.take n ctx.evaluated in
                let evaluated = List.drop n ctx.evaluated in
                let locals = Array.of_list (vals @ val0) in
                let frame = { moduleinst = func.moduleinst; locals } in
                let instrs = func.func.body in
                let block =
                    Icontrol (Block (List.map (fun x -> Some x) rets, instrs))
                in
                let iframe =
                    let m = List.length rets in
                    Iadmin (Frame (m, frame, [ block ]))
                in
                let cont = iframe :: ctx.cont in
                { ctx with evaluated; cont }
            | HostFunc _ -> failwith "TODO" )
    | InitElem (tableaddr, offset_expr, funcs) ->
        let (v, ctx2) = eval_expr { ctx with cont = offset_expr } in
        let offset =
            match v with
                | I32 x -> Int32.to_int x
                | _ -> failwith "assert failure"
        in
        let table_inst = ctx2.store.tables.(tableaddr) in
        let _ =
            List.iteri
              (fun i funcidx -> table_inst.elem.(offset + i) <- Some funcidx)
              funcs
        in
        ctx2
    | InitData (memaddr, offset_expr, data) ->
        let (v, ctx2) = eval_expr { ctx with cont = offset_expr } in
        let offset =
            match v with
                | I32 x -> Int32.to_int x
                | _ -> failwith "assert failure"
        in
        let mem_inst = ctx2.store.mems.(memaddr) in
        let _ = Bytes.blit data 0 mem_inst.data offset (Bytes.length data) in
        ctx2
    | Label (n, next_instrs, inner_instrs) -> (
        let new_ctx = eval_instr { ctx with cont = inner_instrs } in
        match new_ctx.cont with
            | [] | [ Icontrol (Br 0) ] ->
                let evaluated =
                    List.append (List.take n new_ctx.evaluated) ctx.evaluated
                in
                let cont = List.append next_instrs ctx.cont in
                { ctx with store = new_ctx.store; evaluated; cont }
            | [ Icontrol Return ] -> new_ctx
            | _ -> new_ctx )
    | Frame (n, frame, instrs) ->
        let new_ctx = eval_instr { ctx with frame; cont = instrs } in
        let vals = List.take n new_ctx.evaluated in
        let evaluated = List.append vals ctx.evaluated in
        { ctx with store = new_ctx.store; evaluated }


(* ******** *)

let invoke =
    let aux_type_eq params args =
        let test (p : valtype) (a : value) =
            match (p, a) with
                | (TI32, I32 _) | (TI64, I64 _) | (TF32, F32 _) | (TF64, F64 _)
                  -> true
                | _ -> false
        in
        List.for_all2 test params args
    in
    fun (store : store) (f : funcaddr) (values : value list) ->
        let func_inst = store.funcs.(f) in
        let (params, _rets) = aux_get_functype func_inst in
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
          let frame = { locals = [||]; moduleinst } in
          let evaluated = List.rev values in
          let cont = [ Iadmin (Invoke f) ] in
          let ctx = { store; frame; evaluated; cont } in
          eval_instr ctx
        else failwith "argument error"


(* ******** *)

let instantiate (s : store) (m : moduledef) (externs : externval list)
    : context
  =
    let vals =
        let global_externs =
            externs
            |> List.filter (function
                   | EV_global _ -> true
                   | _ -> false)
            |> List.map (function
                   | EV_global addr -> addr
                   | _ -> failwith "never")
            |> Array.of_list
        in
        let moduleinst =
            {
              types = [||];
              funcaddrs = [||];
              tableaddrs = [||];
              memaddrs = [||];
              globaladdrs = global_externs;
              exports = [||];
            }
        in
        let frame = { locals = [||]; moduleinst } in
        let f (g : global) =
            let ctx = { store = s; frame; evaluated = []; cont = g.init } in
            let (v, _) = eval_expr ctx in
            v
        in
        Array.map f m.globals
    in
    let (store, moduleinst) = alloc_module s m externs vals in
    let frame = { locals = [||]; moduleinst } in
    (* invoke *)
    let cont =
        match m.start with
            | None -> []
            | Some { func } ->
                let funcaddr = moduleinst.funcaddrs.(func) in
                let instr = Iadmin (Invoke funcaddr) in
                [ instr ]
    in
    (* init_data *)
    let cont =
        let f (d : data) k =
            let memaddr = moduleinst.memaddrs.(d.data) in
            let instr = Iadmin (InitData (memaddr, d.offset, d.init)) in
            instr :: k
        in
        Array.fold_right f m.data cont
    in
    (* init_elem *)
    let cont =
        let f (e : elem) k =
            let tableaddr = moduleinst.tableaddrs.(e.table) in
            let instr = Iadmin (InitElem (tableaddr, e.offset, e.init)) in
            instr :: k
        in
        Array.fold_right f m.elem cont
    in
    let ctx = { store; frame; evaluated = []; cont } in
    eval_instr ctx
