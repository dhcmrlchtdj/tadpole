open! Containers
open Types

type context = {
    stack : stack;
    store : store;
    frame : frame;
    cont : instr list;
  }

let rec eval_instr (ctx : context) = function
    | Inumeric i -> eval_numeric_instr ctx i
    | Iparametric i -> eval_parametric_instr ctx i
    | Ivariable i -> eval_variable_instr ctx i
    | Imemory i -> eval_memory_instr ctx i
    | Icontrol i -> eval_control_instr ctx i
    | Iadmin i -> eval_admin_instr ctx i


and eval_numeric_instr ctx = function
    | I32Const i ->
        let stack = Value (I32 i) :: ctx.stack in
        { ctx with stack }
    | I64Const i ->
        let stack = Value (I64 i) :: ctx.stack in
        { ctx with stack }
    | F32Const i ->
        let stack = Value (F32 i) :: ctx.stack in
        { ctx with stack }
    | F64Const i ->
        let stack = Value (F64 i) :: ctx.stack in
        { ctx with stack }
    | UnOp (t, op) -> (
        match ctx.stack with
            | Value c :: tail -> (
                match EvalNum.unop (t, op, c) with
                    | Some v ->
                        let stack = Value v :: tail in
                        { ctx with stack }
                    | None -> eval_admin_instr ctx Trap )
            | _ -> failwith "assert failure" )
    | BinOp (t, op) -> (
        match ctx.stack with
            | Value c2 :: Value c1 :: tail -> (
                match EvalNum.binop (t, op, c1, c2) with
                    | Some v ->
                        let stack = Value v :: tail in
                        { ctx with stack }
                    | None -> eval_admin_instr ctx Trap )
            | _ -> failwith "assert failure" )
    | TestOp (t, op) -> (
        match ctx.stack with
            | Value c :: tail -> (
                match EvalNum.testop (t, op, c) with
                    | Some true ->
                        let stack = Value (I32 1l) :: tail in
                        { ctx with stack }
                    | Some false ->
                        let stack = Value (I32 0l) :: tail in
                        { ctx with stack }
                    | None -> eval_admin_instr ctx Trap )
            | _ -> failwith "assert failure" )
    | RelOp (t, op) -> (
        match ctx.stack with
            | Value c2 :: Value c1 :: tail -> (
                match EvalNum.relop (t, op, c1, c2) with
                    | Some true ->
                        let stack = Value (I32 1l) :: tail in
                        { ctx with stack }
                    | Some false ->
                        let stack = Value (I32 0l) :: tail in
                        { ctx with stack }
                    | None -> eval_admin_instr ctx Trap )
            | _ -> failwith "assert failure" )
    | CvtOp (t2, op, t1) -> (
        match ctx.stack with
            | Value c :: tail -> (
                match EvalNum.cvtop (t2, op, t1, c) with
                    | Some v ->
                        let stack = Value v :: tail in
                        { ctx with stack }
                    | None -> eval_admin_instr ctx Trap )
            | _ -> failwith "assert failure" )


and eval_parametric_instr ctx = function
    | Drop -> (
        match ctx.stack with
            | _ :: stack -> { ctx with stack }
            | _ -> failwith "assert failure" )
    | Select -> (
        match ctx.stack with
            | Value (I32 c) :: v2 :: v1 :: t ->
                let h = if Int32.equal c 0l then v2 else v1 in
                let stack = h :: t in
                { ctx with stack }
            | _ -> failwith "assert failure" )


and eval_variable_instr ctx = function
    | LocalGet x ->
        let f = ctx.frame in
        let v = List.get_at_idx_exn x f.locals in
        let stack = Value v :: ctx.stack in
        { ctx with stack }
    | LocalSet x -> (
        let f = ctx.frame in
        match ctx.stack with
            | Value v :: _ ->
                let locals = List.set_at_idx x v f.locals in
                let frame = { ctx.frame with locals } in
                { ctx with frame }
            | _ -> failwith "assert failure" )
    | LocalTee x -> (
        match ctx.stack with
            | h :: _ ->
                let stack = h :: ctx.stack in
                let ctx2 = { ctx with stack } in
                eval_variable_instr ctx2 (LocalSet x)
            | _ -> failwith "assert failure" )
    | GlobalGet x ->
        let addr = ctx.frame.module_.globaladdrs in
        let gidx = List.get_at_idx_exn x addr in
        let glob = List.get_at_idx_exn gidx ctx.store.globals in
        let stack = Value glob.value :: ctx.stack in
        { ctx with stack }
    | GlobalSet x -> (
        let g_addrs = ctx.frame.module_.globaladdrs in
        let g_insts = ctx.store.globals in
        let gidx = List.get_at_idx_exn x g_addrs in
        let glob = List.get_at_idx_exn gidx g_insts in
        match ctx.stack with
            | Value value :: stack ->
                let g2 = { glob with value } in
                let globals = List.set_at_idx gidx g2 g_insts in
                let store = { ctx.store with globals } in
                { ctx with store; stack }
            | _ -> failwith "assert failure" )


(* TODO *)
and eval_memory_instr ctx = function
    (* Memory Instructions *)
    | Load (TI32, memarg) -> aux_memory_load ctx memarg 32 None
    | Load (TI64, memarg) -> aux_memory_load ctx memarg 64 None
    | Load (TF32, memarg) -> aux_memory_load ctx memarg 32 None
    | Load (TF64, memarg) -> aux_memory_load ctx memarg 64 None
    | Load8S (TI32, memarg) -> aux_memory_load ctx memarg 8 (Some `S)
    | Load8S (TI64, memarg) -> aux_memory_load ctx memarg 8 (Some `U)
    | Load8U (TI32, memarg) -> aux_memory_load ctx memarg 8 (Some `S)
    | Load8U (TI64, memarg) -> aux_memory_load ctx memarg 8 (Some `U)
    | Load16S (TI32, memarg) -> aux_memory_load ctx memarg 16 (Some `S)
    | Load16S (TI64, memarg) -> aux_memory_load ctx memarg 16 (Some `U)
    | Load16U (TI32, memarg) -> aux_memory_load ctx memarg 16 (Some `S)
    | Load16U (TI64, memarg) -> aux_memory_load ctx memarg 16 (Some `U)
    | Load32S (TI64, memarg) -> aux_memory_load ctx memarg 32 (Some `S)
    | Load32U (TI64, memarg) -> aux_memory_load ctx memarg 32 (Some `U)
    | Store (TI32, memarg) -> aux_memory_store ctx memarg 32
    | Store (TI64, memarg) -> aux_memory_store ctx memarg 64
    | Store (TF32, memarg) -> aux_memory_store ctx memarg 32
    | Store (TF64, memarg) -> aux_memory_store ctx memarg 64
    | Store8 (TI32, memarg) -> aux_memory_store ctx memarg 8
    | Store8 (TI64, memarg) -> aux_memory_store ctx memarg 8
    | Store16 (TI32, memarg) -> aux_memory_store ctx memarg 16
    | Store16 (TI64, memarg) -> aux_memory_store ctx memarg 16
    | Store32 (TI64, memarg) -> aux_memory_store ctx memarg 32
    | MemorySize ->
        let m_addrs = ctx.frame.module_.memaddrs in
        let m_idx = List.get_at_idx_exn 0 m_addrs in
        let mem = List.get_at_idx_exn m_idx ctx.store.mems in
        let len = Array.length mem.data in
        let sz = Int32.of_int (len / page_size) in
        let stack = Value (I32 sz) :: ctx.stack in
        { ctx with stack }
    | MemoryGrow -> (
        let m_addrs = ctx.frame.module_.memaddrs in
        let m_idx = List.get_at_idx_exn 0 m_addrs in
        let m_insts = ctx.store.mems in
        let mem = List.get_at_idx_exn m_idx m_insts in
        let len = Array.length mem.data in
        let sz = Int32.of_int (len / page_size) in
        match ctx.stack with
            | Value (I32 n) :: stack -> (
                match Alloc.grow_mem mem (Int32.to_int n) with
                    | Some m2 ->
                        let mems = List.set_at_idx m_idx m2 m_insts in
                        let store = { ctx.store with mems } in
                        let stack = Value (I32 sz) :: stack in
                        { ctx with store; stack }
                    | None ->
                        let stack = Value (I32 (-1l)) :: ctx.stack in
                        { ctx with stack } )
            | _ -> failwith "assert failure" )
    | _ -> failwith "never"


and aux_memory_load ctx (memarg : memarg) bit_with (sign : [ `S | `U ] option) =
    let m_addrs = ctx.frame.module_.memaddrs in
    let m_idx = List.get_at_idx_exn 0 m_addrs in
    let m_insts = ctx.store.mems in
    let mem = List.get_at_idx_exn m_idx m_insts in
    let mem_len = Array.length mem.data in
    match ctx.stack with
        | Value (I32 i) :: s2 ->
            let ea = Int32.to_int i + memarg.offset in
            let len = bit_with / 8 in
            if ea + len > mem_len
            then eval_admin_instr ctx Trap
            else
              (* FIXME *)
              (* let b = mem.data[ea:n/8] in *)
              let c : val_ =
                  match sign with
                      | None -> I32 1l
                      | Some `U -> I32 1l
                      | Some `S -> I32 1l
              in
              let stack = Value c :: s2 in
              { ctx with stack }
        | _ -> failwith "assert failure"


and aux_memory_store ctx (memarg : memarg) bit_with =
    let m_addrs = ctx.frame.module_.memaddrs in
    let m_idx = List.get_at_idx_exn 0 m_addrs in
    let m_insts = ctx.store.mems in
    let mem = List.get_at_idx_exn m_idx m_insts in
    let mem_len = Array.length mem.data in
    (* FIXME read c *)
    match ctx.stack with
        | Value (I32 i) :: s2 ->
            let ea = Int32.to_int i + memarg.offset in
            let len = bit_with / 8 in
            if ea + len > mem_len
            then eval_admin_instr ctx Trap
            else (* FIXME *)
                 (* let b = mem.data[ea:n/8] in *)
              ctx
        | _ -> failwith "assert failure"


and eval_control_instr ctx = function
    (* Control Instructions *)
    | Nop -> ctx
    | Unreachable -> eval_admin_instr ctx Trap
    | Block (rtypes, instrs) ->
        let n = List.length rtypes in
        let l = Label (n, [], instrs) in
        failwith "TODO"
    | Loop (rtypes, instrs) ->
        let sub = Loop (rtypes, instrs) in
        (* let l = Label (0, [ sub ], instrs) in *)
        failwith "TODO"
    | If (rtypes, instrs1, instrs2) -> (
        match ctx.stack with
            | Value (I32 c) :: stack ->
                let n = List.length rtypes in
                (* let l = Label (n, ctx.cont) in *)
                if Int32.equal c 0l
                then { ctx with stack; cont = instrs2 }
                else { ctx with stack; cont = instrs1 }
            | _ -> failwith "assert failure" )
    | Br _labelIdx -> failwith "TODO"
    | BrIf _labelIdx -> failwith "TODO"
    | BrTable (_labelIdx, _labelIdx2) -> failwith "TODO"
    | Return ->
        (* let n = ctx.frame.x in *)
        failwith "TODO"
    | Call _funcIdx -> failwith "TODO"
    | CallIndirect _typeIdx -> failwith "TODO"


and eval_admin_instr _ctx = function
    (* Administrative Instructions *)
    | Trap -> failwith "TODO"
    | Invoke _funcAddr -> failwith "TODO"
    | InitElem (_tableAddr, _int, _funcIdx) -> failwith "TODO"
    | InitData (_memAddr, _int, _char) -> failwith "TODO"
    | Label (_n, _, _instr) -> failwith "TODO"
    | Frame (_n, _, _instr) -> failwith "TODO"
