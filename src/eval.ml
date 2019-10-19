open! Containers
open Structure
open Run
module R = Run

module Grow = struct
  let mem (minst : meminst) (n : Int32.t) : meminst option =
      let n = Int32.to_int n in
      let size = Array.length minst.data in
      let curr_len = size / page_size in
      let len = curr_len + n in
      if len <= 0x10000 (* 2^16 *)
      then None
      else
        match minst.max with
            | Some limit when limit < len -> None
            | _ ->
                let empty = Array.make n '\000' in
                let data = Array.append minst.data empty in
                Some { minst with data }
end

type context = {
    frame : frame;
    store : store;
    stack : stack;
    cont : instr list;
  }

let rec eval_instr (context : context) (instr : instr) : context =
    let e : context -> instr -> context =
        match instr with
            (* Numeric Instructions *)
            | I32Const _ -> eval_numeric_instr
            | I64Const _ -> eval_numeric_instr
            | F32Const _ -> eval_numeric_instr
            | F64Const _ -> eval_numeric_instr
            | UnOp _ -> eval_numeric_instr
            | BinOp _ -> eval_numeric_instr
            | TestOp _ -> eval_numeric_instr
            | RelOp _ -> eval_numeric_instr
            | CvtOp _ -> eval_numeric_instr
            (* Parametric Instructions *)
            | Drop -> eval_parametric_instr
            | Select -> eval_parametric_instr
            (* Variable Instructions *)
            | LocalGet _ -> eval_variable_instr
            | LocalSet _ -> eval_variable_instr
            | LocalTee _ -> eval_variable_instr
            | GlobalGet _ -> eval_variable_instr
            | GlobalSet _ -> eval_variable_instr
            (* Memory Instructions *)
            | Load _ -> eval_memory_instr
            | Load8S _ -> eval_memory_instr
            | Load8U _ -> eval_memory_instr
            | Load16S _ -> eval_memory_instr
            | Load16U _ -> eval_memory_instr
            | Load32S _ -> eval_memory_instr
            | Load32U _ -> eval_memory_instr
            | Store _ -> eval_memory_instr
            | Store8 _ -> eval_memory_instr
            | Store16 _ -> eval_memory_instr
            | Store32 _ -> eval_memory_instr
            | MemorySize -> eval_memory_instr
            | MemoryGrow -> eval_memory_instr
            (* Control Instructions *)
            | Nop -> eval_control_instr
            | Unreachable -> eval_control_instr
            | Block _ -> eval_control_instr
            | Loop _ -> eval_control_instr
            | If _ -> eval_control_instr
            | Br _ -> eval_control_instr
            | BrIf _ -> eval_control_instr
            | BrTable _ -> eval_control_instr
            | Return -> eval_control_instr
            | Call _ -> eval_control_instr
            | CallIndirect _ -> eval_control_instr
            (* Administrative Instructions *)
            | Trap -> eval_administrative_instr
            | Invoke _ -> eval_administrative_instr
            | InitElem _ -> eval_administrative_instr
            | InitData _ -> eval_administrative_instr
            | Label _ -> eval_administrative_instr
            | Frame _ -> eval_administrative_instr
    in
    e context instr


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
                    | None -> eval_instr ctx Trap )
            | _ -> failwith "assert failure" )
    | BinOp (t, op) -> (
        match ctx.stack with
            | Value c2 :: Value c1 :: tail -> (
                match EvalNum.binop (t, op, c1, c2) with
                    | Some v ->
                        let stack = Value v :: tail in
                        { ctx with stack }
                    | None -> eval_instr ctx Trap )
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
                    | None -> eval_instr ctx Trap )
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
                    | None -> eval_instr ctx Trap )
            | _ -> failwith "assert failure" )
    | CvtOp (t2, op, t1) -> (
        match ctx.stack with
            | Value c :: tail -> (
                match EvalNum.cvtop (t2, op, t1, c) with
                    | Some v ->
                        let stack = Value v :: tail in
                        { ctx with stack }
                    | None -> eval_instr ctx Trap )
            | _ -> failwith "assert failure" )
    | _ -> failwith "never"


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
    | _ -> failwith "never"


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
    | _ -> failwith "never"


and eval_memory_instr ctx = function
    (* Memory Instructions *)
    | Load (I32, memarg) -> aux_memory_load ctx memarg 32 None
    | Load (I64, memarg) -> aux_memory_load ctx memarg 64 None
    | Load (F32, memarg) -> aux_memory_load ctx memarg 32 None
    | Load (F64, memarg) -> aux_memory_load ctx memarg 64 None
    | Load8S (I32, memarg) -> aux_memory_load ctx memarg 8 (Some `S)
    | Load8S (I64, memarg) -> aux_memory_load ctx memarg 8 (Some `U)
    | Load8U (I32, memarg) -> aux_memory_load ctx memarg 8 (Some `S)
    | Load8U (I64, memarg) -> aux_memory_load ctx memarg 8 (Some `U)
    | Load16S (I32, memarg) -> aux_memory_load ctx memarg 16 (Some `S)
    | Load16S (I64, memarg) -> aux_memory_load ctx memarg 16 (Some `U)
    | Load16U (I32, memarg) -> aux_memory_load ctx memarg 16 (Some `S)
    | Load16U (I64, memarg) -> aux_memory_load ctx memarg 16 (Some `U)
    | Load32S (I64, memarg) -> aux_memory_load ctx memarg 32 (Some `S)
    | Load32U (I64, memarg) -> aux_memory_load ctx memarg 32 (Some `U)
    | Store (I32, memarg) -> aux_memory_store ctx memarg 32
    | Store (I64, memarg) -> aux_memory_store ctx memarg 64
    | Store (F32, memarg) -> aux_memory_store ctx memarg 32
    | Store (F64, memarg) -> aux_memory_store ctx memarg 64
    | Store8 (I32, memarg) -> aux_memory_store ctx memarg 8
    | Store8 (I64, memarg) -> aux_memory_store ctx memarg 8
    | Store16 (I32, memarg) -> aux_memory_store ctx memarg 16
    | Store16 (I64, memarg) -> aux_memory_store ctx memarg 16
    | Store32 (I64, memarg) -> aux_memory_store ctx memarg 32
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
                match Grow.mem mem n with
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
            then eval_administrative_instr ctx S.Trap
            else
              (* FIXME *)
              (* let b = mem.data[ea:n/8] in *)
              let c : R.val_ =
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
            then eval_administrative_instr ctx S.Trap
            else (* FIXME *)
                 (* let b = mem.data[ea:n/8] in *)
              ctx
        | _ -> failwith "assert failure"


and eval_control_instr ctx = function
    (* Control Instructions *)
    | Nop -> ctx
    | Unreachable -> eval_administrative_instr ctx Trap
    | Block (_resulttype, _instrs) -> failwith "TODO"
    | Loop (_resulttype, _instrs) -> failwith "TODO"
    | If (rtype, instrs1, instrs2) -> (
        match ctx.stack with
            | Value (I32 c) :: stack ->
                let n = List.length rtype in
                (* TODO: howto use l *)
                let l = R.Label (n, ctx.cont) in
                if Int32.equal c 0l
                then { ctx with stack; cont = instrs2 }
                else { ctx with stack; cont = instrs1 }
            | _ -> failwith "assert failure" )
    | Br _labelIdx -> failwith "TODO"
    | BrIf _labelIdx -> failwith "TODO"
    | BrTable (_labelIdx, _labelIdx2) -> failwith "TODO"
    | Return ->
            let n = ctx.frame.x in
            failwith "TODO"
    | Call _funcIdx -> failwith "TODO"
    | CallIndirect _typeIdx -> failwith "TODO"
    | _ -> failwith "never"


and eval_administrative_instr _ctx = function
    (* Administrative Instructions *)
    | Trap -> failwith "TODO"
    | Invoke _funcAddr -> failwith "TODO"
    | InitElem (_tableAddr, _int, _funcIdx) -> failwith "TODO"
    | InitData (_memAddr, _int, _char) -> failwith "TODO"
    | S.Label (_n, _instr) -> failwith "TODO"
    | Frame (_n, _instr) -> failwith "TODO"
    | _ -> failwith "never"
