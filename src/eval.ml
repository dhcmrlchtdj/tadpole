open! Containers
open Run
module S = Structure

type context = {
    frame : frame;
    store : store;
    stack : stack;
  }

let rec eval_instr (context : context) (instr : S.instr) : context =
    let e : context -> S.instr -> context =
        match instr with
            (* Numeric Instructions *)
            | S.I32Const _ -> eval_numeric_instr
            | S.I64Const _ -> eval_numeric_instr
            | S.F32Const _ -> eval_numeric_instr
            | S.F64Const _ -> eval_numeric_instr
            | S.UnOp _ -> eval_numeric_instr
            | S.BinOp _ -> eval_numeric_instr
            | S.TestOp _ -> eval_numeric_instr
            | S.RelOp _ -> eval_numeric_instr
            | S.CvtOp _ -> eval_numeric_instr
            (* Parametric Instructions *)
            | S.Drop -> eval_parametric_instr
            | S.Select -> eval_parametric_instr
            (* Variable Instructions *)
            | S.LocalGet _ -> eval_variable_instr
            | S.LocalSet _ -> eval_variable_instr
            | S.LocalTee _ -> eval_variable_instr
            | S.GlobalGet _ -> eval_variable_instr
            | S.GlobalSet _ -> eval_variable_instr
            (* Memory Instructions *)
            | S.Load _ -> eval_memory_instr
            | S.Load8S _ -> eval_memory_instr
            | S.Load8U _ -> eval_memory_instr
            | S.Load16S _ -> eval_memory_instr
            | S.Load16U _ -> eval_memory_instr
            | S.Load32S _ -> eval_memory_instr
            | S.Load32U _ -> eval_memory_instr
            | S.Store _ -> eval_memory_instr
            | S.Store8 _ -> eval_memory_instr
            | S.Store16 _ -> eval_memory_instr
            | S.Store32 _ -> eval_memory_instr
            | S.MemortSize -> eval_memory_instr
            | S.MemortGrow -> eval_memory_instr
            (* Control Instructions *)
            | S.Nop -> eval_control_instr
            | S.Unreachable -> eval_control_instr
            | S.Block _ -> eval_control_instr
            | S.Loop _ -> eval_control_instr
            | S.If _ -> eval_control_instr
            | S.Br _ -> eval_control_instr
            | S.BrIf _ -> eval_control_instr
            | S.BrTable _ -> eval_control_instr
            | S.Return -> eval_control_instr
            | S.Call _ -> eval_control_instr
            | S.CallIndirect _ -> eval_control_instr
            (* Administrative Instructions *)
            | S.Trap -> eval_administrative_instr
            | S.Invoke _ -> eval_administrative_instr
            | S.InitElem _ -> eval_administrative_instr
            | S.InitData _ -> eval_administrative_instr
            | S.Label _ -> eval_administrative_instr
            | S.Frame _ -> eval_administrative_instr
    in
    e context instr


and eval_numeric_instr ctx = function
    | S.I32Const i ->
        let stack = Value (I32 i) :: ctx.stack in
        { ctx with stack }
    | S.I64Const i ->
        let stack = Value (I64 i) :: ctx.stack in
        { ctx with stack }
    | S.F32Const i ->
        let stack = Value (F32 i) :: ctx.stack in
        { ctx with stack }
    | S.F64Const i ->
        let stack = Value (F64 i) :: ctx.stack in
        { ctx with stack }
    | S.UnOp (t, op) -> (
        match ctx.stack with
            | Value c :: tail -> (
                match EvalNum.unop (t, op, c) with
                    | Some v ->
                        let stack = Value v :: tail in
                        { ctx with stack }
                    | None -> eval_instr ctx S.Trap )
            | _ -> failwith "assert failure" )
    | S.BinOp (t, op) -> (
        match ctx.stack with
            | Value c2 :: Value c1 :: tail -> (
                match EvalNum.binop (t, op, c1, c2) with
                    | Some v ->
                        let stack = Value v :: tail in
                        { ctx with stack }
                    | None -> eval_instr ctx S.Trap )
            | _ -> failwith "assert failure" )
    | S.TestOp (t, op) -> (
        match ctx.stack with
            | Value c :: tail -> (
                match EvalNum.testop (t, op, c) with
                    | Some true ->
                        let stack = Value (I32 1l) :: tail in
                        { ctx with stack }
                    | Some false ->
                        let stack = Value (I32 0l) :: tail in
                        { ctx with stack }
                    | None -> eval_instr ctx S.Trap )
            | _ -> failwith "assert failure" )
    | S.RelOp (t, op) -> (
        match ctx.stack with
            | Value c2 :: Value c1 :: tail -> (
                match EvalNum.relop (t, op, c1, c2) with
                    | Some true ->
                        let stack = Value (I32 1l) :: tail in
                        { ctx with stack }
                    | Some false ->
                        let stack = Value (I32 0l) :: tail in
                        { ctx with stack }
                    | None -> eval_instr ctx S.Trap )
            | _ -> failwith "assert failure" )
    | S.CvtOp (t2, op, t1) -> (
        match ctx.stack with
            | Value c :: tail -> (
                match EvalNum.cvtop (t2, op, t1, c) with
                    | Some v ->
                        let stack = Value v :: tail in
                        { ctx with stack }
                    | None -> eval_instr ctx S.Trap )
            | _ -> failwith "assert failure" )
    | _ -> failwith "never"


and eval_parametric_instr ctx = function
    | S.Drop -> (
        match ctx.stack with
            | _ :: stack -> { ctx with stack }
            | _ -> failwith "assert failure" )
    | S.Select -> (
        match ctx.stack with
            | Value (I32 c) :: v2 :: v1 :: t ->
                let h = if Int32.equal c 0l then v2 else v1 in
                let stack = h :: t in
                { ctx with stack }
            | _ -> failwith "assert failure" )
    | _ -> failwith "never"


and eval_variable_instr ctx = function
    | S.LocalGet x ->
        let f = ctx.frame in
        let v = List.get_at_idx_exn x f.locals in
        let stack = Value v :: ctx.stack in
        { ctx with stack }
    | S.LocalSet x -> (
        let f = ctx.frame in
        match ctx.stack with
            | Value v :: t ->
                let locals = List.set_at_idx x v f.locals in
                let frame = { ctx.frame with locals } in
                { ctx with frame }
            | _ -> failwith "assert failure" )
    | S.LocalTee x -> (
        match ctx.stack with
            | h :: _ ->
                let stack = h :: ctx.stack in
                let ctx2 = { ctx with stack } in
                eval_variable_instr ctx2 (S.LocalSet x)
            | _ -> failwith "assert failure" )
    | S.GlobalGet x ->
        let addr = ctx.frame.module_.globaladdrs in
        let gidx = List.get_at_idx_exn x addr in
        let glob = List.get_at_idx_exn gidx ctx.store.globals in
        let stack = Value glob.value :: ctx.stack in
        { ctx with stack }
    | S.GlobalSet x -> (
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
    | S.Load (memarg, valtype) -> failwith "TODO"
    | S.Load8S (memarg, valtype) -> failwith "TODO"
    | S.Load8U (memarg, valtype) -> failwith "TODO"
    | S.Load16S (memarg, valtype) -> failwith "TODO"
    | S.Load16U (memarg, valtype) -> failwith "TODO"
    | S.Load32S (_, memarg) -> failwith "TODO"
    | S.Load32U (_, memarg) -> failwith "TODO"
    | S.Store (memarg, valtype) -> failwith "TODO"
    | S.Store8 (memarg, valtype) -> failwith "TODO"
    | S.Store16 (memarg, valtype) -> failwith "TODO"
    | S.Store32 (_, memarg) -> failwith "TODO"
    | S.MemortSize -> failwith "TODO"
    | S.MemortGrow -> failwith "TODO"
    | _ -> failwith "never"


and eval_control_instr ctx = function
    (* Control Instructions *)
    | S.Nop -> ctx
    | S.Unreachable -> eval_administrative_instr ctx S.Trap
    | S.Block (resulttype, instrs) -> failwith "TODO"
    | S.Loop (resulttype, instrs) -> failwith "TODO"
    | S.If (resulttype, instrs, instr2) -> failwith "TODO"
    | S.Br labelIdx -> failwith "TODO"
    | S.BrIf labelIdx -> failwith "TODO"
    | S.BrTable (labelIdx, labelIdx2) -> failwith "TODO"
    | S.Return -> failwith "TODO"
    | S.Call funcIdx -> failwith "TODO"
    | S.CallIndirect typeIdx -> failwith "TODO"
    | _ -> failwith "never"


and eval_administrative_instr ctx = function
    (* Administrative Instructions *)
    | S.Trap -> failwith "TODO"
    | S.Invoke funcAddr -> failwith "TODO"
    | S.InitElem (tableAddr, int, funcIdx) -> failwith "TODO"
    | S.InitData (memAddr, int, char) -> failwith "TODO"
    | S.Label (n, instr) -> failwith "TODO"
    | S.Frame (n, instr) -> failwith "TODO"
    | _ -> failwith "never"
