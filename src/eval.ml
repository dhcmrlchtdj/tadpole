open! Containers
module S = Structure
module R = Run

let rec eval_instr (env : R.store) (stack : R.stack)
    : S.instr -> R.store * R.stack
  = function
    (* Numeric Instructions *)
    | S.I32Const i -> (env, R.Value (R.I32 i) :: stack)
    | S.I64Const i -> (env, R.Value (R.I64 i) :: stack)
    | S.F32Const i -> (env, R.Value (R.F32 i) :: stack)
    | S.F64Const i -> (env, R.Value (R.F64 i) :: stack)
    | S.UnOp (t, op) -> (
        match stack with
            | R.Value c :: tail -> (
                match EvalNum.unop (t, op, c) with
                    | Some v ->
                        let entry = R.Value v in
                        (env, entry :: tail)
                    | None -> eval_instr env stack S.Trap )
            | _ -> failwith "assert failure" )
    | S.BinOp (t, op) -> (
        match stack with
            | R.Value c2 :: R.Value c1 :: tail -> (
                match EvalNum.binop (t, op, c1, c2) with
                    | Some v ->
                        let entry = R.Value v in
                        (env, entry :: tail)
                    | None -> eval_instr env stack S.Trap )
            | _ -> failwith "assert failure" )
    | S.TestOp (t, op) -> (
        match stack with
            | R.Value c :: tail -> (
                match EvalNum.testop (t, op, c) with
                    | Some true -> (env, R.Value (R.I32 1l) :: tail)
                    | Some false -> (env, R.Value (R.I32 0l) :: tail)
                    | None -> eval_instr env stack S.Trap )
            | _ -> failwith "assert failure" )
    | S.RelOp (t, op) -> (
        match stack with
            | R.Value c2 :: R.Value c1 :: tail -> (
                match EvalNum.relop (t, op, c1, c2) with
                    | Some true -> (env, R.Value (R.I32 1l) :: tail)
                    | Some false -> (env, R.Value (R.I32 0l) :: tail)
                    | None -> eval_instr env stack S.Trap )
            | _ -> failwith "assert failure" )
    | S.CvtOp (t2, op, t1) -> (
        match stack with
            | R.Value c :: tail -> (
                match EvalNum.cvtop (t2, op, t1, c) with
                    | Some v ->
                        let entry = R.Value v in
                        (env, entry :: tail)
                    | None -> eval_instr env stack S.Trap )
            | _ -> failwith "assert failure" )
    (* Parametric Instructions *)
    | S.Drop -> (
        match stack with
            | _ :: t -> (env, t)
            | _ -> failwith "assert failure" )
    | S.Select -> (
        match stack with
            | R.Value (R.I32 c) :: v2 :: v1 :: t ->
                let h = if Int32.equal c 0l then v2 else v1 in
                (env, h :: t)
            | _ -> failwith "assert failure" )
    (* Variable Instructions *)
    | S.LocalGet x -> failwith "TODO"
    | S.LocalSet x -> failwith "TODO"
    | S.LocalTee x -> (
        match stack with
            | h :: _ -> eval_instr env (h :: stack) (S.LocalSet x)
            | _ -> failwith "assert failure" )
    | S.GlobalGet x -> failwith "TODO"
    | S.GlobalSet x -> failwith "TODO"
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
    (* Control Instructions *)
    | S.Nop -> (env, stack)
    | S.Unreachable -> eval_instr env stack S.Trap
    | S.Block (resulttype, instrs) -> failwith "TODO"
    | S.Loop (resulttype, instrs) -> failwith "TODO"
    | S.If (resulttype, instrs, instr2) -> failwith "TODO"
    | S.Br labelIdx -> failwith "TODO"
    | S.BrIf labelIdx -> failwith "TODO"
    | S.BrTable (labelIdx, labelIdx2) -> failwith "TODO"
    | S.Return -> failwith "TODO"
    | S.Call funcIdx -> failwith "TODO"
    | S.CallIndirect typeIdx -> failwith "TODO"
    (* Administrative Instructions *)
    | S.Trap -> failwith "TODO"
    | S.Invoke funcAddr -> failwith "TODO"
    | S.InitElem (tableAddr, int, funcIdx) -> failwith "TODO"
    | S.InitData (memAddr, int, char) -> failwith "TODO"
    | S.Label (n, instr) -> failwith "TODO"
    | S.Frame (n, instr) -> failwith "TODO"


let eval_expr (store : R.store) (stack : R.stack) : S.expr -> R.store * R.stack
  = function
    | _ -> (store, stack)
