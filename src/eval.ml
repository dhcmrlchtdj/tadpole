open! Containers
module A = Ast
module R = Runtime

let rec eval_instr (env : R.store) (stack : R.stack)
    : A.instr -> R.store * R.stack
  = function
    (* Numeric Instructions *)
    | A.Const v ->
        let entry = R.Value v in
        (env, entry :: stack)
    | A.UnOp (op, t) -> (
        match stack with
            | R.Value c :: tail -> (
                match EvalNum.unop (op, t, c) with
                    | Some v ->
                        let entry = R.Value v in
                        (env, entry :: tail)
                    | None -> eval_instr env stack A.Trap )
            | _ -> failwith "assert failure" )
    | A.BinOp (op, t) -> (
        match stack with
            | R.Value c2 :: R.Value c1 :: tail -> (
                match EvalNum.binop (op, t, c1, c2) with
                    | Some v ->
                        let entry = R.Value v in
                        (env, entry :: tail)
                    | None -> eval_instr env stack A.Trap )
            | _ -> failwith "assert failure" )
    | A.TestOp (op, t) -> (
        match stack with
            | R.Value c :: tail -> (
                match EvalNum.testop (op, t, c) with
                    | Some true -> (env, R.Value (A.I32 1l) :: tail)
                    | Some false -> (env, R.Value (A.I32 0l) :: tail)
                    | None -> eval_instr env stack A.Trap )
            | _ -> failwith "assert failure" )
    | A.RelOp (op, t) -> (
        match stack with
            | R.Value c2 :: R.Value c1 :: tail -> (
                match EvalNum.relop (op, t, c1, c2) with
                    | Some true -> (env, R.Value (A.I32 1l) :: tail)
                    | Some false -> (env, R.Value (A.I32 0l) :: tail)
                    | None -> eval_instr env stack A.Trap )
            | _ -> failwith "assert failure" )
    | A.CvtOp (t2, op, t1) -> (
        match stack with
            | R.Value c :: tail -> (
                match EvalNum.cvtop (t2, op, t1, c) with
                    | Some v ->
                        let entry = R.Value v in
                        (env, entry :: tail)
                    | None -> eval_instr env stack A.Trap )
            | _ -> failwith "assert failure" )
    (* Parametric Instructions *)
    | A.Drop -> (
        match stack with
            | _ :: t -> (env, t)
            | _ -> failwith "assert failure" )
    | A.Select -> (
        match stack with
            | R.Value (A.I32 c) :: v2 :: v1 :: t ->
                let h = if Int32.equal c 0l then v2 else v1 in
                (env, h :: t)
            | _ -> failwith "assert failure" )
    (* Variable Instructions *)
    | A.LocalGet x -> failwith "TODO"
    | A.LocalSet x -> failwith "TODO"
    | A.LocalTee x -> (
        match stack with
            | h :: _ -> eval_instr env (h :: stack) (A.LocalSet x)
            | _ -> failwith "assert failure" )
    | A.GlobalGet x -> failwith "TODO"
    | A.GlobalSet x -> failwith "TODO"
    (* Memory Instructions *)
    | A.Load (memarg, valtype) -> failwith "TODO"
    | A.Load8S (memarg, valtype) -> failwith "TODO"
    | A.Load8U (memarg, valtype) -> failwith "TODO"
    | A.Load16S (memarg, valtype) -> failwith "TODO"
    | A.Load16U (memarg, valtype) -> failwith "TODO"
    | A.Load32S memarg -> failwith "TODO"
    | A.Load32U memarg -> failwith "TODO"
    | A.Store (memarg, valtype) -> failwith "TODO"
    | A.Store8 (memarg, valtype) -> failwith "TODO"
    | A.Store16 (memarg, valtype) -> failwith "TODO"
    | A.Store32 memarg -> failwith "TODO"
    | A.MemortSize -> failwith "TODO"
    | A.MemortGrow -> failwith "TODO"
    (* Control Instructions *)
    | A.Nop -> (env, stack)
    | A.Unreachable -> eval_instr env stack A.Trap
    | A.Block (resulttype, instrs) -> failwith "TODO"
    | A.Loop (resulttype, instrs) -> failwith "TODO"
    | A.If (resulttype, instrs, instr2) -> failwith "TODO"
    | A.Br labelIdx -> failwith "TODO"
    | A.BrIf labelIdx -> failwith "TODO"
    | A.BrTable (labelIdx, labelIdx2) -> failwith "TODO"
    | A.Return -> failwith "TODO"
    | A.Call funcIdx -> failwith "TODO"
    | A.CallIndirect typeIdx -> failwith "TODO"
    (* Administrative Instructions *)
    | A.Trap -> failwith "TODO"
    | A.Invoke funcAddr -> failwith "TODO"
    | A.InitElem (tableAddr, int, funcIdx) -> failwith "TODO"
    | A.InitData (memAddr, int, char) -> failwith "TODO"
    | A.Label instr -> failwith "TODO"
    | A.Frame instr -> failwith "TODO"

let eval_expr (store : R.store) (stack : R.stack) : A.expr -> R.store * R.stack
  = function
    | _ -> (store, stack)
