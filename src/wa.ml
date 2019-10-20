open Types

let rec eval_expressions (ctx : context) : value * context =
    let ctx2 = eval_instr ctx in
    match ctx2.stack with
        | Value v :: t -> (v, ctx2)
        | _ -> failwith "assert failure"


and eval_instr (ctx : context) : context =
    match ctx.stack with
        | Instr (Inumeric i) :: stack ->
            eval_numeric_instr { ctx with stack } i
        | Instr (Iparametric i) :: stack ->
            eval_parametric_instr { ctx with stack } i
        | Instr (Ivariable i) :: stack ->
            eval_variable_instr { ctx with stack } i
        | Instr (Imemory i) :: stack -> eval_memory_instr { ctx with stack } i
        | Instr (Icontrol i) :: stack ->
            eval_control_instr { ctx with stack } i
        | Instr (Iadmin i) :: stack -> eval_admin_instr { ctx with stack } i
        | _ -> failwith ""


and eval_numeric_instr (ctx : context) = ctx

and eval_parametric_instr (ctx : context) = ctx

and eval_variable_instr (ctx : context) = ctx

and eval_memory_instr (ctx : context) = ctx

and eval_control_instr (ctx : context) = ctx

and eval_admin_instr (ctx : context) = ctx
