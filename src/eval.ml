open Types

let rec eval_expressions (ctx : context) : value * context =
    let ctx2 = eval_instr ctx in
    match ctx2.stack with
        | Value v :: _ -> (v, ctx2)
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


and eval_numeric_instr (ctx : context) = function
    | _ -> ctx


and eval_parametric_instr (ctx : context) = function
    | _ -> ctx


and eval_variable_instr (ctx : context) = function
    | _ -> ctx


and eval_memory_instr (ctx : context) = function
    | _ -> ctx


and eval_control_instr (ctx : context) = function
    | _ -> ctx


and eval_admin_instr (ctx : context) = function
    | _ -> ctx


(* ******** *)

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
      let stack_values = values |> List.map (fun v -> Value v) |> List.rev in
      let stack = Instr (Iadmin (Invoke f)) :: stack_values in
      let ctx = eval_instr { store; frame; stack } in
      aux_take_m [] (List.length rets) ctx.stack
    else failwith "TODO: argument error"


and aux_type_eq params args =
    let test (p : valtype) (a : value) =
        match (p, a) with
            | TI32, I32 _ | TI64, I64 _ | TF32, F32 _ | TF64, F64 _ -> true
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
