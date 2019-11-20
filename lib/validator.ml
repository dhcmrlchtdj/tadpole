open! Containers
open Types

let ( let* ) = Result.( >>= )

type 'a or_err = ('a, string) result

type pass_or_err = unit or_err

let pass : pass_or_err = Ok ()

type context = {
  types: functype array;
  funcs: functype array;
  tables: tabletype array;
  mems: memtype array;
  globals: globaltype array;
  locals: valtype array;
  labels: resulttype array;
  return: resulttype option;
}

let aux_idx (arr : 'a array) (i : idx) : 'a or_err =
  if i >= Array.length arr then Error "idx out of range" else Ok arr.(i)

module Type = struct
  let limits (k : u32) ({ min; max } : limits) =
    if min > k
    then Error "limits | min > k"
    else (
      match max with
      | Some max when min > max -> Error "limits | min > max"
      | Some max when max > k -> Error "limits | max > k"
      | Some _ -> pass
      | None -> pass
    )

  let functype ((_, rs) : functype) =
    if List.length rs > 1
    then Error "functype | result must not be larger than 1"
    else pass

  let tabletype ((l, _) : tabletype) = limits 0x1_0000_0000 l

  let memtype x = limits 0x1_0000 x

  let globaltype (_ : globaltype) = pass
end

module Instruction = struct
  let rec instrs
      (ctx : context)
      (ins : instr list)
      (l : valtype list)
      (r : valtype list)
    =
    match ins with
    | [] ->
      if Types.equal_resulttype l r
      then pass
      else Error "instrs | does not match"
    | Inumeric i :: t -> numeric ctx t l r i
    | Iparametric i :: t -> parametric ctx t l r i
    | Ivariable i :: t -> variable ctx t l r i
    | Imemory i :: t -> memory ctx t l r i
    | Icontrol i :: t -> control ctx t l r i
    | Iadmin _ :: _ -> Error "instr | never Iadmin"

  and numeric ctx ins l r = function
    | Const (I32 _) -> instrs ctx ins (TI32 :: l) r
    | Const (I64 _) -> instrs ctx ins (TI64 :: l) r
    | Const (F32 _) -> instrs ctx ins (TF32 :: l) r
    | Const (F64 _) -> instrs ctx ins (TF64 :: l) r
    | UnOp (t, _) -> (
      match l with
      | h :: _ when Types.equal_valtype h t -> instrs ctx ins l r
      | _ -> Error "UnOp | invalid"
    )
    | BinOp (t, _) -> (
      match l with
      | h2 :: h1 :: tt when Types.equal_valtype h1 t && Types.equal_valtype h2 t
        -> instrs ctx ins (t :: tt) r
      | _ -> Error "BinOp | invalid"
    )
    | TestOp (t, _) -> (
      match l with
      | h :: tt when Types.equal_valtype h t -> instrs ctx ins (TI32 :: tt) r
      | _ -> Error "TestOp | invalid"
    )
    | RelOp (t, _) -> (
      match l with
      | h2 :: h1 :: tt when Types.equal_valtype h1 t && Types.equal_valtype h2 t
        -> instrs ctx ins (TI32 :: tt) r
      | _ -> Error "RelOp | invalid"
    )
    | CvtOp (t2, _, t1) -> (
      match l with
      | h :: tt when Types.equal_valtype h t1 -> instrs ctx ins (t2 :: tt) r
      | _ -> Error "CvtOp | invalid"
    )

  and parametric ctx ins l r = function
    | Drop -> (
      match l with
      | _ :: t -> instrs ctx ins t r
      | [] -> Error "Drop | invalid"
    )
    | Select -> (
      match l with
      | TI32 :: t2 :: t1 :: t ->
        let foldf _ curr =
          let res = instrs ctx ins curr r in
          match res with
          | Ok _ -> (res, `Stop)
          | Error _ -> (res, `Continue)
        in
        List.fold_while foldf (Error "Select | invalid") [ t1 :: t; t2 :: t ]
      | _ -> Error "Select | invalid"
    )

  and variable ctx ins l r = function
    | LocalGet i ->
      let* x = aux_idx ctx.locals i in
      instrs ctx ins (x :: l) r
    | LocalSet i -> (
      let* x = aux_idx ctx.locals i in
      match l with
      | h :: t when Types.equal_valtype x h -> instrs ctx ins t r
      | _ -> Error "LocalSet | invalid"
    )
    | LocalTee i -> (
      let* x = aux_idx ctx.locals i in
      match l with
      | h :: _ when Types.equal_valtype x h -> instrs ctx ins l r
      | _ -> Error "LocalSet | invalid"
    )
    | GlobalGet i ->
      let* (_, x) = aux_idx ctx.globals i in
      instrs ctx ins (x :: l) r
    | GlobalSet i -> (
      let* x = aux_idx ctx.globals i in
      match x with
      | (CONST, _) -> Error "GlobalSet | invalid | const"
      | (VAR, x) -> (
        match l with
        | h :: t when Types.equal_valtype x h -> instrs ctx ins t r
        | _ -> Error "GlobalSet | invalid"
      )
    )

  and memory ctx ins l r =
    let aux_load max_align t memarg =
      let* _ = aux_idx ctx.mems 0 in
      if memarg.align > max_align
      then Error "Load | invalid | align"
      else (
        match l with
        | TI32 :: tt -> instrs ctx ins (t :: tt) r
        | _ -> Error "Load | invalid"
      )
    and aux_store max_align memarg =
      let* _ = aux_idx ctx.mems 0 in
      if memarg.align > max_align
      then Error "Store | invalid | align"
      else (
        match l with
        | _ :: TI32 :: tt -> instrs ctx ins tt r
        | _ -> Error "Store | invalid"
      )
    in
    function
      | Load8S (t, memarg) | Load8U (t, memarg) -> aux_load 0 t memarg
      | Load16S (t, memarg) | Load16U (t, memarg) -> aux_load 1 t memarg
      | Load32S (t, memarg) | Load32U (t, memarg) -> aux_load 2 t memarg
      | Load (TI32, memarg) -> aux_load 2 TI32 memarg
      | Load (TF32, memarg) -> aux_load 2 TF32 memarg
      | Load (TI64, memarg) -> aux_load 3 TI64 memarg
      | Load (TF64, memarg) -> aux_load 3 TF64 memarg
      | Store8 (_, memarg) -> aux_store 0 memarg
      | Store16 (_, memarg) -> aux_store 1 memarg
      | Store32 (_, memarg) -> aux_store 2 memarg
      | Store (TI32, memarg) -> aux_store 2 memarg
      | Store (TF32, memarg) -> aux_store 2 memarg
      | Store (TI64, memarg) -> aux_store 3 memarg
      | Store (TF64, memarg) -> aux_store 3 memarg
      | MemorySize ->
        let* _ = aux_idx ctx.mems 0 in
        instrs ctx ins (TI32 :: l) r
      | MemoryGrow -> (
        let* _ = aux_idx ctx.mems 0 in
        match l with
        | TI32 :: _ -> instrs ctx ins l r
        | _ -> Error "MemorrGrow"
      )

  and control ctx ins l r = function
    | Nop -> instrs ctx ins l r
    | Unreachable -> Error "Unreachable"
    | Block (rt, ins) ->
      let labels = Array.concat [ [| rt |]; ctx.labels ] in
      let newctx = { ctx with labels } in
      let* () = instrs newctx ins [] rt in
      instrs ctx ins (List.rev rt @ l) r
    | Loop (rt, ins) ->
      let labels = Array.concat [ [| rt |]; ctx.labels ] in
      let newctx = { ctx with labels } in
      let* () = instrs newctx ins [] rt in
      instrs ctx ins (List.rev rt @ l) r
    | If (rt, ins1, ins2) -> (
      let labels = Array.concat [ [| rt |]; ctx.labels ] in
      let newctx = { ctx with labels } in
      let* () = instrs newctx ins1 [] rt in
      let* () = instrs newctx ins2 [] rt in
      match l with
      | TI32 :: t -> instrs newctx ins1 (List.rev rt @ t) r
      | _ -> Error "If"
    )
    | Br label ->
      let* rt = aux_idx ctx.labels label in
      let rec aux = function
        | ([], _) -> pass
        | (a :: at, b :: bt) when Types.equal_valtype a b -> aux (at, bt)
        | _ -> Error "Br"
      in
      aux (List.rev rt, l)
    | BrIf label -> (
      let* rt = aux_idx ctx.labels label in
      match l with
      | TI32 :: t ->
        let rec aux = function
          | ([], bt) -> instrs ctx ins (List.rev rt @ bt) r
          | (a :: at, b :: bt) when Types.equal_valtype a b -> aux (at, bt)
          | _ -> Error "Br"
        in
        aux (List.rev rt, t)
      | _ -> Error "BrIf"
    )
    | BrTable (ls, label) -> (
      let* rt = aux_idx ctx.labels label in
      let* _lss =
        let mapf x =
          let* xrt = aux_idx ctx.labels x in
          if Types.equal_resulttype rt xrt
          then Ok xrt
          else Error "BrTable | labels"
        in
        ls |> Array.to_list |> List.map mapf |> Result.flatten_l
      in
      match l with
      | TI32 :: t ->
        let rec aux = function
          | ([], _) -> pass
          | (a :: at, b :: bt) when Types.equal_valtype a b -> aux (at, bt)
          | _ -> Error "Br"
        in
        aux (List.rev rt, t)
      | _ -> Error ""
    )
    | Return ->
      let* rt = Result.of_opt ctx.return in
      let rec aux = function
        | ([], _) -> pass
        | (a :: at, b :: bt) when Types.equal_valtype a b -> aux (at, bt)
        | _ -> Error "Return"
      in
      aux (List.rev rt, l)
    | Call x ->
      let* (p, rt) = aux_idx ctx.funcs x in
      let rec aux = function
        | ([], bt) -> instrs ctx ins (List.rev rt @ bt) r
        | (a :: at, b :: bt) when Types.equal_valtype a b -> aux (at, bt)
        | _ -> Error "Call"
      in
      aux (List.rev p, l)
    | CallIndirect x -> (
      let* _ = aux_idx ctx.tables 0 in
      let* (p, rt) = aux_idx ctx.types x in
      match l with
      | TI32 :: t ->
        let rec aux = function
          | ([], bt) -> instrs ctx ins (List.rev rt @ bt) r
          | (a :: at, b :: bt) when Types.equal_valtype a b -> aux (at, bt)
          | _ -> Error "Br"
        in
        aux (List.rev p, t)
      | _ -> Error "CallIndirect"
    )

  let expr (ctx : context) (rt : resulttype) (ins : instr list) : pass_or_err =
    instrs ctx ins [] rt

  let expr_const (ctx : context) (rt : resulttype) (ins : instr list) =
    let aux_is_const (ctx : context) (i : instr) =
      match i with
      | Inumeric (Const _) -> true
      | Ivariable (GlobalGet i) -> (
        let g = aux_idx ctx.globals i in
        match g with
        | Ok (CONST, _) -> true
        | _ -> false
      )
      | _ -> false
    in
    let all_const = List.for_all (aux_is_const ctx) ins in
    if all_const then expr ctx rt ins else Error "expr_const"
end

module Module = struct
  let aux_validate (f : 'a -> pass_or_err) (arr : 'a array) =
    let fold_func _ curr =
      match f curr with
      | Ok () -> (pass, `Continue)
      | Error e -> (Error e, `Stop)
    in
    Array.fold_while fold_func pass arr

  let validate_types (_ctx : context) (m : moduledef) =
    let f = Type.functype in
    aux_validate f m.types

  let validate_funcs (ctx : context) (m : moduledef) =
    let f { typei; locals; body } =
      let* ft = aux_idx ctx.types typei in
      let (p, rt) = ft in
      let newctx =
        {
          ctx with
          locals = Array.of_list (p @ locals);
          labels = [| rt |];
          return = Some rt;
        }
      in
      Instruction.expr newctx rt body
    in
    aux_validate f m.funcs

  let validate_tables (_ctx : context) (m : moduledef) =
    let f { ttype } = Type.tabletype ttype in
    aux_validate f m.tables

  let validate_mems (_ctx : context) (m : moduledef) =
    let f { mtype } = Type.memtype mtype in
    aux_validate f m.mems

  let validate_globals (ctx : context) (m : moduledef) =
    let f { gtype; init } =
      let (_, t) = gtype in
      let rt = [ t ] in
      Instruction.expr_const ctx rt init
    in
    aux_validate f m.globals

  let validate_elem (ctx : context) (m : moduledef) =
    let f { table; offset; init } =
      let* _ = aux_idx ctx.tables table in
      let rt = [ TI32 ] in
      let* () = Instruction.expr_const ctx rt offset in
      let* _ = init |> List.map (aux_idx ctx.funcs) |> Result.flatten_l in
      Ok ()
    in
    aux_validate f m.elem

  let validate_data (ctx : context) (m : moduledef) =
    let f { data; offset; _ } =
      let* _ = aux_idx ctx.mems data in
      let rt = [ TI32 ] in
      Instruction.expr_const ctx rt offset
    in
    aux_validate f m.data

  let validate_start (ctx : context) (m : moduledef) =
    match m.start with
    | None -> pass
    | Some { func } ->
      let* func = aux_idx ctx.funcs func in
      let (p, r) = func in
      if List.length p > 0
      then Error "start | parameter must be empty"
      else if List.length r > 0
      then Error "start | result must be empty"
      else pass

  let validate_import (ctx : context) (m : moduledef) =
    let f ({ desc; _ } : import) =
      match desc with
      | ID_func i ->
        aux_idx ctx.funcs i
        |> (*should be validated on initializer *) Result.map ignore
      | ID_table t -> Type.tabletype t
      | ID_mem t -> Type.memtype t
      | ID_global t -> Type.globaltype t
    in
    aux_validate f m.imports

  let validate_export (ctx : context) (m : moduledef) =
    let f ({ desc; _ } : export) =
      match desc with
      | ED_func i ->
        let* f = aux_idx ctx.funcs i in
        Type.functype f
      | ED_table i ->
        let* t = aux_idx ctx.tables i in
        Type.tabletype t
      | ED_mem i ->
        let* m = aux_idx ctx.mems i in
        Type.memtype m
      | ED_global i ->
        let* g = aux_idx ctx.globals i in
        Type.globaltype g
    in
    aux_validate f m.exports

  let no_duplicate_name (es : export array) =
    let names = Array.map (fun (e : export) -> e.name) es in
    let () = Array.sort String.compare names in
    let f prev curr =
      match prev with
      | Ok prev ->
        if String.equal prev curr
        then (Error "all export names must be different", `Stop)
        else (Ok curr, `Continue)
      | Error e -> (Error e, `Stop)
    in
    let* _ = Array.fold_while f (Ok "") names in
    pass

  let validate_module (m : moduledef) (ets : externtype array) : pass_or_err =
    let ift = Types.extern_funcs ets
    and ft = m.types
    and itt = Types.extern_tables ets
    and tt = Array.map (fun t -> t.ttype) m.tables
    and imt = Types.extern_mems ets
    and mt = Array.map (fun t -> t.mtype) m.mems
    and igt = Types.extern_globals ets
    and gt = Array.map (fun t -> t.gtype) m.globals in
    let ctx =
      {
        types = m.types;
        funcs = Array.append ift ft;
        tables = Array.append itt tt;
        mems = Array.append imt mt;
        globals = Array.append igt gt;
        locals = [||];
        labels = [||];
        return = None;
      }
    in
    let* () = validate_types ctx m in
    let* () = validate_funcs ctx m in
    let* () = validate_tables ctx m in
    let* () = validate_mems ctx m in
    let* () = validate_globals ctx m in
    let* () = validate_elem ctx m in
    let* () = validate_data ctx m in
    let* () = validate_start ctx m in
    let* () = validate_import ctx m in
    let* () = validate_export ctx m in
    let* () =
      if Array.length ctx.tables > 1
      then Error "ctx.tables | length must not be greater than 1"
      else pass
    in
    let* () =
      if Array.length ctx.mems > 1
      then Error "ctx.mems | length must not be greater than 1"
      else pass
    in
    let* () = no_duplicate_name m.exports in
    pass
end

let validate = Module.validate_module
