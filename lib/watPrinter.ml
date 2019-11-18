open! Containers
open Types

let sprintf = Printf.sprintf

let concat = String.concat " "

let add_space s = if String.length s = 0 then s else " " ^ s

module Value = struct
  let u32 (x : u32) : string = Int.to_string x

  let i32 (x : Nint32.t) : string = Nint32.to_string x

  let i64 (x : Nint64.t) : string = Nint64.to_string x

  let f32 (x : Nfloat32.t) : string = Nfloat32.to_string x

  let f64 (x : Nfloat64.t) : string = Nfloat64.to_string x

  let byte (x : bytes) : string = Bytes.to_string x

  let idx = u32
end

let aux_typeuse (typei : typeidx) : string =
  sprintf "(type %s)" (Value.idx typei)

module Type = struct
  let valtype = function
    | TI32 -> "i32"
    | TI64 -> "i64"
    | TF32 -> "f32"
    | TF64 -> "f64"

  let elemtype (_e : elemtype) = "funcref"

  let resulttype = function
    | [] -> ""
    | [ t ] -> sprintf "(result %s)" (valtype t)
    | _ -> failwith "Type.resulttype | invalid"

  let functype ((p, r) : functype) =
    let ps =
      match p with
      | [] -> ""
      | _ -> p |> List.map (fun s -> sprintf "(param %s)" (valtype s)) |> concat
    and rs =
      match r with
      | [] -> ""
      | _ ->
        r |> List.map (fun s -> sprintf "(result %s)" (valtype s)) |> concat
    in
    sprintf "(func%s%s)" (add_space ps) (add_space rs)

  let limits ({ min; max } : limits) =
    match max with
    | None -> Value.u32 min
    | Some max -> sprintf "%s %s" (Value.u32 min) (Value.u32 max)

  let memtype x = limits x

  let tabletype ((l, e) : tabletype) = sprintf "%s %s" (limits l) (elemtype e)

  let globaltype ((m, v) : globaltype) =
    let v = valtype v in
    match m with
    | CONST -> v
    | VAR -> sprintf "(mut %s)" v
end

module Instruction = struct
  let memarg n ({ offset; align } : memarg) =
    let o = if offset = 0 then "" else sprintf "offset=%s" (Value.u32 offset) in
    let a = if align = n then "" else sprintf "align=%s" (Value.u32 align) in
    match (o, a) with
    | ("", "") -> ""
    | (o, "") -> o
    | ("", a) -> a
    | (o, a) -> sprintf "%s %s" o a

  let aux_wrap stack =
    match stack with
    | [] -> ""
    | [ h ] -> h
    | stack -> stack |> List.rev |> concat

  let aux_op1 stack op =
    match stack with
    | arg :: t -> sprintf "(%s %s)" op arg :: t
    | [] -> failwith "op1 | invalid module"

  let aux_op2 stack op =
    match stack with
    | arg2 :: arg1 :: t -> sprintf "(%s %s %s)" op arg1 arg2 :: t
    | _ -> failwith "op2 | invalid module"

  let rec expr ins = instrs ins

  and instrs ins =
    let rec aux stack = function
      | [] -> aux_wrap stack
      | h :: t ->
        let stack = instr stack h in
        aux stack t
    in
    aux [] ins

  and instr stack = function
    | Inumeric i -> numeric stack i
    | Iparametric i -> parametric stack i
    | Ivariable i -> variable stack i
    | Imemory i -> memory stack i
    | Icontrol i -> control stack i
    | Iadmin _ -> failwith "never"

  and numeric stack = function
    | Const (I32 v) -> sprintf "(i32.const %s)" (Value.i32 v) :: stack
    | Const (I64 v) -> sprintf "(i64.const %s)" (Value.i64 v) :: stack
    | Const (F32 v) -> sprintf "(f32.const %s)" (Value.f32 v) :: stack
    | Const (F64 v) -> sprintf "(f64.const %s)" (Value.f64 v) :: stack
    (* i32 *)
    | TestOp (TI32, I_EQZ) -> aux_op1 stack "i32.eqz"
    | RelOp (TI32, I_EQ) -> aux_op2 stack "i32.eq"
    | RelOp (TI32, I_NE) -> aux_op2 stack "i32.ne"
    | RelOp (TI32, I_LT_S) -> aux_op2 stack "i32.lt_s"
    | RelOp (TI32, I_LT_U) -> aux_op2 stack "i32.lt_u"
    | RelOp (TI32, I_GT_S) -> aux_op2 stack "i32.gt_s"
    | RelOp (TI32, I_GT_U) -> aux_op2 stack "i32.gt_u"
    | RelOp (TI32, I_LE_S) -> aux_op2 stack "i32.le_s"
    | RelOp (TI32, I_LE_U) -> aux_op2 stack "i32.le_u"
    | RelOp (TI32, I_GE_S) -> aux_op2 stack "i32.ge_s"
    | RelOp (TI32, I_GE_U) -> aux_op2 stack "i32.ge_u"
    (* i64 *)
    | TestOp (TI64, I_EQZ) -> aux_op1 stack "i64.eqz"
    | RelOp (TI64, I_EQ) -> aux_op2 stack "i64.eq"
    | RelOp (TI64, I_NE) -> aux_op2 stack "i64.ne"
    | RelOp (TI64, I_LT_S) -> aux_op2 stack "i64.lt_s"
    | RelOp (TI64, I_LT_U) -> aux_op2 stack "i64.lt_u"
    | RelOp (TI64, I_GT_S) -> aux_op2 stack "i64.gt_s"
    | RelOp (TI64, I_GT_U) -> aux_op2 stack "i64.gt_u"
    | RelOp (TI64, I_LE_S) -> aux_op2 stack "i64.le_s"
    | RelOp (TI64, I_LE_U) -> aux_op2 stack "i64.le_u"
    | RelOp (TI64, I_GE_S) -> aux_op2 stack "i64.ge_s"
    | RelOp (TI64, I_GE_U) -> aux_op2 stack "i64.ge_u"
    (* f32 *)
    | RelOp (TF32, F_EQ) -> aux_op2 stack "f32.eq"
    | RelOp (TF32, F_NE) -> aux_op2 stack "f32.ne"
    | RelOp (TF32, F_LT) -> aux_op2 stack "f32.lt"
    | RelOp (TF32, F_GT) -> aux_op2 stack "f32.gt"
    | RelOp (TF32, F_LE) -> aux_op2 stack "f32.le"
    | RelOp (TF32, F_GE) -> aux_op2 stack "f32.ge"
    (* f64 *)
    | RelOp (TF64, F_EQ) -> aux_op2 stack "f64.eq"
    | RelOp (TF64, F_NE) -> aux_op2 stack "f64.ne"
    | RelOp (TF64, F_LT) -> aux_op2 stack "f64.lt"
    | RelOp (TF64, F_GT) -> aux_op2 stack "f64.gt"
    | RelOp (TF64, F_LE) -> aux_op2 stack "f64.le"
    | RelOp (TF64, F_GE) -> aux_op2 stack "f64.ge"
    (* i32 *)
    | UnOp (TI32, I_CLZ) -> aux_op1 stack "i32.clz"
    | UnOp (TI32, I_CTZ) -> aux_op1 stack "i32.ctz"
    | UnOp (TI32, I_POPCNT) -> aux_op1 stack "i32.popcnt"
    | BinOp (TI32, I_ADD) -> aux_op2 stack "i32.add"
    | BinOp (TI32, I_SUB) -> aux_op2 stack "i32.sub"
    | BinOp (TI32, I_MUL) -> aux_op2 stack "i32.mul"
    | BinOp (TI32, I_DIV_S) -> aux_op2 stack "i32.div_s"
    | BinOp (TI32, I_DIV_U) -> aux_op2 stack "i32.div_u"
    | BinOp (TI32, I_REM_S) -> aux_op2 stack "i32.rem_s"
    | BinOp (TI32, I_REM_U) -> aux_op2 stack "i32.rem_u"
    | BinOp (TI32, I_AND) -> aux_op2 stack "i32.and"
    | BinOp (TI32, I_OR) -> aux_op2 stack "i32.or"
    | BinOp (TI32, I_XOR) -> aux_op2 stack "i32.xor"
    | BinOp (TI32, I_SHL) -> aux_op2 stack "i32.shl"
    | BinOp (TI32, I_SHR_S) -> aux_op2 stack "i32.shr_s"
    | BinOp (TI32, I_SHR_U) -> aux_op2 stack "i32.shr_u"
    | BinOp (TI32, I_ROTL) -> aux_op2 stack "i32.rotl"
    | BinOp (TI32, I_ROTR) -> aux_op2 stack "i32.rotr"
    (* i64 *)
    | UnOp (TI64, I_CLZ) -> aux_op1 stack "i64.clz"
    | UnOp (TI64, I_CTZ) -> aux_op1 stack "i64.ctz"
    | UnOp (TI64, I_POPCNT) -> aux_op1 stack "i64.popcnt"
    | BinOp (TI64, I_ADD) -> aux_op2 stack "i64.add"
    | BinOp (TI64, I_SUB) -> aux_op2 stack "i64.sub"
    | BinOp (TI64, I_MUL) -> aux_op2 stack "i64.mul"
    | BinOp (TI64, I_DIV_S) -> aux_op2 stack "i64.div_s"
    | BinOp (TI64, I_DIV_U) -> aux_op2 stack "i64.div_u"
    | BinOp (TI64, I_REM_S) -> aux_op2 stack "i64.rem_s"
    | BinOp (TI64, I_REM_U) -> aux_op2 stack "i64.rem_u"
    | BinOp (TI64, I_AND) -> aux_op2 stack "i64.and"
    | BinOp (TI64, I_OR) -> aux_op2 stack "i64.or"
    | BinOp (TI64, I_XOR) -> aux_op2 stack "i64.xor"
    | BinOp (TI64, I_SHL) -> aux_op2 stack "i64.shl"
    | BinOp (TI64, I_SHR_S) -> aux_op2 stack "i64.shr_s"
    | BinOp (TI64, I_SHR_U) -> aux_op2 stack "i64.shr_u"
    | BinOp (TI64, I_ROTL) -> aux_op2 stack "i64.rotl"
    | BinOp (TI64, I_ROTR) -> aux_op2 stack "i64.rotr"
    (* f32 *)
    | UnOp (TF32, F_ABS) -> aux_op1 stack "f32.abs"
    | UnOp (TF32, F_NEG) -> aux_op1 stack "f32.neg"
    | UnOp (TF32, F_CEIL) -> aux_op1 stack "f32.ceil"
    | UnOp (TF32, F_FLOOR) -> aux_op1 stack "f32.floor"
    | UnOp (TF32, F_TRUNC) -> aux_op1 stack "f32.trunc"
    | UnOp (TF32, F_NEAREST) -> aux_op1 stack "f32.nearest"
    | UnOp (TF32, F_SQRT) -> aux_op1 stack "f32.sqrt"
    | BinOp (TF32, F_ADD) -> aux_op2 stack "f32.add"
    | BinOp (TF32, F_SUB) -> aux_op2 stack "f32.sub"
    | BinOp (TF32, F_MUL) -> aux_op2 stack "f32.mul"
    | BinOp (TF32, F_DIV) -> aux_op2 stack "f32.div"
    | BinOp (TF32, F_MIN) -> aux_op2 stack "f32.min"
    | BinOp (TF32, F_MAX) -> aux_op2 stack "f32.max"
    | BinOp (TF32, F_COPYSIGN) -> aux_op2 stack "f32.copysign"
    (* f64 *)
    | UnOp (TF64, F_ABS) -> aux_op1 stack "f64.abs"
    | UnOp (TF64, F_NEG) -> aux_op1 stack "f64.neg"
    | UnOp (TF64, F_CEIL) -> aux_op1 stack "f64.ceil"
    | UnOp (TF64, F_FLOOR) -> aux_op1 stack "f64.floor"
    | UnOp (TF64, F_TRUNC) -> aux_op1 stack "f64.trunc"
    | UnOp (TF64, F_NEAREST) -> aux_op1 stack "f64.nearest"
    | UnOp (TF64, F_SQRT) -> aux_op1 stack "f64.sqrt"
    | BinOp (TF64, F_ADD) -> aux_op2 stack "f64.add"
    | BinOp (TF64, F_SUB) -> aux_op2 stack "f64.sub"
    | BinOp (TF64, F_MUL) -> aux_op2 stack "f64.mul"
    | BinOp (TF64, F_DIV) -> aux_op2 stack "f64.div"
    | BinOp (TF64, F_MIN) -> aux_op2 stack "f64.min"
    | BinOp (TF64, F_MAX) -> aux_op2 stack "f64.max"
    | BinOp (TF64, F_COPYSIGN) -> aux_op2 stack "f64.copysign"
    (* cvt *)
    | CvtOp (TI32, CVT_WRAP, TI64) -> aux_op1 stack "i32.wrap_i64"
    | CvtOp (TI32, CVT_TRUNC_S, TF32) -> aux_op1 stack "i32.trunc_f32_s"
    | CvtOp (TI32, CVT_TRUNC_U, TF32) -> aux_op1 stack "i32.trunc_f32_u"
    | CvtOp (TI32, CVT_TRUNC_S, TF64) -> aux_op1 stack "i32.trunc_f64_s"
    | CvtOp (TI32, CVT_TRUNC_U, TF64) -> aux_op1 stack "i32.trunc_f64_u"
    | CvtOp (TI64, CVT_EXTEND_S, TI32) -> aux_op1 stack "i64.extend_i32_s"
    | CvtOp (TI64, CVT_EXTEND_U, TI32) -> aux_op1 stack "i64.extend_i32_u"
    | CvtOp (TI64, CVT_TRUNC_S, TF32) -> aux_op1 stack "i64.trunc_f32_s"
    | CvtOp (TI64, CVT_TRUNC_U, TF32) -> aux_op1 stack "i64.trunc_f32_u"
    | CvtOp (TI64, CVT_TRUNC_S, TF64) -> aux_op1 stack "i64.trunc_f64_s"
    | CvtOp (TI64, CVT_TRUNC_U, TF64) -> aux_op1 stack "i64.trunc_f64_u"
    | CvtOp (TF32, CVT_CONVERT_S, TI32) -> aux_op1 stack "f32.convert_i32_s"
    | CvtOp (TF32, CVT_CONVERT_U, TI32) -> aux_op1 stack "f32.convert_i32_u"
    | CvtOp (TF32, CVT_CONVERT_S, TI64) -> aux_op1 stack "f32.convert_i64_s"
    | CvtOp (TF32, CVT_CONVERT_U, TI64) -> aux_op1 stack "f32.convert_i64_u"
    | CvtOp (TF32, CVT_DEMOTE, TF64) -> aux_op1 stack "f32.demote_f64"
    | CvtOp (TF64, CVT_CONVERT_S, TI32) -> aux_op1 stack "f64.convert_i32_s"
    | CvtOp (TF64, CVT_CONVERT_U, TI32) -> aux_op1 stack "f64.convert_i32_u"
    | CvtOp (TF64, CVT_CONVERT_S, TI64) -> aux_op1 stack "f64.convert_i64_s"
    | CvtOp (TF64, CVT_CONVERT_U, TI64) -> aux_op1 stack "f64.convert_i64_u"
    | CvtOp (TF64, CVT_PROMOTE, TF32) -> aux_op1 stack "f64.promote_f32"
    | CvtOp (TI32, CVT_REINTERPRET, TF32) -> aux_op1 stack "i32.reinterpret_f32"
    | CvtOp (TI64, CVT_REINTERPRET, TF64) -> aux_op1 stack "i64.reinterpret_f64"
    | CvtOp (TF32, CVT_REINTERPRET, TI32) -> aux_op1 stack "f32.reinterpret_i32"
    | CvtOp (TF64, CVT_REINTERPRET, TI64) -> aux_op1 stack "f64.reinterpret_i64"
    | _ -> failwith "never"

  and parametric stack = function
    | Drop -> "(drop)" :: stack
    | Select -> "(select)" :: stack

  and variable stack = function
    | LocalGet i -> sprintf "(local.get %s)" (Value.idx i) :: stack
    | LocalSet i -> sprintf "(local.set %s)" (Value.idx i) :: stack
    | LocalTee i -> sprintf "(local.tee %s)" (Value.idx i) :: stack
    | GlobalGet i -> sprintf "(global.get %s)" (Value.idx i) :: stack
    | GlobalSet i -> sprintf "(global.set %s)" (Value.idx i) :: stack

  and memory stack = function
    | Load (TI32, m) -> sprintf "(i32.load %s)" (memarg 4 m) :: stack
    | Load (TI64, m) -> sprintf "(i64.load %s)" (memarg 8 m) :: stack
    | Load (TF32, m) -> sprintf "(f32.load %s)" (memarg 4 m) :: stack
    | Load (TF64, m) -> sprintf "(f64.load %s)" (memarg 8 m) :: stack
    | Load8S (TI32, m) -> sprintf "(i32.load8_s %s)" (memarg 1 m) :: stack
    | Load8U (TI32, m) -> sprintf "(i32.load8_u %s)" (memarg 1 m) :: stack
    | Load16S (TI32, m) -> sprintf "(i32.load16_s %s)" (memarg 2 m) :: stack
    | Load16U (TI32, m) -> sprintf "(i32.load16_u %s)" (memarg 2 m) :: stack
    | Load8S (TI64, m) -> sprintf "(i64.load8_s %s)" (memarg 1 m) :: stack
    | Load8U (TI64, m) -> sprintf "(i64.load8_u %s)" (memarg 1 m) :: stack
    | Load16S (TI64, m) -> sprintf "(i64.load16_s %s)" (memarg 2 m) :: stack
    | Load16U (TI64, m) -> sprintf "(i64.load16_u %s)" (memarg 2 m) :: stack
    | Load32S (TI64, m) -> sprintf "(i64.load32_s %s)" (memarg 4 m) :: stack
    | Load32U (TI64, m) -> sprintf "(i64.load32_u %s)" (memarg 4 m) :: stack
    | Store (TI32, m) -> sprintf "(i32.store %s)" (memarg 4 m) :: stack
    | Store (TI64, m) -> sprintf "(i64.store %s)" (memarg 8 m) :: stack
    | Store (TF32, m) -> sprintf "(f32.store %s)" (memarg 4 m) :: stack
    | Store (TF64, m) -> sprintf "(f64.store %s)" (memarg 8 m) :: stack
    | Store8 (TI32, m) -> sprintf "(i32.store8 %s)" (memarg 1 m) :: stack
    | Store16 (TI32, m) -> sprintf "(i32.store16 %s)" (memarg 2 m) :: stack
    | Store8 (TI64, m) -> sprintf "(i64.store8 %s)" (memarg 1 m) :: stack
    | Store16 (TI64, m) -> sprintf "(i64.store16 %s)" (memarg 2 m) :: stack
    | Store32 (TI64, m) -> sprintf "(i64.store32 %s)" (memarg 4 m) :: stack
    | MemorySize -> "(memory.size)" :: stack
    | MemoryGrow -> "(memory.grow)" :: stack
    | _ -> failwith "never"

  and control stack = function
    | Unreachable -> "(unreachable)" :: stack
    | Nop -> "(nop)" :: stack
    | Block (r, ins) ->
      let r = Type.resulttype r in
      let ins = instrs ins in
      sprintf "(block %s%s)" r (add_space ins) :: stack
    | Loop (r, ins) ->
      let r = Type.resulttype r in
      let ins = instrs ins in
      sprintf "(loop %s%s)" r (add_space ins) :: stack
    | If (r, in1, []) ->
      let r = Type.resulttype r in
      let in1 = instrs in1 in
      [ sprintf "(if %s %s (then %s))" r (aux_wrap stack) in1 ]
    | If (r, in1, in2) ->
      let r = Type.resulttype r in
      let in1 = instrs in1 in
      let in2 = instrs in2 in
      [ sprintf "(if %s %s (then %s) (else %s))" r (aux_wrap stack) in1 in2 ]
    | Br l -> sprintf "(br %s)" (Value.idx l) :: stack
    | BrIf l -> sprintf "(br_if %s)" (Value.idx l) :: stack
    | BrTable (ls, l) ->
      let ls = ls |> Array.map Value.idx |> Array.to_list |> concat in
      let l = Value.idx l in
      sprintf "(br_table%s%s)" (add_space ls) (add_space l) :: stack
    | Return -> "return" :: stack
    | Call i -> sprintf "(call %s)" (Value.idx i) :: stack
    | CallIndirect i -> sprintf "(call_indirect %s)" (aux_typeuse i) :: stack
end

module Module = struct
  let aux_section (mapf : 'a -> string) (arr : 'a array) : string =
    arr |> Array.map mapf |> Array.to_list |> concat

  let typesec (m : moduledef) =
    let mapf ft = sprintf "(type %s)" (Type.functype ft) in
    aux_section mapf m.types

  let importsec (m : moduledef) =
    let mapf i =
      let modname = i.modname in
      let name = i.name in
      let desc =
        match i.desc with
        | ID_func x -> sprintf "(func %s)" (aux_typeuse x)
        | ID_table t -> sprintf "(table %s)" (Type.tabletype t)
        | ID_mem t -> sprintf "(memory %s)" (Type.memtype t)
        | ID_global t -> sprintf "(global %s)" (Type.globaltype t)
      in
      sprintf "(import %S %S %s)" modname name desc
    in
    aux_section mapf m.imports

  let funcsec (m : moduledef) =
    let mapf (f : func) =
      let t = aux_typeuse f.typei in
      let l =
        f.locals
        |> List.map (fun t -> sprintf "(local %s)" (Type.valtype t))
        |> concat
      in
      let b = Instruction.expr f.body in
      sprintf "(func %s%s%s)" t (add_space l) (add_space b)
    in
    aux_section mapf m.funcs

  let tablesec (m : moduledef) =
    let mapf t = sprintf "(table %s)" (Type.tabletype t.ttype) in
    aux_section mapf m.tables

  let memsec (m : moduledef) =
    let mapf m = sprintf "(memory %s)" (Type.memtype m.mtype) in
    aux_section mapf m.mems

  let globalsec (m : moduledef) =
    let mapf g =
      sprintf
        "(global %s %s)"
        (Type.globaltype g.gtype)
        (Instruction.expr g.init)
    in
    aux_section mapf m.globals

  let exportsec (m : moduledef) =
    let mapf (e : export) =
      let name = e.name in
      let desc =
        match e.desc with
        | ED_func i -> sprintf "(func %s)" (Value.idx i)
        | ED_table i -> sprintf "(table %s)" (Value.idx i)
        | ED_mem i -> sprintf "(memory %s)" (Value.idx i)
        | ED_global i -> sprintf "(global %s)" (Value.idx i)
      in
      sprintf "(export %S %s)" name desc
    in
    aux_section mapf m.exports

  let startsec (m : moduledef) =
    match m.start with
    | None -> ""
    | Some { func } -> sprintf "(start %s)" (Value.idx func)

  let elemsec (m : moduledef) =
    let mapf (e : elem) =
      let x = Value.idx e.table in
      let o = Instruction.expr e.offset in
      let i = e.init |> List.map Value.idx |> concat in
      sprintf "(elem %s (offset %s) %s)" x o i
    in
    aux_section mapf m.elem

  let datasec (m : moduledef) =
    let mapf (d : data) =
      let x = Value.idx d.data in
      let o = Instruction.expr d.offset in
      let i = Value.byte d.init in
      sprintf "(data %s (offset %s) %S)" x o i
    in
    aux_section mapf m.data

  let to_string (m : moduledef) : string =
    let fields =
      [
        typesec m;
        importsec m;
        funcsec m;
        tablesec m;
        memsec m;
        globalsec m;
        exportsec m;
        startsec m;
        elemsec m;
        datasec m;
      ]
    in
    let fields =
      fields |> List.filter (fun s -> String.length s > 0) |> concat
    in
    sprintf "(module%s)" (add_space fields)
end

let to_string = Module.to_string
