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

  let name (x : string) : string = x

  let idx = u32
end

let aux_typeuse (typei : typeidx) : string =
  (* TODO: http://webassembly.github.io/spec/core/text/modules.html#type-uses *)
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
    | [ t ] -> sprintf "(%s)" (valtype t)
    | _ -> failwith "Typ.resulttype | invalid"

  let functype ((p, r) : functype) =
    let ps =
      match p with
        | [] -> ""
        | _ ->
          p |> List.map (fun s -> sprintf "(param %s)" (valtype s)) |> concat
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
  let memarg n ({ align; offset } : memarg) =
    let a = if align = n then "" else sprintf "align=%s" (Value.u32 align) in
    let o = if offset = 0 then "" else sprintf "offset=%s" (Value.u32 offset) in
    match (o, a) with
      | ("", "") -> ""
      | ("", a) -> a
      | (o, "") -> o
      | (o, a) -> sprintf "%s %s" o a

  let rec expr ins = instrs ins

  and instrs ins =
    let rec aux acc = function
      | [] -> concat (List.rev acc)
      | h :: t -> aux (instr h :: acc) t
    in
    aux [] ins

  and instr = function
    | Inumeric i -> numeric i
    | Iparametric i -> parametric i
    | Ivariable i -> variable i
    | Imemory i -> memory i
    | Icontrol i -> control i
    | Iadmin _ -> failwith "never"

  and numeric = function
    | Const (I32 v) -> sprintf "(i32.const %s)" (Value.i32 v)
    | Const (I64 v) -> sprintf "(i64.const %s)" (Value.i64 v)
    | Const (F32 v) -> sprintf "(f32.const %s)" (Value.f32 v)
    | Const (F64 v) -> sprintf "(f64.const %s)" (Value.f64 v)
    (* i32 *)
    | TestOp (TI32, I_EQZ) -> "i32.eqz"
    | RelOp (TI32, I_EQ) -> "i32.eq"
    | RelOp (TI32, I_NE) -> "i32.ne"
    | RelOp (TI32, I_LT_S) -> "i32.lt_s"
    | RelOp (TI32, I_LT_U) -> "i32.lt_u"
    | RelOp (TI32, I_GT_S) -> "i32.gt_s"
    | RelOp (TI32, I_GT_U) -> "i32.gt_u"
    | RelOp (TI32, I_LE_S) -> "i32.le_s"
    | RelOp (TI32, I_LE_U) -> "i32.le_u"
    | RelOp (TI32, I_GE_S) -> "i32.ge_s"
    | RelOp (TI32, I_GE_U) -> "i32.ge_u"
    (* i64 *)
    | TestOp (TI64, I_EQZ) -> "i64.eqz"
    | RelOp (TI64, I_EQ) -> "i64.eq"
    | RelOp (TI64, I_NE) -> "i64.ne"
    | RelOp (TI64, I_LT_S) -> "i64.lt_s"
    | RelOp (TI64, I_LT_U) -> "i64.lt_u"
    | RelOp (TI64, I_GT_S) -> "i64.gt_s"
    | RelOp (TI64, I_GT_U) -> "i64.gt_u"
    | RelOp (TI64, I_LE_S) -> "i64.le_s"
    | RelOp (TI64, I_LE_U) -> "i64.le_u"
    | RelOp (TI64, I_GE_S) -> "i64.ge_s"
    | RelOp (TI64, I_GE_U) -> "i64.ge_u"
    (* f32 *)
    | RelOp (TF32, F_EQ) -> "f32.eq"
    | RelOp (TF32, F_NE) -> "f32.ne"
    | RelOp (TF32, F_LT) -> "f32.lt"
    | RelOp (TF32, F_GT) -> "f32.gt"
    | RelOp (TF32, F_LE) -> "f32.le"
    | RelOp (TF32, F_GE) -> "f32.ge"
    (* f64 *)
    | RelOp (TF64, F_EQ) -> "f64.eq"
    | RelOp (TF64, F_NE) -> "f64.ne"
    | RelOp (TF64, F_LT) -> "f64.lt"
    | RelOp (TF64, F_GT) -> "f64.gt"
    | RelOp (TF64, F_LE) -> "f64.le"
    | RelOp (TF64, F_GE) -> "f64.ge"
    (* i32 *)
    | UnOp (TI32, I_CLZ) -> "i32.clz"
    | UnOp (TI32, I_CTZ) -> "i32.ctz"
    | UnOp (TI32, I_POPCONT) -> "i32.popcnt"
    | BinOp (TI32, I_ADD) -> "i32.add"
    | BinOp (TI32, I_SUB) -> "i32.sub"
    | BinOp (TI32, I_MUL) -> "i32.mul"
    | BinOp (TI32, I_DIV_S) -> "i32.div_s"
    | BinOp (TI32, I_DIV_U) -> "i32.div_u"
    | BinOp (TI32, I_REM_S) -> "i32.rem_s"
    | BinOp (TI32, I_REM_U) -> "i32.rem_u"
    | BinOp (TI32, I_AND) -> "i32.and"
    | BinOp (TI32, I_OR) -> "i32.or"
    | BinOp (TI32, I_XOR) -> "i32.xor"
    | BinOp (TI32, I_SHL) -> "i32.shl"
    | BinOp (TI32, I_SHR_S) -> "i32.shr_s"
    | BinOp (TI32, I_SHR_U) -> "i32.shr_u"
    | BinOp (TI32, I_ROTL) -> "i32.rotl"
    | BinOp (TI32, I_ROTR) -> "i32.rotr"
    (* i64 *)
    | UnOp (TI64, I_CLZ) -> "i64.clz"
    | UnOp (TI64, I_CTZ) -> "i64.ctz"
    | UnOp (TI64, I_POPCONT) -> "i64.popcnt"
    | BinOp (TI64, I_ADD) -> "i64.add"
    | BinOp (TI64, I_SUB) -> "i64.sub"
    | BinOp (TI64, I_MUL) -> "i64.mul"
    | BinOp (TI64, I_DIV_S) -> "i64.div_s"
    | BinOp (TI64, I_DIV_U) -> "i64.div_u"
    | BinOp (TI64, I_REM_S) -> "i64.rem_s"
    | BinOp (TI64, I_REM_U) -> "i64.rem_u"
    | BinOp (TI64, I_AND) -> "i64.and"
    | BinOp (TI64, I_OR) -> "i64.or"
    | BinOp (TI64, I_XOR) -> "i64.xor"
    | BinOp (TI64, I_SHL) -> "i64.shl"
    | BinOp (TI64, I_SHR_S) -> "i64.shr_s"
    | BinOp (TI64, I_SHR_U) -> "i64.shr_u"
    | BinOp (TI64, I_ROTL) -> "i64.rotl"
    | BinOp (TI64, I_ROTR) -> "i64.rotr"
    (* f32 *)
    | UnOp (TF32, F_ABS) -> "f32.abs"
    | UnOp (TF32, F_NEG) -> "f32.neg"
    | UnOp (TF32, F_CEIL) -> "f32.ceil"
    | UnOp (TF32, F_FLOOR) -> "f32.floor"
    | UnOp (TF32, F_TRUNC) -> "f32.trunc"
    | UnOp (TF32, F_NEAREST) -> "f32.nearest"
    | UnOp (TF32, F_SQRT) -> "f32.sqrt"
    | BinOp (TF32, F_ADD) -> "f32.add"
    | BinOp (TF32, F_SUB) -> "f32.sub"
    | BinOp (TF32, F_MUL) -> "f32.mul"
    | BinOp (TF32, F_DIV) -> "f32.div"
    | BinOp (TF32, F_MIN) -> "f32.min"
    | BinOp (TF32, F_MAX) -> "f32.max"
    | BinOp (TF32, F_COPYSIGN) -> "f32.copysign"
    (* f64 *)
    | UnOp (TF64, F_ABS) -> "f64.abs"
    | UnOp (TF64, F_NEG) -> "f64.neg"
    | UnOp (TF64, F_CEIL) -> "f64.ceil"
    | UnOp (TF64, F_FLOOR) -> "f64.floor"
    | UnOp (TF64, F_TRUNC) -> "f64.trunc"
    | UnOp (TF64, F_NEAREST) -> "f64.nearest"
    | UnOp (TF64, F_SQRT) -> "f64.sqrt"
    | BinOp (TF64, F_ADD) -> "f64.add"
    | BinOp (TF64, F_SUB) -> "f64.sub"
    | BinOp (TF64, F_MUL) -> "f64.mul"
    | BinOp (TF64, F_DIV) -> "f64.div"
    | BinOp (TF64, F_MIN) -> "f64.min"
    | BinOp (TF64, F_MAX) -> "f64.max"
    | BinOp (TF64, F_COPYSIGN) -> "f64.copysign"
    (* cvt *)
    | CvtOp (TI32, CVT_WRAP, TI64) -> "i32.wrap_i64"
    | CvtOp (TI32, CVT_TRUNC_S, TF32) -> "i32.trunc_f32_s"
    | CvtOp (TI32, CVT_TRUNC_U, TF32) -> "i32.trunc_f32_u"
    | CvtOp (TI32, CVT_TRUNC_S, TF64) -> "i32.trunc_f64_s"
    | CvtOp (TI32, CVT_TRUNC_U, TF64) -> "i32.trunc_f64_u"
    | CvtOp (TI64, CVT_EXTEND_S, TI32) -> "i64.extend_i32_s"
    | CvtOp (TI64, CVT_EXTEND_U, TI32) -> "i64.extend_i32_u"
    | CvtOp (TI64, CVT_TRUNC_S, TF32) -> "i64.trunc_f32_s"
    | CvtOp (TI64, CVT_TRUNC_U, TF32) -> "i64.trunc_f32_u"
    | CvtOp (TI64, CVT_TRUNC_S, TF64) -> "i64.trunc_f64_s"
    | CvtOp (TI64, CVT_TRUNC_U, TF64) -> "i64.trunc_f64_u"
    | CvtOp (TF32, CVT_CONVERT_S, TI32) -> "f32.convert_i32_s"
    | CvtOp (TF32, CVT_CONVERT_U, TI32) -> "f32.convert_i32_u"
    | CvtOp (TF32, CVT_CONVERT_S, TI64) -> "f32.convert_i64_s"
    | CvtOp (TF32, CVT_CONVERT_U, TI64) -> "f32.convert_i64_u"
    | CvtOp (TF32, CVT_DEMOTE, TF64) -> "f32.demote_f64"
    | CvtOp (TF64, CVT_CONVERT_S, TI32) -> "f64.convert_i32_s"
    | CvtOp (TF64, CVT_CONVERT_U, TI32) -> "f64.convert_i32_u"
    | CvtOp (TF64, CVT_CONVERT_S, TI64) -> "f64.convert_i64_s"
    | CvtOp (TF64, CVT_CONVERT_U, TI64) -> "f64.convert_i64_u"
    | CvtOp (TF64, CVT_PROMOTE, TF32) -> "f64.promote_f32"
    | CvtOp (TI32, CVT_REINTERPRET, TF32) -> "i32.reinterpret_f32"
    | CvtOp (TI64, CVT_REINTERPRET, TF64) -> "i64.reinterpret_f64"
    | CvtOp (TF32, CVT_REINTERPRET, TI32) -> "f32.reinterpret_i32"
    | CvtOp (TF64, CVT_REINTERPRET, TI64) -> "f64.reinterpret_i64"
    | _ -> failwith "never"

  and parametric = function
    | Drop -> "(drop)"
    | Select -> "(select)"

  and variable = function
    | LocalGet i -> sprintf "(local.get %s)" (Value.idx i)
    | LocalSet i -> sprintf "(local.set %s)" (Value.idx i)
    | LocalTee i -> sprintf "(local.tee %s)" (Value.idx i)
    | GlobalGet i -> sprintf "(global.get %s)" (Value.idx i)
    | GlobalSet i -> sprintf "(global.set %s)" (Value.idx i)

  and memory = function
    | Load (TI32, m) -> sprintf "(i32.load %s)" (memarg 4 m)
    | Load (TI64, m) -> sprintf "(i64.load %s)" (memarg 8 m)
    | Load (TF32, m) -> sprintf "(f32.load %s)" (memarg 4 m)
    | Load (TF64, m) -> sprintf "(f64.load %s)" (memarg 8 m)
    | Load8S (TI32, m) -> sprintf "(i32.load8_s %s)" (memarg 1 m)
    | Load8U (TI32, m) -> sprintf "(i32.load8_u %s)" (memarg 1 m)
    | Load16S (TI32, m) -> sprintf "(i32.load16_s %s)" (memarg 2 m)
    | Load16U (TI32, m) -> sprintf "(i32.load16_u %s)" (memarg 2 m)
    | Load8S (TI64, m) -> sprintf "(i32.load8_s %s)" (memarg 1 m)
    | Load8U (TI64, m) -> sprintf "(i32.load8_u %s)" (memarg 1 m)
    | Load16S (TI64, m) -> sprintf "(i32.load16_s %s)" (memarg 2 m)
    | Load16U (TI64, m) -> sprintf "(i32.load16_u %s)" (memarg 2 m)
    | Load32S (TI64, m) -> sprintf "(i32.load32_s %s)" (memarg 4 m)
    | Load32U (TI64, m) -> sprintf "(i32.load32_s %s)" (memarg 4 m)
    | Store (TI32, m) -> sprintf "(i32.store %s)" (memarg 4 m)
    | Store (TI64, m) -> sprintf "(i64.store %s)" (memarg 8 m)
    | Store (TF32, m) -> sprintf "(f32.store %s)" (memarg 4 m)
    | Store (TF64, m) -> sprintf "(f64.store %s)" (memarg 8 m)
    | Store8 (TI32, m) -> sprintf "(i32.store8 %s)" (memarg 1 m)
    | Store16 (TI32, m) -> sprintf "(i32.store16 %s)" (memarg 2 m)
    | Store8 (TI64, m) -> sprintf "(i64.store8 %s)" (memarg 1 m)
    | Store16 (TI64, m) -> sprintf "(i64.store16 %s)" (memarg 2 m)
    | Store32 (TI64, m) -> sprintf "(i64.store32 %s)" (memarg 4 m)
    | MemorySize -> "(memory.size)"
    | MemoryGrow -> "(memory.grow)"
    | _ -> failwith "never"

  and control = function
    | Unreachable -> "(unreachable)"
    | Nop -> "(nop)"
    | Block (r, ins) ->
      let r = Type.resulttype r in
      let ins = instrs ins in
      sprintf "(block %s%s)" r (add_space ins)
    | Loop (r, ins) ->
      let r = Type.resulttype r in
      let ins = instrs ins in
      sprintf "(loop %s%s)" r (add_space ins)
    | If (r, in1, []) ->
      let r = Type.resulttype r in
      let in1 = instrs in1 in
      sprintf "if %s%s end" r (add_space in1)
    | If (r, in1, in2) ->
      let r = Type.resulttype r in
      let in1 = instrs in1 in
      let in2 = instrs in2 in
      sprintf "if %s%s else%s end" r (add_space in1) (add_space in2)
    | Br l -> sprintf "(br %s)" (Value.idx l)
    | BrIf l -> sprintf "(br_if %s)" (Value.idx l)
    | BrTable (ls, l) ->
      let ls = ls |> Array.map Value.idx |> Array.to_list |> concat in
      let l = Value.idx l in
      sprintf "(br_table%s%s)" (add_space ls) (add_space l)
    | Return -> "return"
    | Call i -> sprintf "(call %s)" (Value.idx i)
    | CallIndirect i -> sprintf "(call_indirect %s)" (aux_typeuse i)
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
      sprintf "(import %s %s %s)" modname name desc
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
      sprintf "(func%s%s%s)" t l b
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
      sprintf "(export %s %s)" name desc
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
      sprintf "(data %s (offset %s) %s)" x o i
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
