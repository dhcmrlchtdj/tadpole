open! Containers
open Types
module D = WatDatum

let ( let* ) = Result.( >>= )

type 'a or_err = ('a, string) result

type tm = {
  types: functype Vector.vector;
  funcs: func Vector.vector;
  tables: table Vector.vector;
  mems: mem Vector.vector;
  globals: global Vector.vector;
  elem: elem Vector.vector;
  data: data Vector.vector;
  start: start option;
  imports: import Vector.vector;
  exports: export Vector.vector;
}

module Value = struct
  let u32 (x : string) : u32 or_err = Int.of_string x |> Result.of_opt

  let i32 (x : string) : Nint32.t or_err = Nint32.of_string x |> Result.of_opt

  let i64 (x : string) : Nint64.t or_err = Nint64.of_string x |> Result.of_opt

  let f32 (x : string) : Nfloat32.t or_err =
    Nfloat32.of_string x |> Result.of_opt

  let f64 (x : string) : Nfloat64.t or_err =
    Nfloat64.of_string x |> Result.of_opt

  let byte (x : string) : bytes = Bytes.of_string x

  let idx (_tm : tm) (d : D.t) : u32 or_err =
    match d with
    | D.NUM s -> u32 s
    | _ -> failwith "unsupported idx"
end

let aux_parse_typeuse (tm : tm) (ds : D.t list) : typeidx or_err =
  match ds with
  | D.LIST (D.KEYWORD "param" :: _) :: _ -> failwith "unsupported"
  | D.LIST (D.KEYWORD "result" :: _) :: _ -> failwith "unsupported"
  | D.LIST [ D.KEYWORD "type"; _ ] :: D.LIST (D.KEYWORD "param" :: _) :: _ ->
    failwith "unsupported"
  | D.LIST [ D.KEYWORD "type"; _ ] :: D.LIST (D.KEYWORD "result" :: _) :: _ ->
    failwith "unsupported"
  | [ D.LIST [ D.KEYWORD "type"; x ] ] -> Value.idx tm x
  | _ -> Error "typeuse | invalid"

module Type = struct
  let valtype = function
    | "i32" -> Ok TI32
    | "i64" -> Ok TI64
    | "f32" -> Ok TF32
    | "f64" -> Ok TF64
    | _ -> Error "invalid valtype"

  let resulttype (d : D.t) : resulttype or_err =
    match d with
    | D.LIST [ D.KEYWORD "result"; D.KEYWORD v ] ->
      let* t = valtype v in
      Ok [ t ]
    | _ -> Error "resulttype | invalid"

  let functype (ds : D.t list) : functype or_err =
    let rec aux p r = function
      | [] -> Ok (List.rev p, List.rev r)
      | D.LIST [ D.KEYWORD "param"; D.ID _id; D.KEYWORD pt ] :: t ->
        (* TODO id *)
        let* pt = valtype pt in
        aux (pt :: p) r t
      | D.LIST (D.KEYWORD "param" :: pts) :: t ->
        let* pts =
          pts
          |> List.rev_map (function
               | D.KEYWORD kw -> valtype kw
               | _ -> Error "expect keyword")
          |> Result.flatten_l
        in
        aux (pts @ p) r t
      | D.LIST (D.KEYWORD "result" :: rts) :: t ->
        let* rts =
          rts
          |> List.rev_map (function
               | D.KEYWORD kw -> valtype kw
               | _ -> Error "expect keyword")
          |> Result.flatten_l
        in
        aux p (rts @ r) t
      | _ -> Error "invalid functype"
    in
    aux [] [] ds

  let limits (n : string) (m : string option) : limits or_err =
    let* min = Value.u32 n in
    if min < 0 || min > (* 4GiB *) 0x1_0000
    then Error "limits | min size"
    else
      let* max =
        match m with
        | None -> Ok None
        | Some m ->
          let* m = Value.u32 m in
          if m < min || m > (* 4GiB *) 0x1_0000
          then Error "limits | max size"
          else Ok (Some m)
      in
      Ok { min; max }

  let memtype (ds : D.t list) : memtype or_err =
    match ds with
    | [ D.NUM n ] -> limits n None
    | [ D.NUM n; D.NUM m ] -> limits n (Some m)
    | _ -> Error "memtype | invalid"

  let tabletype (ds : D.t list) : tabletype or_err =
    match ds with
    | [ D.NUM n; D.KEYWORD "funcref" ] ->
      let* lim = limits n None in
      Ok (lim, FUNCREF)
    | [ D.NUM n; D.NUM m; D.KEYWORD "funcref" ] ->
      let* lim = limits n (Some m) in
      Ok (lim, FUNCREF)
    | _ -> Error "tabletype | invalid"

  let globaltype (d : D.t) : globaltype or_err =
    match d with
    | D.STRING v ->
      let* v = valtype v in
      Ok (CONST, v)
    | D.LIST [ D.KEYWORD "mut"; D.STRING v ] ->
      let* v = valtype v in
      Ok (VAR, v)
    | _ -> Error "globaltype | invalid"
end

module Instruction = struct
  let rec instrs tm (ds : D.t list) : instr list or_err =
    let rec aux acc = function
      | [] -> Ok (acc |> List.rev |> List.flatten)
      | h :: t ->
        let* i = instr tm h in
        aux (i :: acc) t
    in
    aux [] ds

  and instr tm (d : D.t) : instr list or_err =
    match d with
    | D.LIST (D.KEYWORD op :: _ as ds) -> (
      match op with
      (* numeric *)
      | "i32.const"
      | "i64.const"
      | "f32.const"
      | "f64.const"
      (* i32 *)
      | "i32.eqz"
      | "i32.eq"
      | "i32.ne"
      | "i32.lt_s"
      | "i32.lt_u"
      | "i32.gt_s"
      | "i32.gt_u"
      | "i32.le_s"
      | "i32.le_u"
      | "i32.ge_s"
      | "i32.ge_u"
      | "i32.clz"
      | "i32.ctz"
      | "i32.popcnt"
      | "i32.add"
      | "i32.sub"
      | "i32.mul"
      | "i32.div_s"
      | "i32.div_u"
      | "i32.rem_s"
      | "i32.rem_u"
      | "i32.and"
      | "i32.or"
      | "i32.xor"
      | "i32.shl"
      | "i32.shr_s"
      | "i32.shr_u"
      | "i32.rotl"
      | "i32.rotr"
      (* i64 *)
      | "i64.eqz"
      | "i64.eq"
      | "i64.ne"
      | "i64.lt_s"
      | "i64.lt_u"
      | "i64.gt_s"
      | "i64.gt_u"
      | "i64.le_s"
      | "i64.le_u"
      | "i64.ge_s"
      | "i64.ge_u"
      | "i64.clz"
      | "i64.ctz"
      | "i64.popcnt"
      | "i64.add"
      | "i64.sub"
      | "i64.mul"
      | "i64.div_s"
      | "i64.div_u"
      | "i64.rem_s"
      | "i64.rem_u"
      | "i64.and"
      | "i64.or"
      | "i64.xor"
      | "i64.shl"
      | "i64.shr_s"
      | "i64.shr_u"
      | "i64.rotl"
      | "i64.rotr"
      (* f32 *)
      | "f32.eq"
      | "f32.ne"
      | "f32.lt"
      | "f32.gt"
      | "f32.le"
      | "f32.ge"
      | "f32.abs"
      | "f32.neg"
      | "f32.ceil"
      | "f32.floor"
      | "f32.trunc"
      | "f32.nearest"
      | "f32.sqrt"
      | "f32.add"
      | "f32.sub"
      | "f32.mul"
      | "f32.div"
      | "f32.min"
      | "f32.max"
      | "f32.copysign"
      (* f64 *)
      | "f64.eq"
      | "f64.ne"
      | "f64.lt"
      | "f64.gt"
      | "f64.le"
      | "f64.ge"
      | "f64.abs"
      | "f64.neg"
      | "f64.ceil"
      | "f64.floor"
      | "f64.trunc"
      | "f64.nearest"
      | "f64.sqrt"
      | "f64.add"
      | "f64.sub"
      | "f64.mul"
      | "f64.div"
      | "f64.min"
      | "f64.max"
      | "f64.copysign"
      (* cvt *)
      | "i32.wrap_i64"
      | "i32.trunc_f32_s"
      | "i32.trunc_f32_u"
      | "i32.trunc_f64_s"
      | "i32.trunc_f64_u"
      | "i64.extend_i32_s"
      | "i64.extend_i32_u"
      | "i64.trunc_f32_s"
      | "i64.trunc_f32_u"
      | "i64.trunc_f64_s"
      | "i64.trunc_f64_u"
      | "f32.convert_i32_s"
      | "f32.convert_i32_u"
      | "f32.convert_i64_s"
      | "f32.convert_i64_u"
      | "f32.demote_f64"
      | "f64.convert_i32_s"
      | "f64.convert_i32_u"
      | "f64.convert_i64_s"
      | "f64.convert_i64_u"
      | "f64.promote_f32"
      | "i32.reinterpret_f32"
      | "i64.reinterpret_f64"
      | "f32.reinterpret_i32"
      | "f64.reinterpret_i64" -> numric tm ds
      (* parametric *)
      | "drop" | "select" -> parametric ds
      (* variable *)
      | "local.get" | "local.set" | "local.tee" | "global.get" | "global.set" ->
        variable tm ds
      (* memory *)
      | "i32.load"
      | "i64.load"
      | "f32.load"
      | "f64.load"
      | "i32.load8_s"
      | "i32.load8_u"
      | "i32.load16_s"
      | "i32.load16_u"
      | "i64.load8_s"
      | "i64.load8_u"
      | "i64.load16_s"
      | "i64.load16_u"
      | "i64.load32_s"
      | "i64.load32_u"
      | "i32.store"
      | "i64.store"
      | "f32.store"
      | "f64.store"
      | "i32.store8"
      | "i32.store16"
      | "i64.store8"
      | "i64.store16"
      | "i64.store32"
      | "memory.size"
      | "memory.grow" -> memory ds
      (* control *)
      | "unreachable"
      | "nop"
      | "block"
      | "loop"
      | "if"
      | "br"
      | "br_if"
      | "br_table"
      | "return"
      | "call"
      | "call_indirect" -> control tm ds
      | _ -> Error "invalid op"
    )
    | _ -> Error "unsupported instr"

  and numric tm (ds : D.t list) : instr list or_err =
    match ds with
    | [ D.KEYWORD "i32.const"; D.NUM n ] ->
      let* n = Value.i32 n in
      Ok [ Inumeric (Const (I32 n)) ]
    | [ D.KEYWORD "i64.const"; D.NUM n ] ->
      let* n = Value.i64 n in
      Ok [ Inumeric (Const (I64 n)) ]
    | [ D.KEYWORD "f32.const"; D.NUM n ] ->
      let* n = Value.f32 n in
      Ok [ Inumeric (Const (F32 n)) ]
    | [ D.KEYWORD "f64.const"; D.NUM n ] ->
      let* n = Value.f64 n in
      Ok [ Inumeric (Const (F64 n)) ]
    | [ D.KEYWORD op; a ] ->
      let* op =
        match op with
        | "i32.eqz" -> Ok [ Inumeric (TestOp (TI32, I_EQZ)) ]
        | "i32.clz" -> Ok [ Inumeric (UnOp (TI32, I_CLZ)) ]
        | "i32.ctz" -> Ok [ Inumeric (UnOp (TI32, I_CTZ)) ]
        | "i32.popcnt" -> Ok [ Inumeric (UnOp (TI32, I_POPCNT)) ]
        | "i64.eqz" -> Ok [ Inumeric (TestOp (TI64, I_EQZ)) ]
        | "i64.clz" -> Ok [ Inumeric (UnOp (TI64, I_CLZ)) ]
        | "i64.ctz" -> Ok [ Inumeric (UnOp (TI64, I_CTZ)) ]
        | "i64.popcnt" -> Ok [ Inumeric (UnOp (TI64, I_POPCNT)) ]
        | "f32.abs" -> Ok [ Inumeric (UnOp (TF32, F_ABS)) ]
        | "f32.neg" -> Ok [ Inumeric (UnOp (TF32, F_NEG)) ]
        | "f32.ceil" -> Ok [ Inumeric (UnOp (TF32, F_CEIL)) ]
        | "f32.floor" -> Ok [ Inumeric (UnOp (TF32, F_FLOOR)) ]
        | "f32.trunc" -> Ok [ Inumeric (UnOp (TF32, F_TRUNC)) ]
        | "f32.nearest" -> Ok [ Inumeric (UnOp (TF32, F_NEAREST)) ]
        | "f32.sqrt" -> Ok [ Inumeric (UnOp (TF32, F_SQRT)) ]
        | "f64.abs" -> Ok [ Inumeric (UnOp (TF32, F_ABS)) ]
        | "f64.neg" -> Ok [ Inumeric (UnOp (TF32, F_NEG)) ]
        | "f64.ceil" -> Ok [ Inumeric (UnOp (TF32, F_CEIL)) ]
        | "f64.floor" -> Ok [ Inumeric (UnOp (TF32, F_FLOOR)) ]
        | "f64.trunc" -> Ok [ Inumeric (UnOp (TF32, F_TRUNC)) ]
        | "f64.nearest" -> Ok [ Inumeric (UnOp (TF32, F_NEAREST)) ]
        | "f64.sqrt" -> Ok [ Inumeric (UnOp (TF32, F_SQRT)) ]
        | "i32.wrap_i64" -> Ok [ Inumeric (CvtOp (TI32, CVT_WRAP, TI64)) ]
        | "i32.trunc_f32_s" -> Ok [ Inumeric (CvtOp (TI32, CVT_TRUNC_S, TF32)) ]
        | "i32.trunc_f32_u" -> Ok [ Inumeric (CvtOp (TI32, CVT_TRUNC_U, TF32)) ]
        | "i32.trunc_f64_s" -> Ok [ Inumeric (CvtOp (TI32, CVT_TRUNC_S, TF64)) ]
        | "i32.trunc_f64_u" -> Ok [ Inumeric (CvtOp (TI32, CVT_TRUNC_U, TF64)) ]
        | "i64.extend_i32_s" ->
          Ok [ Inumeric (CvtOp (TI64, CVT_EXTEND_S, TI32)) ]
        | "i64.extend_i32_u" ->
          Ok [ Inumeric (CvtOp (TI64, CVT_EXTEND_U, TI32)) ]
        | "i64.trunc_f32_s" -> Ok [ Inumeric (CvtOp (TI64, CVT_TRUNC_S, TF32)) ]
        | "i64.trunc_f32_u" -> Ok [ Inumeric (CvtOp (TI64, CVT_TRUNC_U, TF32)) ]
        | "i64.trunc_f64_s" -> Ok [ Inumeric (CvtOp (TI64, CVT_TRUNC_S, TF64)) ]
        | "i64.trunc_f64_u" -> Ok [ Inumeric (CvtOp (TI64, CVT_TRUNC_U, TF64)) ]
        | "f32.convert_i32_s" ->
          Ok [ Inumeric (CvtOp (TF32, CVT_CONVERT_S, TI32)) ]
        | "f32.convert_i32_u" ->
          Ok [ Inumeric (CvtOp (TF32, CVT_CONVERT_U, TI32)) ]
        | "f32.convert_i64_s" ->
          Ok [ Inumeric (CvtOp (TF32, CVT_CONVERT_S, TI64)) ]
        | "f32.convert_i64_u" ->
          Ok [ Inumeric (CvtOp (TF32, CVT_CONVERT_U, TI64)) ]
        | "f32.demote_f64" -> Ok [ Inumeric (CvtOp (TF32, CVT_DEMOTE, TF64)) ]
        | "f64.convert_i32_s" ->
          Ok [ Inumeric (CvtOp (TF64, CVT_CONVERT_S, TI32)) ]
        | "f64.convert_i32_u" ->
          Ok [ Inumeric (CvtOp (TF64, CVT_CONVERT_U, TI32)) ]
        | "f64.convert_i64_s" ->
          Ok [ Inumeric (CvtOp (TF64, CVT_CONVERT_S, TI64)) ]
        | "f64.convert_i64_u" ->
          Ok [ Inumeric (CvtOp (TF64, CVT_CONVERT_U, TI64)) ]
        | "f64.promote_f32" -> Ok [ Inumeric (CvtOp (TF64, CVT_PROMOTE, TF32)) ]
        | "i32.reinterpret_f32" ->
          Ok [ Inumeric (CvtOp (TI32, CVT_REINTERPRET, TF32)) ]
        | "i64.reinterpret_f64" ->
          Ok [ Inumeric (CvtOp (TI64, CVT_REINTERPRET, TF64)) ]
        | "f32.reinterpret_i32" ->
          Ok [ Inumeric (CvtOp (TF32, CVT_REINTERPRET, TI32)) ]
        | "f64.reinterpret_i64" ->
          Ok [ Inumeric (CvtOp (TF64, CVT_REINTERPRET, TI64)) ]
        | _ -> Error "invalid uanry op"
      in
      let* a = instr tm a in
      let ins = [ a; op ] in
      Ok (List.flatten ins)
    | [ D.KEYWORD op; a1; a2 ] ->
      let* op =
        match op with
        | "i32.eq" -> Ok [ Inumeric (RelOp (TI32, I_EQ)) ]
        | "i32.ne" -> Ok [ Inumeric (RelOp (TI32, I_NE)) ]
        | "i32.lt_s" -> Ok [ Inumeric (RelOp (TI32, I_LT_S)) ]
        | "i32.lt_u" -> Ok [ Inumeric (RelOp (TI32, I_LT_U)) ]
        | "i32.gt_s" -> Ok [ Inumeric (RelOp (TI32, I_GT_S)) ]
        | "i32.gt_u" -> Ok [ Inumeric (RelOp (TI32, I_GT_U)) ]
        | "i32.le_s" -> Ok [ Inumeric (RelOp (TI32, I_LE_S)) ]
        | "i32.le_u" -> Ok [ Inumeric (RelOp (TI32, I_LE_U)) ]
        | "i32.ge_s" -> Ok [ Inumeric (RelOp (TI32, I_GE_S)) ]
        | "i32.ge_u" -> Ok [ Inumeric (RelOp (TI32, I_GE_U)) ]
        | "i32.add" -> Ok [ Inumeric (BinOp (TI32, I_ADD)) ]
        | "i32.sub" -> Ok [ Inumeric (BinOp (TI32, I_SUB)) ]
        | "i32.mul" -> Ok [ Inumeric (BinOp (TI32, I_MUL)) ]
        | "i32.div_s" -> Ok [ Inumeric (BinOp (TI32, I_DIV_S)) ]
        | "i32.div_u" -> Ok [ Inumeric (BinOp (TI32, I_DIV_U)) ]
        | "i32.rem_s" -> Ok [ Inumeric (BinOp (TI32, I_REM_S)) ]
        | "i32.rem_u" -> Ok [ Inumeric (BinOp (TI32, I_REM_U)) ]
        | "i32.and" -> Ok [ Inumeric (BinOp (TI32, I_AND)) ]
        | "i32.or" -> Ok [ Inumeric (BinOp (TI32, I_OR)) ]
        | "i32.xor" -> Ok [ Inumeric (BinOp (TI32, I_XOR)) ]
        | "i32.shl" -> Ok [ Inumeric (BinOp (TI32, I_SHL)) ]
        | "i32.shr_s" -> Ok [ Inumeric (BinOp (TI32, I_SHR_S)) ]
        | "i32.shr_u" -> Ok [ Inumeric (BinOp (TI32, I_SHR_U)) ]
        | "i32.rotl" -> Ok [ Inumeric (BinOp (TI32, I_ROTL)) ]
        | "i32.rotr" -> Ok [ Inumeric (BinOp (TI32, I_ROTR)) ]
        | "i64.eq" -> Ok [ Inumeric (RelOp (TI64, I_EQ)) ]
        | "i64.ne" -> Ok [ Inumeric (RelOp (TI64, I_NE)) ]
        | "i64.lt_s" -> Ok [ Inumeric (RelOp (TI64, I_LT_S)) ]
        | "i64.lt_u" -> Ok [ Inumeric (RelOp (TI64, I_LT_U)) ]
        | "i64.gt_s" -> Ok [ Inumeric (RelOp (TI64, I_GT_S)) ]
        | "i64.gt_u" -> Ok [ Inumeric (RelOp (TI64, I_GT_U)) ]
        | "i64.le_s" -> Ok [ Inumeric (RelOp (TI64, I_LE_S)) ]
        | "i64.le_u" -> Ok [ Inumeric (RelOp (TI64, I_LE_U)) ]
        | "i64.ge_s" -> Ok [ Inumeric (RelOp (TI64, I_GE_S)) ]
        | "i64.ge_u" -> Ok [ Inumeric (RelOp (TI64, I_GE_U)) ]
        | "i64.add" -> Ok [ Inumeric (BinOp (TI64, I_ADD)) ]
        | "i64.sub" -> Ok [ Inumeric (BinOp (TI64, I_SUB)) ]
        | "i64.mul" -> Ok [ Inumeric (BinOp (TI64, I_MUL)) ]
        | "i64.div_s" -> Ok [ Inumeric (BinOp (TI64, I_DIV_S)) ]
        | "i64.div_u" -> Ok [ Inumeric (BinOp (TI64, I_DIV_U)) ]
        | "i64.rem_s" -> Ok [ Inumeric (BinOp (TI64, I_REM_S)) ]
        | "i64.rem_u" -> Ok [ Inumeric (BinOp (TI64, I_REM_U)) ]
        | "i64.and" -> Ok [ Inumeric (BinOp (TI64, I_AND)) ]
        | "i64.or" -> Ok [ Inumeric (BinOp (TI64, I_OR)) ]
        | "i64.xor" -> Ok [ Inumeric (BinOp (TI64, I_XOR)) ]
        | "i64.shl" -> Ok [ Inumeric (BinOp (TI64, I_SHL)) ]
        | "i64.shr_s" -> Ok [ Inumeric (BinOp (TI64, I_SHR_S)) ]
        | "i64.shr_u" -> Ok [ Inumeric (BinOp (TI64, I_SHR_U)) ]
        | "i64.rotl" -> Ok [ Inumeric (BinOp (TI64, I_ROTL)) ]
        | "i64.rotr" -> Ok [ Inumeric (BinOp (TI64, I_ROTR)) ]
        | "f32.eq" -> Ok [ Inumeric (RelOp (TF32, F_EQ)) ]
        | "f32.ne" -> Ok [ Inumeric (RelOp (TF32, F_NE)) ]
        | "f32.lt" -> Ok [ Inumeric (RelOp (TF32, F_LT)) ]
        | "f32.gt" -> Ok [ Inumeric (RelOp (TF32, F_GT)) ]
        | "f32.le" -> Ok [ Inumeric (RelOp (TF32, F_LE)) ]
        | "f32.ge" -> Ok [ Inumeric (RelOp (TF32, F_GE)) ]
        | "f32.add" -> Ok [ Inumeric (BinOp (TF32, F_ADD)) ]
        | "f32.sub" -> Ok [ Inumeric (BinOp (TF32, F_SUB)) ]
        | "f32.mul" -> Ok [ Inumeric (BinOp (TF32, F_MUL)) ]
        | "f32.div" -> Ok [ Inumeric (BinOp (TF32, F_DIV)) ]
        | "f32.min" -> Ok [ Inumeric (BinOp (TF32, F_MIN)) ]
        | "f32.max" -> Ok [ Inumeric (BinOp (TF32, F_MAX)) ]
        | "f32.copysign" -> Ok [ Inumeric (BinOp (TF32, F_COPYSIGN)) ]
        | "f64.eq" -> Ok [ Inumeric (RelOp (TF64, F_EQ)) ]
        | "f64.ne" -> Ok [ Inumeric (RelOp (TF64, F_NE)) ]
        | "f64.lt" -> Ok [ Inumeric (RelOp (TF64, F_LT)) ]
        | "f64.gt" -> Ok [ Inumeric (RelOp (TF64, F_GT)) ]
        | "f64.le" -> Ok [ Inumeric (RelOp (TF64, F_LE)) ]
        | "f64.ge" -> Ok [ Inumeric (RelOp (TF64, F_GE)) ]
        | "f64.add" -> Ok [ Inumeric (BinOp (TF64, F_ADD)) ]
        | "f64.sub" -> Ok [ Inumeric (BinOp (TF64, F_SUB)) ]
        | "f64.mul" -> Ok [ Inumeric (BinOp (TF64, F_MUL)) ]
        | "f64.div" -> Ok [ Inumeric (BinOp (TF64, F_DIV)) ]
        | "f64.min" -> Ok [ Inumeric (BinOp (TF64, F_MIN)) ]
        | "f64.max" -> Ok [ Inumeric (BinOp (TF64, F_MAX)) ]
        | "f64.copysign" -> Ok [ Inumeric (BinOp (TF64, F_COPYSIGN)) ]
        | _ -> Error "invalid binary op"
      in
      let* a1 = instr tm a1 in
      let* a2 = instr tm a2 in
      let ins = [ a1; a2; op ] in
      Ok (List.flatten ins)
    | _ -> Error "numeric | invalid"

  and parametric (ds : D.t list) : instr list or_err =
    match ds with
    | [ D.KEYWORD "drop" ] -> Ok [ Iparametric Drop ]
    | [ D.KEYWORD "select" ] -> Ok [ Iparametric Select ]
    | _ -> Error "parametric | invalid"

  and variable tm (ds : D.t list) : instr list or_err =
    match ds with
    | [ D.KEYWORD op; a ] ->
      let* i = Value.idx tm a in
      let ins =
        match op with
        | "local.get" -> Ivariable (LocalGet i)
        | "local.set" -> Ivariable (LocalSet i)
        | "local.tee" -> Ivariable (LocalTee i)
        | "global.get" -> Ivariable (GlobalGet i)
        | "global.set" -> Ivariable (GlobalSet i)
        | _ -> failwith "never"
      in
      Ok [ ins ]
    | _ -> Error "variable | invalid"

  and memory (ds : D.t list) : instr list or_err =
    match ds with
    | [ D.KEYWORD "memory.size" ] -> Ok [ Imemory MemorySize ]
    | [ D.KEYWORD "memory.grow" ] -> Ok [ Imemory MemoryGrow ]
    | D.KEYWORD op :: ds ->
      let fix_align n memarg =
        if memarg.align = -1 then { memarg with align = n } else memarg
      in
      let* memarg =
        match ds with
        | [ D.KEYWORD o; D.KEYWORD a ] -> (
          match (String.split_on_char '=' o, String.split_on_char '=' a) with
          | ([ "offset"; o ], [ "align"; a ]) ->
            let* offset = Value.u32 o in
            let* align = Value.u32 a in
            Ok { offset; align }
          | _ -> Error "invalid memarg | [o;a]"
        )
        | [ D.KEYWORD oa ] -> (
          match String.split_on_char '=' oa with
          | [ "offset"; o ] ->
            let* offset = Value.u32 o in
            Ok { offset; align = -1 }
          | [ "align"; a ] ->
            let* align = Value.u32 a in
            Ok { offset = 0; align }
          | _ -> Error "invalid memarg | [oa]"
        )
        | [] -> Ok { offset = 0; align = -1 }
        | _ -> Error "invalid memarg"
      in
      let op =
        match op with
        | "i32.load" -> Load (TI32, fix_align 4 memarg)
        | "i64.load" -> Load (TI64, fix_align 8 memarg)
        | "f32.load" -> Load (TF32, fix_align 4 memarg)
        | "f64.load" -> Load (TF64, fix_align 8 memarg)
        | "i32.load8_s" -> Load8S (TI32, fix_align 1 memarg)
        | "i32.load8_u" -> Load8U (TI32, fix_align 1 memarg)
        | "i32.load16_s" -> Load16S (TI32, fix_align 2 memarg)
        | "i32.load16_u" -> Load16U (TI32, fix_align 2 memarg)
        | "i64.load8_s" -> Load8S (TI64, fix_align 1 memarg)
        | "i64.load8_u" -> Load8U (TI64, fix_align 1 memarg)
        | "i64.load16_s" -> Load16S (TI64, fix_align 2 memarg)
        | "i64.load16_u" -> Load16U (TI64, fix_align 2 memarg)
        | "i64.load32_s" -> Load32S (TI64, fix_align 4 memarg)
        | "i64.load32_u" -> Load32U (TI64, fix_align 4 memarg)
        | "i32.store" -> Store (TI32, fix_align 4 memarg)
        | "i64.store" -> Store (TI64, fix_align 8 memarg)
        | "f32.store" -> Store (TF32, fix_align 4 memarg)
        | "f64.store" -> Store (TF64, fix_align 8 memarg)
        | "i32.store8" -> Store8 (TI32, fix_align 1 memarg)
        | "i32.store16" -> Store16 (TI32, fix_align 2 memarg)
        | "i64.store8" -> Store8 (TI64, fix_align 1 memarg)
        | "i64.store16" -> Store16 (TI64, fix_align 2 memarg)
        | "i64.store32" -> Store32 (TI64, fix_align 4 memarg)
        | _ -> failwith "never"
      in
      Ok [ Imemory op ]
    | _ -> Error "memory | invalid"

  and control tm (ds : D.t list) : instr list or_err =
    match ds with
    | D.KEYWORD "block" :: rt :: ins ->
      let* rt = Type.resulttype rt in
      let* ins = instrs tm ins in
      Ok [ Icontrol (Block (rt, ins)) ]
    | D.KEYWORD "loop" :: rt :: ins ->
      let* rt = Type.resulttype rt in
      let* ins = instrs tm ins in
      Ok [ Icontrol (Loop (rt, ins)) ]
    | [ D.KEYWORD "if"; rt; ins; D.LIST (D.KEYWORD "then" :: ins1) ] ->
      let* rt = Type.resulttype rt in
      let* ins = instr tm ins in
      let* ins1 = instrs tm ins1 in
      let iif = [ Icontrol (If (rt, ins1, [])) ] in
      Ok (List.flatten [ ins; iif ])
    | [
     D.KEYWORD "if";
     rt;
     ins;
     D.LIST (D.KEYWORD "then" :: ins1);
     D.LIST (D.KEYWORD "else" :: ins2);
    ] ->
      let* rt = Type.resulttype rt in
      let* ins = instr tm ins in
      let* ins1 = instrs tm ins1 in
      let* ins2 = instrs tm ins2 in
      let iif = [ Icontrol (If (rt, ins1, ins2)) ] in
      Ok (List.flatten [ ins; iif ])
    | [ D.KEYWORD "unreachable" ] -> Ok [ Icontrol Unreachable ]
    | [ D.KEYWORD "nop" ] -> Ok [ Icontrol Nop ]
    | [ D.KEYWORD "br"; l ] ->
      let* l = Value.idx tm l in
      Ok [ Icontrol (Br l) ]
    | [ D.KEYWORD "br_if"; l ] ->
      let* l = Value.idx tm l in
      Ok [ Icontrol (BrIf l) ]
    | D.KEYWORD "br_table" :: labels -> (
      let* labels = labels |> List.rev_map (Value.idx tm) |> Result.flatten_l in
      match labels with
      | l :: ls ->
        let ls = ls |> List.rev |> Array.of_list in
        Ok [ Icontrol (BrTable (ls, l)) ]
      | [] -> Error "br_table | invalid"
    )
    | [ D.KEYWORD "return" ] -> Ok [ Icontrol Return ]
    | [ D.KEYWORD "call"; x ] ->
      let* x = Value.idx tm x in
      Ok [ Icontrol (Call x) ]
    | D.KEYWORD "call_indirect" :: x ->
      let* x = aux_parse_typeuse tm x in
      Ok [ Icontrol (CallIndirect x) ]
    | _ -> Error "control | invalid"

  let expr = instrs
end

module Module = struct
  let aux_fresh_id =
    let cnt = ref 0 in
    fun () ->
      let id = Printf.sprintf "$tadpole_fresh_id@%d~" !cnt in
      let () = incr cnt in
      id

  let aux_match_id = function
    | D.ID id :: t -> (D.ID id, t)
    | t -> (D.ID (aux_fresh_id ()), t)

  let rec parse_section (tm : tm) (ds : D.t list) : tm or_err =
    match ds with
    | [] -> Ok tm
    | D.LIST (D.KEYWORD k :: h) :: t ->
      let* f =
        match k with
        | "type" -> Ok parse_typesec
        | "import" -> Ok parse_importsec
        | "func" -> Ok parse_funcsec
        | "table" -> Ok parse_tablesec
        | "memory" -> Ok parse_memorysec
        | "global" -> Ok parse_globalsec
        | "export" -> Ok parse_exportsec
        | "start" -> Ok parse_startsec
        | "elem" -> Ok parse_elemsec
        | "data" -> Ok parse_datasec
        | _ -> Error "unkonwn section"
      in
      let* tm = f tm h in
      parse_section tm t
    | _ -> Error "unexpected section"

  and parse_typesec (tm : tm) (ds : D.t list) : tm or_err =
    let (_id, ds) = aux_match_id ds in
    match ds with
    | [ D.LIST (D.KEYWORD "func" :: f) ] ->
      (* TODO id *)
      let* ft = Type.functype f in
      let () = Vector.push tm.types ft in
      Ok tm
    | _ -> Error "typesec | invalid"

  and parse_importsec (tm : tm) (ds : D.t list) : tm or_err =
    match ds with
    | [ D.STRING modname; D.STRING name; D.LIST idesc ] ->
      let* desc =
        (* TODO id *)
        match idesc with
        | D.KEYWORD "func" :: t ->
          let (_id, t) = aux_match_id t in
          let* typ = aux_parse_typeuse tm t in
          Ok (ID_func typ)
        | D.KEYWORD "table" :: t ->
          let (_id, t) = aux_match_id t in
          let* idx = Type.tabletype t in
          Ok (ID_table idx)
        | D.KEYWORD "memory" :: t ->
          let (_id, t) = aux_match_id t in
          let* idx = Type.memtype t in
          Ok (ID_mem idx)
        | D.KEYWORD "global" :: t -> (
          let (_id, t) = aux_match_id t in
          match t with
          | [ t ] ->
            let* idx = Type.globaltype t in
            Ok (ID_global idx)
          | _ -> Error "importdesc | invalid global"
        )
        | _ -> Error "importsec | invalid desc"
      in
      let i = { modname; name; desc } in
      let () = Vector.push tm.imports i in
      Ok tm
    | _ -> Error "importsec | invalid"

  and parse_funcsec (tm : tm) (ds : D.t list) : tm or_err =
    let aux_parse_type ds =
      match ds with
      | (D.LIST (D.KEYWORD "type" :: _) as h) :: t ->
        let* typei = aux_parse_typeuse tm [ h ] in
        Ok (typei, t)
      | _ -> Error "unsupported func type"
    in
    let aux_parse_local ds =
      let rec aux acc = function
        | D.LIST [ D.KEYWORD "local"; D.ID _; D.KEYWORD v ] :: t ->
          let* v = Type.valtype v in
          aux (v :: acc) t
        | D.LIST [ D.KEYWORD "local"; D.KEYWORD v ] :: t ->
          let* v = Type.valtype v in
          aux (v :: acc) t
        | ds -> Ok (List.rev acc, ds)
      in
      aux [] ds
    in
    let (_id, ds) = aux_match_id ds in
    let* (typei, ds) = aux_parse_type ds in
    let* (locals, ds) = aux_parse_local ds in
    let* body = Instruction.expr tm ds in
    let f = { typei; locals; body } in
    let () = Vector.push tm.funcs f in
    Ok tm

  and parse_tablesec (tm : tm) (ds : D.t list) : tm or_err =
    let (id, ds) = aux_match_id ds in
    match ds with
    | D.NUM _ :: _ as tt ->
      (* TODO id *)
      let* ttype = Type.tabletype tt in
      let table = { ttype } in
      Vector.push tm.tables table;
      Ok tm
    | [ (D.KEYWORD _ as et); D.LIST (D.KEYWORD "elem" :: fidxs) ] ->
      let n = List.length fidxs |> Int.to_string in
      let tsec = [ id; D.NUM n; D.NUM n; et ] in
      let esec = id :: D.LIST [ D.KEYWORD "i32.const"; D.NUM "0" ] :: fidxs in
      let* tm = parse_tablesec tm tsec in
      parse_elemsec tm esec
    | D.LIST [ D.KEYWORD "import"; name1; name2 ] :: tt ->
      let isec = [ name1; name2; D.LIST (D.KEYWORD "table" :: id :: tt) ] in
      parse_importsec tm isec
    | D.LIST [ D.KEYWORD "export"; name ] :: t ->
      let esec = [ name; D.LIST [ D.KEYWORD "table"; id ] ] in
      let tsec = id :: t in
      let* tm = parse_exportsec tm esec in
      parse_tablesec tm tsec
    | _ -> Error "tablesec | invalid"

  and parse_memorysec (tm : tm) (ds : D.t list) : tm or_err =
    let (id, ds) = aux_match_id ds in
    match ds with
    | [ D.NUM _ ] as l ->
      (* TODO id *)
      let* mtype = Type.memtype l in
      let t = { mtype } in
      Vector.push tm.mems t;
      Ok tm
    | [ D.NUM _; D.NUM _ ] as l ->
      (* TODO id *)
      let* mtype = Type.memtype l in
      let t = { mtype } in
      Vector.push tm.mems t;
      Ok tm
    | [ D.LIST [ D.KEYWORD "data" ] ] -> Ok tm
    | [ D.LIST [ D.KEYWORD "data"; D.STRING d ] ] ->
      let m =
        d
        |> Value.byte
        |> Bytes.length
        |> Float.of_int
        |> (fun x -> x /. 0x1_0000.)
        |> ceil
        |> Float.to_int
        |> Int.to_string
      in
      let msec = [ id; D.NUM m; D.NUM m ] in
      let dsec =
        [ id; D.LIST [ D.KEYWORD "i32.const"; D.NUM "0" ]; D.STRING d ]
      in
      let* tm = parse_memorysec tm msec in
      parse_datasec tm dsec
    | D.LIST [ D.KEYWORD "import"; name1; name2 ] :: mt ->
      let isec = [ name1; name2; D.LIST (D.KEYWORD "memory" :: id :: mt) ] in
      parse_importsec tm isec
    | D.LIST [ D.KEYWORD "export"; name ] :: mm ->
      let esec = [ name; D.LIST [ D.KEYWORD "memory"; id ] ] in
      let msec = id :: mm in
      let* tm = parse_exportsec tm esec in
      parse_memorysec tm msec
    | _ -> failwith "memorysec | invalid"

  and parse_globalsec (tm : tm) (ds : D.t list) : tm or_err =
    let (id, ds) = aux_match_id ds in
    match ds with
    | [ (D.STRING _ as gt); D.LIST e ] ->
      (* TODO id *)
      let* gtype = Type.globaltype gt in
      let* init = Instruction.expr tm e in
      let global = { gtype; init } in
      Vector.push tm.globals global;
      Ok tm
    | [ (D.LIST [ D.KEYWORD "mut"; D.STRING _ ] as gt); D.LIST e ] ->
      let* gtype = Type.globaltype gt in
      let* init = Instruction.expr tm e in
      let global = { gtype; init } in
      Vector.push tm.globals global;
      Ok tm
    | [ D.LIST [ D.KEYWORD "import"; name1; name2 ]; gt ] ->
      let isec = [ name1; name2; D.LIST [ D.KEYWORD "global"; id; gt ] ] in
      parse_importsec tm isec
    | D.LIST [ D.KEYWORD "export"; name ] :: t ->
      let esec = [ name; D.LIST [ D.KEYWORD "global"; id ] ] in
      let gsec = id :: t in
      let* tm = parse_exportsec tm esec in
      parse_globalsec tm gsec
    | _ -> Error "globalsec | invalid"

  and parse_exportsec (tm : tm) (ds : D.t list) : tm or_err =
    match ds with
    | [ D.STRING name; D.LIST desc ] ->
      let* desc =
        match desc with
        | [ D.KEYWORD "func"; t ] ->
          let* idx = Value.idx tm t in
          Ok (ED_func idx)
        | [ D.KEYWORD "table"; t ] ->
          let* idx = Value.idx tm t in
          Ok (ED_table idx)
        | [ D.KEYWORD "memory"; t ] ->
          let* idx = Value.idx tm t in
          Ok (ED_mem idx)
        | [ D.KEYWORD "global"; t ] ->
          let* idx = Value.idx tm t in
          Ok (ED_global idx)
        | _ -> Error "importsec | invalid desc"
      in
      let e = { name; desc } in
      let () = Vector.push tm.exports e in
      Ok tm
    | _ -> Error "importsec | invalid"

  and parse_startsec (tm : tm) (ds : D.t list) : tm or_err =
    match ds with
    | [ x ] ->
      let* func = Value.idx tm x in
      Ok { tm with start = Some { func } }
    | _ -> Error "startsec | invalid"

  and parse_elemsec (tm : tm) (ds : D.t list) : tm or_err =
    match ds with
    | D.LIST _ :: _ ->
      let esec = D.NUM "0" :: ds in
      parse_elemsec tm esec
    | x :: D.LIST [ D.KEYWORD "offset"; D.LIST e ] :: fidxs ->
      let* table = Value.idx tm x in
      let* offset = Instruction.expr tm e in
      let* init = fidxs |> List.map (Value.idx tm) |> Result.flatten_l in
      let elem = { table; offset; init } in
      Vector.push tm.elem elem;
      Ok tm
    | x :: D.LIST e :: fidxs ->
      let esec = x :: D.LIST [ D.KEYWORD "offset"; D.LIST e ] :: fidxs in
      parse_elemsec tm esec
    | _ -> Error "elemsec | invalid"

  and parse_datasec (tm : tm) (ds : D.t list) : tm or_err =
    match ds with
    | D.LIST _ :: _ ->
      let dsec = D.NUM "0" :: ds in
      parse_datasec tm dsec
    | [ x; D.LIST [ D.KEYWORD "offset"; D.LIST e ]; D.STRING b ] ->
      let* data = Value.idx tm x in
      let* offset = Instruction.expr tm e in
      let init = Value.byte b in
      let d = { data; offset; init } in
      Vector.push tm.data d;
      Ok tm
    | [ x; D.LIST e; b ] ->
      let dsec = [ x; D.LIST [ D.KEYWORD "offset"; D.LIST e ]; b ] in
      parse_datasec tm dsec
    | _ -> Error "datasec | invalid"

  let parse_module (d : D.t) : moduledef or_err =
    let tm =
      {
        types = Vector.create ();
        funcs = Vector.create ();
        tables = Vector.create ();
        mems = Vector.create ();
        globals = Vector.create ();
        elem = Vector.create ();
        data = Vector.create ();
        start = None;
        imports = Vector.create ();
        exports = Vector.create ();
      }
    in
    match d with
    | D.LIST (D.KEYWORD "module" :: t) ->
      let* tm = parse_section tm t in
      let m : moduledef =
        {
          types = Vector.to_array tm.types;
          funcs = Vector.to_array tm.funcs;
          tables = Vector.to_array tm.tables;
          mems = Vector.to_array tm.mems;
          globals = Vector.to_array tm.globals;
          elem = Vector.to_array tm.elem;
          data = Vector.to_array tm.data;
          start = tm.start;
          imports = Vector.to_array tm.imports;
          exports = Vector.to_array tm.exports;
        }
      in
      Ok m
    | _ -> Error "parse_module"
end

let parse (src : string) : Types.moduledef or_err =
  let* tokens = WatScanner.scan src in
  let* datum = D.of_tokens tokens in
  (* let () = prerr_endline (D.show datum) in *)
  (* let () = prerr_newline () in *)
  Module.parse_module datum
