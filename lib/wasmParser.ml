open! Containers
open Types

let ( let* ) = Result.( >>= )

type 'a or_err = ('a, string) result

module S = struct
  type t = {
    s: string;
    left: int;
    right: int;
  }

  let of_string (s : string) : t = { s; left = 0; right = String.length s }

  let to_string ({ s; left; right } : t) : string =
    String.sub s left (right - left)

  let take (len : int) (src : t) : (t * t) or_err =
    let { left; right; _ } = src in
    if left + len <= right
    then (
      let sub = { src with right = left + len } in
      let src = { src with left = left + len } in
      Ok (sub, src)
    )
    else Error "take | EOF"

  let take_char (src : t) : (char * t) or_err =
    let { s; left; right } = src in
    if left = right
    then Error "take_char | EOF"
    else (
      let ch = s.[left] in
      let src = { src with left = left + 1 } in
      Ok (ch, src)
    )

  let take_int (src : t) : (int * t) or_err =
    let* (ch, src) = take_char src in
    let n = Char.code ch in
    Ok (n, src)

  let peek_char (src : t) : char option or_err =
    let { s; left; right } = src in
    if left = right
    then Ok None
    else (
      let ch = s.[left] in
      Ok (Some ch)
    )

  let consume (pattern : string) (src : t) : t or_err =
    let len = String.length pattern in
    let* (sub, src) = take len src in
    if String.equal pattern (to_string sub)
    then Ok src
    else Error "consume | not match"

  let consume_char (p : char) (src : t) : t or_err =
    let* (ch, src) = take_char src in
    if Char.equal p ch then Ok src else Error "consume_char | not match"

  let skip (len : int) (src : t) : t or_err =
    let { left; right; _ } = src in
    if left + len <= right
    then (
      let src = { src with left = left + len } in
      Ok src
    )
    else Error "EOF | skip"
end

module Value = struct
  let aux_leb128 (src : S.t) : (string * S.t) or_err =
    let rec aux (acc : int list) (src : S.t) =
      let* (n, src) = S.take_int src in
      let acc = n :: acc in
      if n land 0x80 = 0
      then (
        let s = acc |> List.rev_map Char.chr |> String.of_list in
        Ok (s, src)
      )
      else aux acc src
    in
    aux [] src

  let u32 (src : S.t) : (u32 * S.t) or_err =
    let* (s, src) = aux_leb128 src in
    let i = s |> Leb128.Unsigned.decode |> Int64.to_int in
    Ok (i, src)

  let i32 (src : S.t) : (Nint32.t * S.t) or_err =
    let* (s, src) = aux_leb128 src in
    let i = s |> Leb128.Signed.decode |> Int64.to_int32 in
    Ok (i, src)

  let i64 (src : S.t) : (Nint64.t * S.t) or_err =
    let* (s, src) = aux_leb128 src in
    let i = s |> Leb128.Signed.decode in
    Ok (i, src)

  let f32 (src : S.t) : (Nfloat32.t * S.t) or_err =
    let* (sub, src) = S.take 4 src in
    let n = sub |> S.to_string |> Bytes.of_string |> Nfloat32.of_bytes_le in
    Ok (n, src)

  let f64 (src : S.t) : (Nfloat64.t * S.t) or_err =
    let* (sub, src) = S.take 8 src in
    let n = sub |> S.to_string |> Bytes.of_string |> Nfloat64.of_bytes_le in
    Ok (n, src)

  let byte (src : S.t) : (bytes * S.t) or_err =
    let* (size, src) = u32 src in
    let* (b, src) = S.take size src in
    let b = b |> S.to_string |> Bytes.of_string in
    Ok (b, src)

  let name (src : S.t) : (string * S.t) or_err =
    let* (size, src) = u32 src in
    let* (n, src) = S.take size src in
    let n = S.to_string n in
    Ok (n, src)

  let idx = u32
end

let aux_vec (f : S.t -> ('a * S.t) or_err) (src : S.t) : ('a list * S.t) or_err =
  let rec aux (acc : 'a list) (src : S.t) n =
    if n = 0
    then (
      let l = List.rev acc in
      Ok (l, src)
    )
    else
      let* (a, src) = f src in
      aux (a :: acc) src (n - 1)
  in
  let* (n, src) = Value.u32 src in
  aux [] src n

module Type = struct
  let valtype (s : S.t) : (valtype * S.t) or_err =
    let* (t, s) = S.take_char s in
    match t with
    | '\x7f' -> Ok (TI32, s)
    | '\x7e' -> Ok (TI64, s)
    | '\x7d' -> Ok (TF32, s)
    | '\x7c' -> Ok (TF64, s)
    | _ -> Error "Type.valtype"

  let resulttype (s : S.t) : (resulttype * S.t) or_err =
    let* c = S.peek_char s in
    if Option.equal Char.equal c (Some '\x40')
    then
      let* s = S.skip 1 s in
      Ok ([], s)
    else
      let* (v, s) = valtype s in
      Ok ([ v ], s)

  let functype (s : S.t) : (functype * S.t) or_err =
    let* s = S.consume_char '\x60' s in
    let* (t1, s) = aux_vec valtype s in
    let* (t2, s) = aux_vec valtype s in
    let func = (t1, t2) in
    Ok (func, s)

  let limits (s : S.t) : (limits * S.t) or_err =
    let* (t, s) = S.take_char s in
    match t with
    | '\x00' ->
      let* (min, s) = Value.u32 s in
      if min < 0 || min > (* 4GiB *) 0x10000
      then Error "limits | min size"
      else (
        let limits = { min; max = None } in
        Ok (limits, s)
      )
    | '\x01' ->
      let* (min, s) = Value.u32 s in
      let* (max, s) = Value.u32 s in
      if min < 0
         || min > (* 4GiB *) 0x10000
         || max < min
         || max > (* 4GiB *) 0x10000
      then Error "limits | size"
      else (
        let limits = { min; max = Some max } in
        Ok (limits, s)
      )
    | _ -> Error "Type.limits"

  let memtype (s : S.t) : (memtype * S.t) or_err = limits s

  let elemtype (s : S.t) : (elemtype * S.t) or_err =
    let* s = S.consume_char '\x70' s in
    Ok (FUNCREF, s)

  let tabletype (s : S.t) : (tabletype * S.t) or_err =
    let* (et, s) = elemtype s in
    let* (lim, s) = limits s in
    let table = (lim, et) in
    Ok (table, s)

  let mut (s : S.t) : (mut * S.t) or_err =
    let* (t, s) = S.take_char s in
    match t with
    | '\x00' -> Ok (CONST, s)
    | '\x01' -> Ok (VAR, s)
    | _ -> Error "Type.mut"

  let globaltype (s : S.t) : (globaltype * S.t) or_err =
    let* (t, s) = valtype s in
    let* (m, s) = mut s in
    let g = (m, t) in
    Ok (g, s)
end

module Instruction = struct
  let rec memarg (s : S.t) : (memarg * S.t) or_err =
    let* (align, s) = Value.u32 s in
    let* (offset, s) = Value.u32 s in
    let m = { align; offset } in
    Ok (m, s)

  and icontrol (s : S.t) : (instr * S.t) or_err =
    let* (t, s) = S.take_char s in
    match t with
    | '\x00' -> Ok (Icontrol Unreachable, s)
    | '\x01' -> Ok (Icontrol Nop, s)
    | '\x02' ->
      let* (rt, s) = Type.resulttype s in
      let* (ins, s) = instrs s in
      let* s = S.consume_char '\x0b' s in
      Ok (Icontrol (Block (rt, ins)), s)
    | '\x03' ->
      let* (rt, s) = Type.resulttype s in
      let* (ins, s) = instrs s in
      let* s = S.consume_char '\x0b' s in
      Ok (Icontrol (Loop (rt, ins)), s)
    | '\x04' ->
      let* (rt, s) = Type.resulttype s in
      let* (in1, s) = instrs s in
      let* (in2, s) =
        let* (c, s) = S.take_char s in
        match c with
        | '\x0b' -> Ok ([], s)
        | '\x05' ->
          let* (in2, s) = instrs s in
          let* s = S.consume_char '\x0b' s in
          Ok (in2, s)
        | _ -> Error "invalid control instr | if_else"
      in
      Ok (Icontrol (If (rt, in1, in2)), s)
    | '\x0c' ->
      let* (x, s) = Value.idx s in
      Ok (Icontrol (Br x), s)
    | '\x0d' ->
      let* (x, s) = Value.idx s in
      Ok (Icontrol (BrIf x), s)
    | '\x0e' ->
      let* (ls, s) = aux_vec Value.idx s in
      let ls = Array.of_list ls in
      let* (l, s) = Value.idx s in
      Ok (Icontrol (BrTable (ls, l)), s)
    | '\x0f' -> Ok (Icontrol Return, s)
    | '\x10' ->
      let* (x, s) = Value.idx s in
      Ok (Icontrol (Call x), s)
    | '\x11' ->
      let* (x, s) = Value.idx s in
      let* s = S.consume_char '\x00' s in
      Ok (Icontrol (CallIndirect x), s)
    | _ -> Error "invalid control instr"

  and iparametric (s : S.t) : (instr * S.t) or_err =
    let* (t, s) = S.take_char s in
    match t with
    | '\x1a' -> Ok (Iparametric Drop, s)
    | '\x1b' -> Ok (Iparametric Select, s)
    | _ -> Error "invalid parameteric instr"

  and ivariable (s : S.t) : (instr * S.t) or_err =
    let* (t, s) = S.take_char s in
    let* (idx, s) = Value.idx s in
    match t with
    | '\x20' -> Ok (Ivariable (LocalGet idx), s)
    | '\x21' -> Ok (Ivariable (LocalSet idx), s)
    | '\x22' -> Ok (Ivariable (LocalTee idx), s)
    | '\x23' -> Ok (Ivariable (GlobalGet idx), s)
    | '\x24' -> Ok (Ivariable (GlobalSet idx), s)
    | _ -> Error "invalid variable instr"

  and imemory (s : S.t) : (instr * S.t) or_err =
    let* (t, s) = S.take_char s in
    match t with
    | '\x3f' ->
      let* s = S.consume_char '\x00' s in
      Ok (Imemory MemorySize, s)
    | '\x40' ->
      let* s = S.consume_char '\x00' s in
      Ok (Imemory MemoryGrow, s)
    | '\x28' .. '\x3e' -> (
      let* (m, s) = memarg s in
      match t with
      | '\x28' -> Ok (Imemory (Load (TI32, m)), s)
      | '\x29' -> Ok (Imemory (Load (TI64, m)), s)
      | '\x2a' -> Ok (Imemory (Load (TF32, m)), s)
      | '\x2b' -> Ok (Imemory (Load (TF64, m)), s)
      | '\x2c' -> Ok (Imemory (Load8S (TI32, m)), s)
      | '\x2d' -> Ok (Imemory (Load8S (TI32, m)), s)
      | '\x2e' -> Ok (Imemory (Load16S (TI32, m)), s)
      | '\x2f' -> Ok (Imemory (Load16S (TI32, m)), s)
      | '\x30' -> Ok (Imemory (Load8S (TI64, m)), s)
      | '\x31' -> Ok (Imemory (Load8U (TI64, m)), s)
      | '\x32' -> Ok (Imemory (Load16S (TI64, m)), s)
      | '\x33' -> Ok (Imemory (Load16U (TI64, m)), s)
      | '\x34' -> Ok (Imemory (Load32S (TI64, m)), s)
      | '\x35' -> Ok (Imemory (Load32U (TI64, m)), s)
      | '\x36' -> Ok (Imemory (Store (TI32, m)), s)
      | '\x37' -> Ok (Imemory (Store (TI32, m)), s)
      | '\x38' -> Ok (Imemory (Store (TF32, m)), s)
      | '\x39' -> Ok (Imemory (Store (TF64, m)), s)
      | '\x3a' -> Ok (Imemory (Store8 (TI32, m)), s)
      | '\x3b' -> Ok (Imemory (Store16 (TI32, m)), s)
      | '\x3c' -> Ok (Imemory (Store8 (TI64, m)), s)
      | '\x3d' -> Ok (Imemory (Store16 (TI64, m)), s)
      | '\x3e' -> Ok (Imemory (Store32 (TI64, m)), s)
      | _ -> failwith "never"
    )
    | _ -> Error "invalid memory instr"

  and inumric (s : S.t) : (instr * S.t) or_err =
    let* (t, s) = S.take_char s in
    match t with
    | '\x41' ->
      let* (n, s) = Value.i32 s in
      Ok (Inumeric (Const (I32 n)), s)
    | '\x42' ->
      let* (n, s) = Value.i64 s in
      Ok (Inumeric (Const (I64 n)), s)
    | '\x43' ->
      let* (n, s) = Value.f32 s in
      Ok (Inumeric (Const (F32 n)), s)
    | '\x44' ->
      let* (n, s) = Value.f64 s in
      Ok (Inumeric (Const (F64 n)), s)
    (* i32 *)
    | '\x45' -> Ok (Inumeric (TestOp (TI32, I_EQZ)), s)
    | '\x46' -> Ok (Inumeric (RelOp (TI32, I_EQ)), s)
    | '\x47' -> Ok (Inumeric (RelOp (TI32, I_NE)), s)
    | '\x48' -> Ok (Inumeric (RelOp (TI32, I_LT_S)), s)
    | '\x49' -> Ok (Inumeric (RelOp (TI32, I_LT_U)), s)
    | '\x4a' -> Ok (Inumeric (RelOp (TI32, I_GT_S)), s)
    | '\x4b' -> Ok (Inumeric (RelOp (TI32, I_GT_U)), s)
    | '\x4c' -> Ok (Inumeric (RelOp (TI32, I_LE_S)), s)
    | '\x4d' -> Ok (Inumeric (RelOp (TI32, I_LE_U)), s)
    | '\x4e' -> Ok (Inumeric (RelOp (TI32, I_GE_S)), s)
    | '\x4f' -> Ok (Inumeric (RelOp (TI32, I_GE_U)), s)
    (* i64 *)
    | '\x50' -> Ok (Inumeric (TestOp (TI64, I_EQZ)), s)
    | '\x51' -> Ok (Inumeric (RelOp (TI64, I_EQ)), s)
    | '\x52' -> Ok (Inumeric (RelOp (TI64, I_NE)), s)
    | '\x53' -> Ok (Inumeric (RelOp (TI64, I_LT_S)), s)
    | '\x54' -> Ok (Inumeric (RelOp (TI64, I_LT_U)), s)
    | '\x55' -> Ok (Inumeric (RelOp (TI64, I_GT_S)), s)
    | '\x56' -> Ok (Inumeric (RelOp (TI64, I_GT_U)), s)
    | '\x57' -> Ok (Inumeric (RelOp (TI64, I_LE_S)), s)
    | '\x58' -> Ok (Inumeric (RelOp (TI64, I_LE_U)), s)
    | '\x59' -> Ok (Inumeric (RelOp (TI64, I_GE_S)), s)
    | '\x5a' -> Ok (Inumeric (RelOp (TI64, I_GE_U)), s)
    (* f32 *)
    | '\x5b' -> Ok (Inumeric (RelOp (TF32, F_EQ)), s)
    | '\x5c' -> Ok (Inumeric (RelOp (TF32, F_NE)), s)
    | '\x5d' -> Ok (Inumeric (RelOp (TF32, F_LT)), s)
    | '\x5e' -> Ok (Inumeric (RelOp (TF32, F_GT)), s)
    | '\x5f' -> Ok (Inumeric (RelOp (TF32, F_LE)), s)
    | '\x60' -> Ok (Inumeric (RelOp (TF32, F_GE)), s)
    (* f64 *)
    | '\x61' -> Ok (Inumeric (RelOp (TF64, F_EQ)), s)
    | '\x62' -> Ok (Inumeric (RelOp (TF64, F_NE)), s)
    | '\x63' -> Ok (Inumeric (RelOp (TF64, F_LT)), s)
    | '\x64' -> Ok (Inumeric (RelOp (TF64, F_GT)), s)
    | '\x65' -> Ok (Inumeric (RelOp (TF64, F_LE)), s)
    | '\x66' -> Ok (Inumeric (RelOp (TF64, F_GE)), s)
    (* i32 *)
    | '\x67' -> Ok (Inumeric (UnOp (TI32, I_CLZ)), s)
    | '\x68' -> Ok (Inumeric (UnOp (TI32, I_CTZ)), s)
    | '\x69' -> Ok (Inumeric (UnOp (TI32, I_POPCONT)), s)
    | '\x6a' -> Ok (Inumeric (BinOp (TI32, I_ADD)), s)
    | '\x6b' -> Ok (Inumeric (BinOp (TI32, I_SUB)), s)
    | '\x6c' -> Ok (Inumeric (BinOp (TI32, I_MUL)), s)
    | '\x6d' -> Ok (Inumeric (BinOp (TI32, I_DIV_S)), s)
    | '\x6e' -> Ok (Inumeric (BinOp (TI32, I_DIV_U)), s)
    | '\x6f' -> Ok (Inumeric (BinOp (TI32, I_REM_S)), s)
    | '\x70' -> Ok (Inumeric (BinOp (TI32, I_REM_U)), s)
    | '\x71' -> Ok (Inumeric (BinOp (TI32, I_AND)), s)
    | '\x72' -> Ok (Inumeric (BinOp (TI32, I_OR)), s)
    | '\x73' -> Ok (Inumeric (BinOp (TI32, I_XOR)), s)
    | '\x74' -> Ok (Inumeric (BinOp (TI32, I_SHL)), s)
    | '\x75' -> Ok (Inumeric (BinOp (TI32, I_SHR_S)), s)
    | '\x76' -> Ok (Inumeric (BinOp (TI32, I_SHR_U)), s)
    | '\x77' -> Ok (Inumeric (BinOp (TI32, I_ROTL)), s)
    | '\x78' -> Ok (Inumeric (BinOp (TI32, I_ROTR)), s)
    (* i64 *)
    | '\x79' -> Ok (Inumeric (UnOp (TI64, I_CLZ)), s)
    | '\x7a' -> Ok (Inumeric (UnOp (TI64, I_CTZ)), s)
    | '\x7b' -> Ok (Inumeric (UnOp (TI64, I_POPCONT)), s)
    | '\x7c' -> Ok (Inumeric (BinOp (TI64, I_ADD)), s)
    | '\x7d' -> Ok (Inumeric (BinOp (TI64, I_SUB)), s)
    | '\x7e' -> Ok (Inumeric (BinOp (TI64, I_MUL)), s)
    | '\x7f' -> Ok (Inumeric (BinOp (TI64, I_DIV_S)), s)
    | '\x80' -> Ok (Inumeric (BinOp (TI64, I_DIV_U)), s)
    | '\x81' -> Ok (Inumeric (BinOp (TI64, I_REM_S)), s)
    | '\x82' -> Ok (Inumeric (BinOp (TI64, I_REM_U)), s)
    | '\x83' -> Ok (Inumeric (BinOp (TI64, I_AND)), s)
    | '\x84' -> Ok (Inumeric (BinOp (TI64, I_OR)), s)
    | '\x85' -> Ok (Inumeric (BinOp (TI64, I_XOR)), s)
    | '\x86' -> Ok (Inumeric (BinOp (TI64, I_SHL)), s)
    | '\x87' -> Ok (Inumeric (BinOp (TI64, I_SHR_S)), s)
    | '\x88' -> Ok (Inumeric (BinOp (TI64, I_SHR_U)), s)
    | '\x89' -> Ok (Inumeric (BinOp (TI64, I_ROTL)), s)
    | '\x8a' -> Ok (Inumeric (BinOp (TI64, I_ROTR)), s)
    (* f32 *)
    | '\x8b' -> Ok (Inumeric (UnOp (TF32, F_ABS)), s)
    | '\x8c' -> Ok (Inumeric (UnOp (TF32, F_NEG)), s)
    | '\x8d' -> Ok (Inumeric (UnOp (TF32, F_CEIL)), s)
    | '\x8e' -> Ok (Inumeric (UnOp (TF32, F_FLOOR)), s)
    | '\x8f' -> Ok (Inumeric (UnOp (TF32, F_TRUNC)), s)
    | '\x90' -> Ok (Inumeric (UnOp (TF32, F_NEAREST)), s)
    | '\x91' -> Ok (Inumeric (UnOp (TF32, F_SQRT)), s)
    | '\x92' -> Ok (Inumeric (BinOp (TF32, F_ADD)), s)
    | '\x93' -> Ok (Inumeric (BinOp (TF32, F_SUB)), s)
    | '\x94' -> Ok (Inumeric (BinOp (TF32, F_MUL)), s)
    | '\x95' -> Ok (Inumeric (BinOp (TF32, F_DIV)), s)
    | '\x96' -> Ok (Inumeric (BinOp (TF32, F_MIN)), s)
    | '\x97' -> Ok (Inumeric (BinOp (TF32, F_MAX)), s)
    | '\x98' -> Ok (Inumeric (BinOp (TF32, F_COPYSIGN)), s)
    (* f64 *)
    | '\x99' -> Ok (Inumeric (UnOp (TF64, F_ABS)), s)
    | '\x9a' -> Ok (Inumeric (UnOp (TF64, F_NEG)), s)
    | '\x9b' -> Ok (Inumeric (UnOp (TF64, F_CEIL)), s)
    | '\x9c' -> Ok (Inumeric (UnOp (TF64, F_FLOOR)), s)
    | '\x9d' -> Ok (Inumeric (UnOp (TF64, F_TRUNC)), s)
    | '\x9e' -> Ok (Inumeric (UnOp (TF64, F_NEAREST)), s)
    | '\x9f' -> Ok (Inumeric (UnOp (TF64, F_SQRT)), s)
    | '\xa0' -> Ok (Inumeric (BinOp (TF64, F_ADD)), s)
    | '\xa1' -> Ok (Inumeric (BinOp (TF64, F_SUB)), s)
    | '\xa2' -> Ok (Inumeric (BinOp (TF64, F_MUL)), s)
    | '\xa3' -> Ok (Inumeric (BinOp (TF64, F_DIV)), s)
    | '\xa4' -> Ok (Inumeric (BinOp (TF64, F_MIN)), s)
    | '\xa5' -> Ok (Inumeric (BinOp (TF64, F_MAX)), s)
    | '\xa6' -> Ok (Inumeric (BinOp (TF64, F_COPYSIGN)), s)
    (* cvt *)
    | '\xa7' -> Ok (Inumeric (CvtOp (TI32, CVT_WRAP, TI64)), s)
    | '\xa8' -> Ok (Inumeric (CvtOp (TI32, CVT_TRUNC_S, TF32)), s)
    | '\xa9' -> Ok (Inumeric (CvtOp (TI32, CVT_TRUNC_U, TF32)), s)
    | '\xaa' -> Ok (Inumeric (CvtOp (TI32, CVT_TRUNC_S, TF64)), s)
    | '\xab' -> Ok (Inumeric (CvtOp (TI32, CVT_TRUNC_U, TF64)), s)
    | '\xac' -> Ok (Inumeric (CvtOp (TI64, CVT_EXTEND_S, TI32)), s)
    | '\xad' -> Ok (Inumeric (CvtOp (TI64, CVT_EXTEND_U, TI32)), s)
    | '\xae' -> Ok (Inumeric (CvtOp (TI64, CVT_TRUNC_S, TF32)), s)
    | '\xaf' -> Ok (Inumeric (CvtOp (TI64, CVT_TRUNC_U, TF32)), s)
    | '\xb0' -> Ok (Inumeric (CvtOp (TI64, CVT_TRUNC_S, TF64)), s)
    | '\xb1' -> Ok (Inumeric (CvtOp (TI64, CVT_TRUNC_U, TF64)), s)
    | '\xb2' -> Ok (Inumeric (CvtOp (TF32, CVT_CONVERT_S, TI32)), s)
    | '\xb3' -> Ok (Inumeric (CvtOp (TF32, CVT_CONVERT_U, TI32)), s)
    | '\xb4' -> Ok (Inumeric (CvtOp (TF32, CVT_CONVERT_S, TI64)), s)
    | '\xb5' -> Ok (Inumeric (CvtOp (TF32, CVT_CONVERT_U, TI64)), s)
    | '\xb6' -> Ok (Inumeric (CvtOp (TF32, CVT_DEMOTE, TF64)), s)
    | '\xb7' -> Ok (Inumeric (CvtOp (TF64, CVT_CONVERT_S, TI32)), s)
    | '\xb8' -> Ok (Inumeric (CvtOp (TF64, CVT_CONVERT_U, TI32)), s)
    | '\xb9' -> Ok (Inumeric (CvtOp (TF64, CVT_CONVERT_S, TI64)), s)
    | '\xba' -> Ok (Inumeric (CvtOp (TF64, CVT_CONVERT_U, TI64)), s)
    | '\xbb' -> Ok (Inumeric (CvtOp (TF64, CVT_PROMOTE, TF32)), s)
    | '\xbc' -> Ok (Inumeric (CvtOp (TI32, CVT_REINTERPRET, TF32)), s)
    | '\xbd' -> Ok (Inumeric (CvtOp (TI64, CVT_REINTERPRET, TF64)), s)
    | '\xbe' -> Ok (Inumeric (CvtOp (TF32, CVT_REINTERPRET, TI32)), s)
    | '\xbf' -> Ok (Inumeric (CvtOp (TF64, CVT_REINTERPRET, TI64)), s)
    | _ -> Error "invalid numeric instr"

  and instr (s : S.t) : (instr option * S.t) or_err =
    let wrap = Result.map (fun (i, s) -> (Some i, s)) in
    let* t = S.peek_char s in
    match t with
    | None -> Ok (None, s)
    | Some t -> (
      match t with
      | '\x00' .. '\x04' | '\x0c' .. '\x11' -> icontrol s |> wrap
      | '\x1a' .. '\x1b' -> iparametric s |> wrap
      | '\x20' .. '\x24' -> ivariable s |> wrap
      | '\x28' .. '\x40' -> imemory s |> wrap
      | '\x41' .. '\xbf' -> inumric s |> wrap
      | _ -> Ok (None, s)
    )

  and instrs (s : S.t) : (instr list * S.t) or_err =
    let rec aux acc s =
      let* (i, s) = instr s in
      match i with
      | None -> Ok (List.rev acc, s)
      | Some i -> aux (i :: acc) s
    in
    aux [] s

  let expr (s : S.t) : (expr * S.t) or_err =
    let* (ins, s) = instrs s in
    let* s = S.consume_char '\x0b' s in
    Ok (ins, s)
end

module Module = struct
  let aux_parse_section (f : S.t -> ('a * S.t) or_err) (src : S.t option)
      : 'a array or_err
    =
    match src with
    | None -> Ok [||]
    | Some src ->
      let* (x, _) = aux_vec f src in
      Ok (Array.of_list x)

  let parse_type (src : S.t option) : functype array or_err =
    let f = Type.functype in
    aux_parse_section f src

  let parse_func (idx : S.t option) (code : S.t option) : func array or_err =
    match (idx, code) with
    | (None, Some _) -> Error "invalid funcsec"
    | (Some _, None) -> Error "invalid codesec"
    | (None, None) -> Ok [||]
    | (Some idx, Some code) ->
      let aux_locals (s : S.t) : (valtype list * S.t) or_err =
        let* (n, s) = Value.u32 s in
        let* (t, s) = Type.valtype s in
        let rec aux acc = function
          | 0 -> Ok (acc, s)
          | n -> aux (t :: acc) (n - 1)
        in
        aux [] n
      in
      let fcode (s : S.t) : ((valtype list * expr) * S.t) or_err =
        let* (size, s) = Value.u32 s in
        let* (code, s) = S.take size s in
        let* (locals, code) = aux_vec aux_locals code in
        let locals = List.flatten locals in
        let* (body, _) = Instruction.expr code in
        Ok ((locals, body), s)
      in
      let fidx (s : S.t) : (typeidx * S.t) or_err = Value.idx s in
      let mapf (sidx : S.t) (scode : S.t) : func array or_err =
        let* (idx, _) = aux_vec fidx sidx in
        let* (code, _) = aux_vec fcode scode in
        let f =
          List.map2
            (fun typei (locals, body) -> { typei; locals; body })
            idx
            code
        in
        Ok (Array.of_list f)
      in
      mapf idx code

  let parse_table (s : S.t option) : table array or_err =
    let f (s : S.t) =
      let* (ttype, s) = Type.tabletype s in
      Ok ({ ttype }, s)
    in
    aux_parse_section f s

  let parse_mem (s : S.t option) : mem array or_err =
    let f (s : S.t) =
      let* (mtype, s) = Type.memtype s in
      Ok ({ mtype }, s)
    in
    aux_parse_section f s

  let parse_global (s : S.t option) : global array or_err =
    let f (s : S.t) =
      let* (gtype, s) = Type.globaltype s in
      let* (init, s) = Instruction.expr s in
      Ok ({ gtype; init }, s)
    in
    aux_parse_section f s

  let parse_elem (s : S.t option) : elem array or_err =
    let f (s : S.t) =
      let* (table, s) = Value.idx s in
      let* (offset, s) = Instruction.expr s in
      let* (init, s) = aux_vec Value.idx s in
      Ok ({ table; offset; init }, s)
    in
    aux_parse_section f s

  let parse_data (s : S.t option) : data array or_err =
    let f (s : S.t) =
      let* (data, s) = Value.idx s in
      let* (offset, s) = Instruction.expr s in
      let* (init, s) = Value.byte s in
      Ok ({ data; offset; init }, s)
    in
    aux_parse_section f s

  let parse_start (s : S.t option) : start option or_err =
    match s with
    | None -> Ok None
    | Some s ->
      let* (func, _) = Value.idx s in
      Ok (Some { func })

  let parse_import (s : S.t option) : import array or_err =
    let aux_desc (s : S.t) : (importdesc * S.t) or_err =
      let* (t, s) = S.take_char s in
      match t with
      | '\x00' ->
        let* (x, s) = Value.idx s in
        Ok (ID_func x, s)
      | '\x01' ->
        let* (tt, s) = Type.tabletype s in
        Ok (ID_table tt, s)
      | '\x02' ->
        let* (mt, s) = Type.memtype s in
        Ok (ID_mem mt, s)
      | '\x03' ->
        let* (gt, s) = Type.globaltype s in
        Ok (ID_global gt, s)
      | _ -> Error "invalid import desc"
    in
    let f (s : S.t) =
      let* (modname, s) = Value.name s in
      let* (name, s) = Value.name s in
      let* (desc, s) = aux_desc s in
      Ok ({ modname; name; desc }, s)
    in
    aux_parse_section f s

  let parse_export (s : S.t option) : export array or_err =
    let aux_desc (s : S.t) : (exportdesc * S.t) or_err =
      let* (t, s) = S.take_char s in
      let* (i, s) = Value.idx s in
      match t with
      | '\x00' -> Ok (ED_func i, s)
      | '\x01' -> Ok (ED_table i, s)
      | '\x02' -> Ok (ED_mem i, s)
      | '\x03' -> Ok (ED_global i, s)
      | _ -> Error "invalid export desc"
    in
    let f (s : S.t) =
      let* (name, s) = Value.name s in
      let* (desc, s) = aux_desc s in
      Ok ({ name; desc }, s)
    in
    aux_parse_section f s

  let rec aux_take_sec (sid : char) (src : S.t) : (S.t option * S.t) or_err =
    let* id = S.peek_char src in
    match id with
    | Some id when Char.equal id '\x00' ->
      let* (size, src) = Value.u32 src in
      let* (_, src) = S.take size src in
      aux_take_sec sid src
    | Some id when Char.equal id sid ->
      let* (size, src) = Value.u32 src in
      let* (sec, src) = S.take size src in
      Ok (Some sec, src)
    | Some _ | None -> Ok (None, src)

  let magic = "\x00\x61\x73\x6d"

  let version = "\x01\x00\x00\x00"

  let parse_module (src : S.t) : moduledef or_err =
    let* src = S.consume magic src in
    let* src = S.consume version src in

    let* (typesec, src) = aux_take_sec '\x01' src in
    let* (importsec, src) = aux_take_sec '\x02' src in
    let* (funcsec, src) = aux_take_sec '\x03' src in
    let* (tablesec, src) = aux_take_sec '\x04' src in
    let* (memsec, src) = aux_take_sec '\x05' src in
    let* (globalsec, src) = aux_take_sec '\x06' src in
    let* (exportsec, src) = aux_take_sec '\x07' src in
    let* (startsec, src) = aux_take_sec '\x08' src in
    let* (elemsec, src) = aux_take_sec '\x09' src in
    let* (codesec, src) = aux_take_sec '\x0a' src in
    let* (datasec, _) = aux_take_sec '\x0b' src in

    let* types = parse_type typesec in
    let* funcs = parse_func funcsec codesec in
    let* tables = parse_table tablesec in
    let* mems = parse_mem memsec in
    let* globals = parse_global globalsec in
    let* elem = parse_elem elemsec in
    let* data = parse_data datasec in
    let* start = parse_start startsec in
    let* imports = parse_import importsec in
    let* exports = parse_export exportsec in

    let m =
      {
        types;
        funcs;
        tables;
        mems;
        globals;
        elem;
        data;
        start;
        imports;
        exports;
      }
    in
    Ok m

  let parse = parse_module
end

let parse s = Module.parse (S.of_string s)
