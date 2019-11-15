open! Containers
open Types

let ( let* ) = Result.( >>= )

(* let ( let+ ) = Result.( >|= ) *)

type 'a or_err = ('a, string) result

module S = struct
  type t = {
    s: string;
    left: int;
    right: int;
  }

  let empty = { s = ""; left = 0; right = 0 }

  let of_string (s : string) : t = { s; left = 0; right = String.length s }

  let to_string ({ s; left; right } : t) : string =
    String.sub s left (right - left)

  let concat (t1 : t) (t2 : t) : t =
    let s1 = to_string t1 in
    let s2 = to_string t2 in
    let s = s1 ^ s2 in
    of_string s

  let take (len : int) (src : t) : (t * t) or_err =
    let { left; right; _ } = src in
    if left + len <= right
    then (
      let sub = { src with right = left + len } in
      let src = { src with left = left + len } in
      Ok (sub, src)
    )
    else Error "EOF"

  let take_char (src : t) : (char * t) or_err =
    let { s; left; right } = src in
    if left = right
    then Error "EOF"
    else (
      let ch = s.[left] in
      let src = { src with left = left + 1 } in
      Ok (ch, src)
    )

  let take_int (src : t) : (int * t) or_err =
    let* (ch, src) = take_char src in
    let n = Char.code ch in
    Ok (n, src)

  let peek_char (src : t) : char or_err =
    let { s; left; right } = src in
    if left = right
    then Error "EOF"
    else (
      let ch = s.[left] in
      Ok ch
    )

  let peek_int (src : t) : int or_err =
    let* ch = peek_char src in
    let n = Char.code ch in
    Ok n

  let consume (pattern : string) (src : t) : t or_err =
    let len = String.length pattern in
    let* (sub, src) = take len src in
    if String.equal pattern (to_string sub)
    then Ok src
    else Error "consume | failure"

  let consume_char (p : char) (src : t) : t or_err =
    let* (ch, src) = take_char src in
    if Char.equal p ch then Ok src else Error "consume_char | failure"

  let skip (len : int) (src : t) : t or_err =
    let { left; right; _ } = src in
    if left + len <= right
    then (
      let src = { src with left = left + len } in
      Ok src
    )
    else Error "EOF"
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

  let uint (src : S.t) : (int * S.t) or_err =
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
    let* (size, src) = uint src in
    let* (b, src) = S.take size src in
    let b = b |> S.to_string |> Bytes.of_string in
    Ok (b, src)

  let name (src : S.t) : (string * S.t) or_err =
    let* (size, src) = uint src in
    let* (n, src) = S.take size src in
    let n = S.to_string n in
    Ok (n, src)

  let idx = uint
end

let aux_vec (src : S.t) (f : S.t -> ('a * S.t) or_err) : ('a list * S.t) or_err =
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
  let* (n, src) = Value.uint src in
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
    if Char.equal c '\x40'
    then
      let* s = S.skip 1 s in
      Ok ([], s)
    else
      let* (v, s) = valtype s in
      Ok ([ v ], s)

  let functype (s : S.t) : (functype * S.t) or_err =
    let* s = S.consume_char '\x60' s in
    let* (t1, s) = aux_vec s valtype in
    let* (t2, s) = aux_vec s valtype in
    let func = (t1, t2) in
    Ok (func, s)

  let limits (s : S.t) : (limits * S.t) or_err =
    let* (t, s) = S.take_char s in
    match t with
      | '\x00' ->
        let* (n, s) = Value.uint s in
        let limits = { min = n; max = None } in
        Ok (limits, s)
      | '\x01' ->
        let* (n, s) = Value.uint s in
        let* (m, s) = Value.uint s in
        let limits = { min = n; max = Some m } in
        Ok (limits, s)
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
    let* (align, s) = Value.uint s in
    let* (offset, s) = Value.uint s in
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
        let* (ls, s) = aux_vec s Value.idx in
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
      | '\x00' .. '\x04' | '\x0c' .. '\x11' -> icontrol s |> wrap
      | '\x1a' .. '\x1b' -> iparametric s |> wrap
      | '\x20' .. '\x24' -> ivariable s |> wrap
      | '\x28' .. '\x40' -> imemory s |> wrap
      | '\x41' .. '\xbf' -> inumric s |> wrap
      | _ -> Ok (None, s)

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
  type sections = {
    typesec: S.t;
    importsec: S.t;
    funcsec: S.t;
    tablesec: S.t;
    memsec: S.t;
    globalsec: S.t;
    exportsec: S.t;
    startsec: S.t;
    elemsec: S.t;
    codesec: S.t;
    datasec: S.t;
  }

  let empty_sections =
    {
      typesec = S.empty;
      importsec = S.empty;
      funcsec = S.empty;
      tablesec = S.empty;
      memsec = S.empty;
      globalsec = S.empty;
      exportsec = S.empty;
      startsec = S.empty;
      elemsec = S.empty;
      codesec = S.empty;
      datasec = S.empty;
    }

  let aux_read_section (src : S.t) : (int * S.t * S.t) or_err =
    let* (id, src) = S.take_int src in
    if id > 11
    then Error "unsupported section"
    else
      let* (size, src) = Value.uint src in
      let* (sec, src) = S.take size src in
      Ok (id, sec, src)

  let aux_split_module (src : S.t) : sections or_err =
    let rec aux (acc : sections) (src : S.t) : sections or_err =
      let ({ left; right; _ } : S.t) = src in
      if left = right
      then Ok acc
      else
        let* (sec_id, sec, src) = aux_read_section src in
        let acc =
          match sec_id with
            | 0 -> acc
            | 1 -> { acc with typesec = S.concat acc.typesec sec }
            | 2 -> { acc with importsec = S.concat acc.importsec sec }
            | 3 -> { acc with funcsec = S.concat acc.funcsec sec }
            | 4 -> { acc with tablesec = S.concat acc.tablesec sec }
            | 5 -> { acc with memsec = S.concat acc.memsec sec }
            | 6 -> { acc with globalsec = S.concat acc.globalsec sec }
            | 7 -> { acc with exportsec = S.concat acc.exportsec sec }
            | 8 -> { acc with startsec = S.concat acc.startsec sec }
            | 9 -> { acc with elemsec = S.concat acc.elemsec sec }
            | 10 -> { acc with codesec = S.concat acc.codesec sec }
            | 11 -> { acc with datasec = S.concat acc.datasec sec }
            | _ -> failwith "never"
        in
        aux acc src
    in
    aux empty_sections src

  let parse_types _x = failwith "TODO"

  let parse_funcs _x _x = failwith "TODO"

  let parse_tables _x = failwith "TODO"

  let parse_mems _x = failwith "TODO"

  let parse_globals _x = failwith "TODO"

  let parse_elem _x = failwith "TODO"

  let parse_data _x = failwith "TODO"

  let parse_start _x = failwith "TODO"

  let parse_imports _x = failwith "TODO"

  let parse_exports _x = failwith "TODO"

  let parse_module (src : S.t) : moduledef or_err =
    let magic = "\x00\x61\x73\x6d" in
    let version = "\x01\x00\x00\x00" in
    let* src = S.consume magic src in
    let* src = S.consume version src in
    let* sec = aux_split_module src in

    let* types = parse_types sec.typesec in
    let* funcs = parse_funcs sec.funcsec sec.codesec in
    let* tables = parse_tables sec.tablesec in
    let* mems = parse_mems sec.memsec in
    let* globals = parse_globals sec.globalsec in
    let* elem = parse_elem sec.elemsec in
    let* data = parse_data sec.datasec in
    let* start = parse_start sec.startsec in
    let* imports = parse_imports sec.importsec in
    let* exports = parse_exports sec.exportsec in

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

  let parse (src : S.t) : moduledef =
    let m = parse_module src in
    Result.get_or_failwith m
end

let parse s = Module.parse (S.of_string s)
