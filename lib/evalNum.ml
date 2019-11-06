open Types

let unop = function
    (* TI32 *)
    | (TI32, I_CLZ, I32 i) -> Some (I32 (Nint32.UnOp.clz i))
    | (TI32, I_CTZ, I32 i) -> Some (I32 (Nint32.UnOp.ctz i))
    | (TI32, I_POPCONT, I32 i) -> Some (I32 (Nint32.UnOp.popcont i))
    (* TI64 *)
    | (TI64, I_CLZ, I64 i) -> Some (I64 (Nint64.UnOp.clz i))
    | (TI64, I_CTZ, I64 i) -> Some (I64 (Nint64.UnOp.ctz i))
    | (TI64, I_POPCONT, I64 i) -> Some (I64 (Nint64.UnOp.popcont i))
    (* TF32 *)
    | (TF32, F_ABS, F32 i) -> Some (F32 (Nfloat32.UnOp.abs i))
    | (TF32, F_NEG, F32 i) -> Some (F32 (Nfloat32.UnOp.neg i))
    | (TF32, F_SQRT, F32 i) -> Some (F32 (Nfloat32.UnOp.sqrt i))
    | (TF32, F_CEIL, F32 i) -> Some (F32 (Nfloat32.UnOp.ceil i))
    | (TF32, F_FLOOR, F32 i) -> Some (F32 (Nfloat32.UnOp.floor i))
    | (TF32, F_TRUNC, F32 i) -> Some (F32 (Nfloat32.UnOp.trunc i))
    | (TF32, F_NEAREST, F32 i) -> Some (F32 (Nfloat32.UnOp.nearest i))
    (* TF64 *)
    | (TF64, F_ABS, F64 i) -> Some (F64 (Nfloat64.UnOp.abs i))
    | (TF64, F_NEG, F64 i) -> Some (F64 (Nfloat64.UnOp.neg i))
    | (TF64, F_SQRT, F64 i) -> Some (F64 (Nfloat64.UnOp.sqrt i))
    | (TF64, F_CEIL, F64 i) -> Some (F64 (Nfloat64.UnOp.ceil i))
    | (TF64, F_FLOOR, F64 i) -> Some (F64 (Nfloat64.UnOp.floor i))
    | (TF64, F_TRUNC, F64 i) -> Some (F64 (Nfloat64.UnOp.trunc i))
    | (TF64, F_NEAREST, F64 i) -> Some (F64 (Nfloat64.UnOp.nearest i))
    | _ -> None


let binop = function
    (* TI32 *)
    | (TI32, I_ADD, I32 x, I32 y) -> Some (I32 (Nint32.BinOp.add x y))
    | (TI32, I_SUB, I32 x, I32 y) -> Some (I32 (Nint32.BinOp.sub x y))
    | (TI32, I_MUL, I32 x, I32 y) -> Some (I32 (Nint32.BinOp.mul x y))
    | (TI32, I_DIV_S, I32 x, I32 y) -> Some (I32 (Nint32.BinOp.div_s x y))
    | (TI32, I_DIV_U, I32 x, I32 y) -> Some (I32 (Nint32.BinOp.div_u x y))
    | (TI32, I_REM_S, I32 x, I32 y) -> Some (I32 (Nint32.BinOp.rem_s x y))
    | (TI32, I_REM_U, I32 x, I32 y) -> Some (I32 (Nint32.BinOp.rem_u x y))
    | (TI32, I_AND, I32 x, I32 y) -> Some (I32 (Nint32.BinOp.logand x y))
    | (TI32, I_OR, I32 x, I32 y) -> Some (I32 (Nint32.BinOp.logor x y))
    | (TI32, I_XOR, I32 x, I32 y) -> Some (I32 (Nint32.BinOp.logxor x y))
    | (TI32, I_SHL, I32 x, I32 y) -> Some (I32 (Nint32.BinOp.shl x y))
    | (TI32, I_SHR_S, I32 x, I32 y) -> Some (I32 (Nint32.BinOp.shr_s x y))
    | (TI32, I_SHR_U, I32 x, I32 y) -> Some (I32 (Nint32.BinOp.shr_u x y))
    | (TI32, I_ROTL, I32 x, I32 y) -> Some (I32 (Nint32.BinOp.rotl x y))
    | (TI32, I_ROTR, I32 x, I32 y) -> Some (I32 (Nint32.BinOp.rotr x y))
    (* TI64 *)
    | (TI64, I_ADD, I64 x, I64 y) -> Some (I64 (Nint64.BinOp.add x y))
    | (TI64, I_SUB, I64 x, I64 y) -> Some (I64 (Nint64.BinOp.sub x y))
    | (TI64, I_MUL, I64 x, I64 y) -> Some (I64 (Nint64.BinOp.mul x y))
    | (TI64, I_DIV_S, I64 x, I64 y) -> Some (I64 (Nint64.BinOp.div_s x y))
    | (TI64, I_DIV_U, I64 x, I64 y) -> Some (I64 (Nint64.BinOp.div_u x y))
    | (TI64, I_REM_S, I64 x, I64 y) -> Some (I64 (Nint64.BinOp.rem_s x y))
    | (TI64, I_REM_U, I64 x, I64 y) -> Some (I64 (Nint64.BinOp.rem_u x y))
    | (TI64, I_AND, I64 x, I64 y) -> Some (I64 (Nint64.BinOp.logand x y))
    | (TI64, I_OR, I64 x, I64 y) -> Some (I64 (Nint64.BinOp.logor x y))
    | (TI64, I_XOR, I64 x, I64 y) -> Some (I64 (Nint64.BinOp.logxor x y))
    | (TI64, I_SHL, I64 x, I64 y) -> Some (I64 (Nint64.BinOp.logxor x y))
    | (TI64, I_SHR_S, I64 x, I64 y) -> Some (I64 (Nint64.BinOp.logxor x y))
    | (TI64, I_SHR_U, I64 x, I64 y) -> Some (I64 (Nint64.BinOp.logxor x y))
    | (TI64, I_ROTL, I64 x, I64 y) -> Some (I64 (Nint64.BinOp.logxor x y))
    | (TI64, I_ROTR, I64 x, I64 y) -> Some (I64 (Nint64.BinOp.logxor x y))
    (* TF32 *)
    | (TF32, F_ADD, F32 x, F32 y) -> Some (F32 (Nfloat32.BinOp.add x y))
    | (TF32, F_SUB, F32 x, F32 y) -> Some (F32 (Nfloat32.BinOp.sub x y))
    | (TF32, F_MUL, F32 x, F32 y) -> Some (F32 (Nfloat32.BinOp.mul x y))
    | (TF32, F_DIV, F32 x, F32 y) -> Some (F32 (Nfloat32.BinOp.div x y))
    | (TF32, F_MIN, F32 x, F32 y) -> Some (F32 (Nfloat32.BinOp.min x y))
    | (TF32, F_MAX, F32 x, F32 y) -> Some (F32 (Nfloat32.BinOp.max x y))
    | (TF32, F_COPYSIGN, F32 x, F32 y) ->
        Some (F32 (Nfloat32.BinOp.copy_sign x y))
    (* TF64 *)
    | (TF64, F_ADD, F64 x, F64 y) -> Some (F64 (Nfloat64.BinOp.add x y))
    | (TF64, F_SUB, F64 x, F64 y) -> Some (F64 (Nfloat64.BinOp.sub x y))
    | (TF64, F_MUL, F64 x, F64 y) -> Some (F64 (Nfloat64.BinOp.mul x y))
    | (TF64, F_DIV, F64 x, F64 y) -> Some (F64 (Nfloat64.BinOp.div x y))
    | (TF64, F_MIN, F64 x, F64 y) -> Some (F64 (Nfloat64.BinOp.min x y))
    | (TF64, F_MAX, F64 x, F64 y) -> Some (F64 (Nfloat64.BinOp.max x y))
    | (TF64, F_COPYSIGN, F64 x, F64 y) ->
        Some (F64 (Nfloat64.BinOp.copy_sign x y))
    | _ -> None


let testop = function
    | (TI32, I_EQZ, I32 i) -> Some (Nint32.equal i 0l)
    | (TI64, I_EQZ, I64 i) -> Some (Nint64.equal i 0L)
    | _ -> None


let relop = function
    (* TI32 *)
    | (TI32, I_EQ, I32 x, I32 y) -> Some (Nint32.RelOp.eq x y)
    | (TI32, I_NE, I32 x, I32 y) -> Some (Nint32.RelOp.ne x y)
    | (TI32, I_LT_S, I32 x, I32 y) -> Some (Nint32.RelOp.lt_s x y)
    | (TI32, I_LT_U, I32 x, I32 y) -> Some (Nint32.RelOp.lt_u x y)
    | (TI32, I_LE_S, I32 x, I32 y) -> Some (Nint32.RelOp.le_s x y)
    | (TI32, I_LE_U, I32 x, I32 y) -> Some (Nint32.RelOp.le_u x y)
    | (TI32, I_GT_S, I32 x, I32 y) -> Some (Nint32.RelOp.gt_s x y)
    | (TI32, I_GT_U, I32 x, I32 y) -> Some (Nint32.RelOp.gt_u x y)
    | (TI32, I_GE_S, I32 x, I32 y) -> Some (Nint32.RelOp.ge_s x y)
    | (TI32, I_GE_U, I32 x, I32 y) -> Some (Nint32.RelOp.ge_u x y)
    (* TI64 *)
    | (TI64, I_EQ, I64 x, I64 y) -> Some (Nint64.RelOp.eq x y)
    | (TI64, I_NE, I64 x, I64 y) -> Some (Nint64.RelOp.ne x y)
    | (TI64, I_LT_S, I64 x, I64 y) -> Some (Nint64.RelOp.lt_s x y)
    | (TI64, I_LT_U, I64 x, I64 y) -> Some (Nint64.RelOp.lt_u x y)
    | (TI64, I_LE_S, I64 x, I64 y) -> Some (Nint64.RelOp.le_s x y)
    | (TI64, I_LE_U, I64 x, I64 y) -> Some (Nint64.RelOp.le_u x y)
    | (TI64, I_GT_S, I64 x, I64 y) -> Some (Nint64.RelOp.gt_s x y)
    | (TI64, I_GT_U, I64 x, I64 y) -> Some (Nint64.RelOp.gt_u x y)
    | (TI64, I_GE_S, I64 x, I64 y) -> Some (Nint64.RelOp.ge_s x y)
    | (TI64, I_GE_U, I64 x, I64 y) -> Some (Nint64.RelOp.ge_u x y)
    (* TF32 *)
    | (TF32, F_EQ, F32 x, F32 y) -> Some (Nfloat32.RelOp.eq x y)
    | (TF32, F_NE, F32 x, F32 y) -> Some (Nfloat32.RelOp.ne x y)
    | (TF32, F_LT, F32 x, F32 y) -> Some (Nfloat32.RelOp.lt x y)
    | (TF32, F_LE, F32 x, F32 y) -> Some (Nfloat32.RelOp.le x y)
    | (TF32, F_GT, F32 x, F32 y) -> Some (Nfloat32.RelOp.gt x y)
    | (TF32, F_GE, F32 x, F32 y) -> Some (Nfloat32.RelOp.ge x y)
    (* TF64 *)
    | (TF64, F_EQ, F64 x, F64 y) -> Some (Nfloat64.RelOp.eq x y)
    | (TF64, F_NE, F64 x, F64 y) -> Some (Nfloat64.RelOp.ne x y)
    | (TF64, F_LT, F64 x, F64 y) -> Some (Nfloat64.RelOp.lt x y)
    | (TF64, F_LE, F64 x, F64 y) -> Some (Nfloat64.RelOp.le x y)
    | (TF64, F_GT, F64 x, F64 y) -> Some (Nfloat64.RelOp.gt x y)
    | (TF64, F_GE, F64 x, F64 y) -> Some (Nfloat64.RelOp.ge x y)
    | _ -> None


let cvtop = function
    (* TI32 *)
    | (TI32, CVT_WRAP, TI64, I64 x) -> Some (I32 (Nint32.CvtOp.wrap_i64 x))
    | (TI32, CVT_TRUNC_S, TF32, F32 x) ->
        Some (I32 (Nint32.CvtOp.trunc_s_f32 x))
    | (TI32, CVT_TRUNC_S, TF64, F64 x) ->
        Some (I32 (Nint32.CvtOp.trunc_s_f64 x))
    | (TI32, CVT_TRUNC_U, TF32, F32 x) ->
        Some (I32 (Nint32.CvtOp.trunc_u_f32 x))
    | (TI32, CVT_TRUNC_U, TF64, F64 x) ->
        Some (I32 (Nint32.CvtOp.trunc_u_f64 x))
    | (TI32, CVT_REINTERPRET, TF32, F32 x) ->
        Some (I32 (Nint32.CvtOp.reinterpret_f32 x))
    (* TI64 *)
    | (TI64, CVT_EXTEND_S, TI32, I32 x) ->
        Some (I64 (Nint64.CvtOp.extend_s_i32 x))
    | (TI64, CVT_EXTEND_U, TI32, I32 x) ->
        Some (I64 (Nint64.CvtOp.extend_u_i32 x))
    | (TI64, CVT_TRUNC_S, TF32, F32 x) ->
        Some (I64 (Nint64.CvtOp.trunc_s_f32 x))
    | (TI64, CVT_TRUNC_S, TF64, F64 x) ->
        Some (I64 (Nint64.CvtOp.trunc_s_f64 x))
    | (TI64, CVT_TRUNC_U, TF32, F32 x) ->
        Some (I64 (Nint64.CvtOp.trunc_u_f32 x))
    | (TI64, CVT_TRUNC_U, TF64, F64 x) ->
        Some (I64 (Nint64.CvtOp.trunc_u_f64 x))
    | (TI64, CVT_REINTERPRET, TF64, F64 x) ->
        Some (I64 (Nint64.CvtOp.reinterpret_f64 x))
    (* TF32 *)
    | (TF32, CVT_CONVERT_S, TI32, I32 x) ->
        Some (F32 (Nfloat32.CvtOp.convert_s_i32 x))
    | (TF32, CVT_CONVERT_S, TI64, I64 x) ->
        Some (F32 (Nfloat32.CvtOp.convert_s_i64 x))
    | (TF32, CVT_CONVERT_U, TI32, I32 x) ->
        Some (F32 (Nfloat32.CvtOp.convert_u_i32 x))
    | (TF32, CVT_CONVERT_U, TI64, I64 x) ->
        Some (F32 (Nfloat32.CvtOp.convert_u_i64 x))
    | (TF32, CVT_DEMOTE, TF64, F64 x) ->
        Some (F32 (Nfloat32.CvtOp.demote_f64 x))
    | (TF32, CVT_REINTERPRET, TI32, I32 x) ->
        Some (F32 (Nfloat32.CvtOp.reinterpret_i32 x))
    (* TF64 *)
    | (TF64, CVT_CONVERT_S, TI32, I32 x) ->
        Some (F64 (Nfloat64.CvtOp.convert_s_i32 x))
    | (TF64, CVT_CONVERT_S, TI64, I64 x) ->
        Some (F64 (Nfloat64.CvtOp.convert_s_i64 x))
    | (TF64, CVT_CONVERT_U, TI32, I32 x) ->
        Some (F64 (Nfloat64.CvtOp.convert_u_i32 x))
    | (TF64, CVT_CONVERT_U, TI64, I64 x) ->
        Some (F64 (Nfloat64.CvtOp.convert_u_i64 x))
    | (TF64, CVT_PROMOTE, TF32, F32 x) ->
        Some (F64 (Nfloat64.CvtOp.promote_f32 x))
    | (TF64, CVT_REINTERPRET, TI64, I64 x) ->
        Some (F64 (Nfloat64.CvtOp.reinterpret_i64 x))
    | _ -> None
