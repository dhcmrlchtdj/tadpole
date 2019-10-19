open! Containers
open Ast
module Float64 = Stdlib.Float

let unop = function
    (* I32 *)
    | I_CLZ, TI32, I32 i ->
        if Int32.equal i 0l
        then Some (I32 32l)
        else
          let j = ref i in
          let cnt = ref 0 in
          let break = ref false in
          let loop = ref 0 in
          while Bool.equal !break false && !loop < 32 do
            let r = Int32.logand Int32.min_int !j in
            if Int32.equal r 0l
            then (
              incr cnt ;
              incr loop ;
              j := Int32.shift_left !j 1 )
            else break := true
          done ;
          Some (I32 (Int32.of_int !cnt))
    | I_CTZ, TI32, I32 i ->
        if Int32.equal i 0l
        then Some (I32 32l)
        else
          let j = ref i in
          let cnt = ref 0 in
          let break = ref false in
          let loop = ref 0 in
          while Bool.equal !break false && !loop < 32 do
            let r = Int32.logand 1l !j in
            if Int32.equal r 0l
            then (
              incr cnt ;
              incr loop ;
              j := Int32.shift_right_logical !j 1 )
            else break := true
          done ;
          Some (I32 (Int32.of_int !cnt))
    | I_POPCONT, TI32, I32 i ->
        let j = ref i in
        let cnt = ref 0 in
        for _ = 0 to 31 do
          let r = Int32.logand 1l !j in
          if Int32.equal r 1l then incr cnt ;
          j := Int32.shift_right_logical !j 1
        done ;
        Some (I32 (Int32.of_int !cnt))
    (* I64 *)
    | I_CLZ, TI64, I64 i ->
        if Int64.equal i 0L
        then Some (I64 64L)
        else
          let j = ref i in
          let cnt = ref 0 in
          let break = ref false in
          let loop = ref 0 in
          while Bool.equal !break false && !loop < 64 do
            let r = Int64.logand Int64.min_int !j in
            if Int64.equal r 0L
            then (
              incr cnt ;
              incr loop ;
              j := Int64.shift_left !j 1 )
            else break := true
          done ;
          Some (I64 (Int64.of_int !cnt))
    | I_CTZ, TI64, I64 i ->
        if Int64.equal i 0L
        then Some (I64 64L)
        else
          let j = ref i in
          let cnt = ref 0 in
          let break = ref false in
          let loop = ref 0 in
          while Bool.equal !break false && !loop < 64 do
            let r = Int64.logand 1L !j in
            if Int64.equal r 0L
            then (
              incr cnt ;
              incr loop ;
              j := Int64.shift_right_logical !j 1 )
            else break := true
          done ;
          Some (I64 (Int64.of_int !cnt))
    | I_POPCONT, TI64, I64 i ->
        let j = ref i in
        let cnt = ref 0 in
        for _ = 0 to 31 do
          let r = Int64.logand 1L !j in
          if Int64.equal r 1L then incr cnt ;
          j := Int64.shift_right_logical !j 1
        done ;
        Some (I64 (Int64.of_int !cnt))
    (* F64 *)
    | F_ABS, TF64, F64 i -> Some (F64 (Float64.abs i))
    | F_NEG, TF64, F64 i -> Some (F64 (Float64.neg i))
    | F_SQRT, TF64, F64 i -> Some (F64 (Float64.sqrt i))
    | F_CEIL, TF64, F64 i -> Some (F64 (Float64.ceil i))
    | F_FLOOR, TF64, F64 i -> Some (F64 (Float64.floor i))
    | F_TRUNC, TF64, F64 i -> Some (F64 (Float64.trunc i))
    | F_NEAREST, TF64, F64 i -> Some (F64 (Float64.round i))
    (* F32 *)
    | F_ABS, TF32, F32 i -> Some (F32 (Float32.abs i))
    | F_NEG, TF32, F32 i -> Some (F32 (Float32.neg i))
    | F_SQRT, TF32, F32 i -> Some (F32 (Float32.sqrt i))
    | F_CEIL, TF32, F32 i -> Some (F32 (Float32.ceil i))
    | F_FLOOR, TF32, F32 i -> Some (F32 (Float32.floor i))
    | F_TRUNC, TF32, F32 i -> Some (F32 (Float32.trunc i))
    | F_NEAREST, TF32, F32 i -> Some (F32 (Float32.nearest i))
    | _ -> None

let binop = function
    (* I32 *)
    | I_ADD, TI32, I32 x, I32 y -> Some (I32 (Int32.add x y))
    | I_SUB, TI32, I32 x, I32 y -> Some (I32 (Int32.sub x y))
    | I_MUL, TI32, I32 x, I32 y -> Some (I32 (Int32.mul x y))
    | I_DIV_S, TI32, I32 x, I32 y -> Some (I32 (Int32.div x y))
    | I_DIV_U, TI32, I32 x, I32 y -> Some (I32 (Int32.unsigned_div x y))
    | I_REM_S, TI32, I32 x, I32 y -> Some (I32 (Int32.rem x y))
    | I_REM_U, TI32, I32 x, I32 y -> Some (I32 (Int32.unsigned_rem x y))
    | I_AND, TI32, I32 x, I32 y -> Some (I32 (Int32.logand x y))
    | I_OR, TI32, I32 x, I32 y -> Some (I32 (Int32.logor x y))
    | I_XOR, TI32, I32 x, I32 y -> Some (I32 (Int32.logxor x y))
    | I_SHL, TI32, I32 x, I32 y ->
        let k32 = Int32.rem y 32l in
        let k = Int32.to_int k32 in
        Some (I32 (Int32.shift_left x k))
    | I_SHR_S, TI32, I32 x, I32 y ->
        let k32 = Int32.rem y 32l in
        let k = Int32.to_int k32 in
        Some (I32 (Int32.shift_right_logical x k))
    | I_SHR_U, TI32, I32 x, I32 y ->
        let k32 = Int32.rem y 32l in
        let k = Int32.to_int k32 in
        Some (I32 (Int32.shift_right x k))
    | I_ROTL, TI32, I32 x, I32 y ->
        let k32 = Int32.rem y 32l in
        let k = Int32.to_int k32 in
        let l = Int32.shift_left x k in
        let r = Int32.shift_right x (32 - k) in
        Some (I32 (Int32.add l r))
    | I_ROTR, TI32, I32 x, I32 y ->
        let k32 = Int32.rem y 32l in
        let k = Int32.to_int k32 in
        let l = Int32.shift_left x (32 - k) in
        let r = Int32.shift_right x k in
        Some (I32 (Int32.add l r))
    (* I64 *)
    | I_ADD, TI64, I64 x, I64 y -> Some (I64 (Int64.add x y))
    | I_SUB, TI64, I64 x, I64 y -> Some (I64 (Int64.sub x y))
    | I_MUL, TI64, I64 x, I64 y -> Some (I64 (Int64.mul x y))
    | I_DIV_S, TI64, I64 x, I64 y -> Some (I64 (Int64.div x y))
    | I_DIV_U, TI64, I64 x, I64 y -> Some (I64 (Int64.unsigned_div x y))
    | I_REM_S, TI64, I64 x, I64 y -> Some (I64 (Int64.rem x y))
    | I_REM_U, TI64, I64 x, I64 y -> Some (I64 (Int64.unsigned_rem x y))
    | I_AND, TI64, I64 x, I64 y -> Some (I64 (Int64.logand x y))
    | I_OR, TI64, I64 x, I64 y -> Some (I64 (Int64.logor x y))
    | I_XOR, TI64, I64 x, I64 y -> Some (I64 (Int64.logxor x y))
    | I_SHL, TI64, I64 x, I64 y ->
        let k64 = Int64.rem y 64L in
        let k = Int64.to_int k64 in
        Some (I64 (Int64.shift_left x k))
    | I_SHR_S, TI64, I64 x, I64 y ->
        let k64 = Int64.rem y 64L in
        let k = Int64.to_int k64 in
        Some (I64 (Int64.shift_right_logical x k))
    | I_SHR_U, TI64, I64 x, I64 y ->
        let k64 = Int64.rem y 64L in
        let k = Int64.to_int k64 in
        Some (I64 (Int64.shift_right x k))
    | I_ROTL, TI64, I64 x, I64 y ->
        let k64 = Int64.rem y 64L in
        let k = Int64.to_int k64 in
        let l = Int64.shift_left x k in
        let r = Int64.shift_right x (64 - k) in
        Some (I64 (Int64.add l r))
    | I_ROTR, TI64, I64 x, I64 y ->
        let k64 = Int64.rem y 64L in
        let k = Int64.to_int k64 in
        let l = Int64.shift_left x (64 - k) in
        let r = Int64.shift_right x k in
        Some (I64 (Int64.add l r))
    (* F64 *)
    | F_ADD, TF64, F64 x, F64 y -> Some (F64 (Float64.add x y))
    | F_SUB, TF64, F64 x, F64 y -> Some (F64 (Float64.sub x y))
    | F_MUL, TF64, F64 x, F64 y -> Some (F64 (Float64.mul x y))
    | F_DIV, TF64, F64 x, F64 y -> Some (F64 (Float64.div x y))
    | F_MIN, TF64, F64 x, F64 y -> Some (F64 (Float64.min x y))
    | F_MAX, TF64, F64 x, F64 y -> Some (F64 (Float64.max x y))
    | F_COPYSIGN, TF64, F64 x, F64 y -> Some (F64 (Float64.copy_sign x y))
    (* F32 *)
    | F_ADD, TF32, F32 x, F32 y -> Some (F32 (Float32.add x y))
    | F_SUB, TF32, F32 x, F32 y -> Some (F32 (Float32.sub x y))
    | F_MUL, TF32, F32 x, F32 y -> Some (F32 (Float32.mul x y))
    | F_DIV, TF32, F32 x, F32 y -> Some (F32 (Float32.div x y))
    | F_MIN, TF32, F32 x, F32 y -> Some (F32 (Float32.min x y))
    | F_MAX, TF32, F32 x, F32 y -> Some (F32 (Float32.max x y))
    | F_COPYSIGN, TF32, F32 x, F32 y -> Some (F32 (Float32.copy_sign x y))
    | _ -> None

let testop = function
    | I_EQZ, TI32, I32 i -> Some (Int32.equal i 0l)
    | I_EQZ, TI64, I64 i -> Some (Int64.equal i 0L)
    | _ -> None

let relop = function
    (* I32 *)
    | I_EQ, TI32, I32 x, I32 y -> Some (Int32.equal x y)
    | I_NE, TI32, I32 x, I32 y -> Some (not (Int32.equal x y))
    | I_LT_S, TI32, I32 x, I32 y -> Some (Int32.compare x y < 0)
    | I_LT_U, TI32, I32 x, I32 y -> Some (Int32.unsigned_compare x y < 0)
    | I_LE_S, TI32, I32 x, I32 y -> Some (Int32.compare x y <= 0)
    | I_LE_U, TI32, I32 x, I32 y -> Some (Int32.unsigned_compare x y <= 0)
    | I_GT_S, TI32, I32 x, I32 y -> Some (Int32.compare x y > 0)
    | I_GT_U, TI32, I32 x, I32 y -> Some (Int32.unsigned_compare x y > 0)
    | I_GE_S, TI32, I32 x, I32 y -> Some (Int32.compare x y >= 0)
    | I_GE_U, TI32, I32 x, I32 y -> Some (Int32.unsigned_compare x y >= 0)
    (* I64 *)
    | I_EQ, TI64, I64 x, I64 y -> Some (Int64.equal x y)
    | I_NE, TI64, I64 x, I64 y -> Some (not (Int64.equal x y))
    | I_LT_S, TI64, I64 x, I64 y -> Some (Int64.compare x y < 0)
    | I_LT_U, TI64, I64 x, I64 y -> Some (Int64.unsigned_compare x y < 0)
    | I_LE_S, TI64, I64 x, I64 y -> Some (Int64.compare x y <= 0)
    | I_LE_U, TI64, I64 x, I64 y -> Some (Int64.unsigned_compare x y <= 0)
    | I_GT_S, TI64, I64 x, I64 y -> Some (Int64.compare x y > 0)
    | I_GT_U, TI64, I64 x, I64 y -> Some (Int64.unsigned_compare x y > 0)
    | I_GE_S, TI64, I64 x, I64 y -> Some (Int64.compare x y >= 0)
    | I_GE_U, TI64, I64 x, I64 y -> Some (Int64.unsigned_compare x y >= 0)
    (* F64 *)
    | F_EQ, TF64, F64 x, F64 y -> Some (Float64.equal x y)
    | F_NE, TF64, F64 x, F64 y -> Some (not (Float64.equal x y))
    | F_LT, TF64, F64 x, F64 y -> Some (Float64.compare x y < 0)
    | F_LE, TF64, F64 x, F64 y -> Some (Float64.compare x y <= 0)
    | F_GT, TF64, F64 x, F64 y -> Some (Float64.compare x y > 0)
    | F_GE, TF64, F64 x, F64 y -> Some (Float64.compare x y >= 0)
    (* F32 *)
    | F_EQ, TF32, F32 x, F32 y -> Some (Float32.eq x y)
    | F_NE, TF32, F32 x, F32 y -> Some (Float32.ne x y)
    | F_LT, TF32, F32 x, F32 y -> Some (Float32.lt x y)
    | F_LE, TF32, F32 x, F32 y -> Some (Float32.le x y)
    | F_GT, TF32, F32 x, F32 y -> Some (Float32.gt x y)
    | F_GE, TF32, F32 x, F32 y -> Some (Float32.ge x y)
    | _ -> None

let cvtop = function
    (* FIXME *)
    | TI32, CVT_WRAP, TI64, I64 x ->
        Some (I32 (Int64.rem x 0x1_0000_0000L |> Int64.to_int32))
    | TI64, CVT_EXTEND_S, TI32, I32 x -> Some (I64 (Int64.of_int32 x))
    | TI64, CVT_EXTEND_U, TI32, I32 _x -> None
    | TI32, CVT_TRUNC_S, TF32, F32 _x -> None
    | TI32, CVT_TRUNC_S, TF64, F64 _x -> None
    | TI64, CVT_TRUNC_S, TF32, F32 _x -> None
    | TI64, CVT_TRUNC_S, TF64, F64 _x -> None
    | TI32, CVT_TRUNC_U, TF32, F32 _x -> None
    | TI32, CVT_TRUNC_U, TF64, F64 _x -> None
    | TI64, CVT_TRUNC_U, TF32, F32 _x -> None
    | TI64, CVT_TRUNC_U, TF64, F64 _x -> None
    | TF32, CVT_CONVERT_S, TI32, I32 x ->
        Some (F32 (Float32.of_float (Int32.to_float x)))
    | TF32, CVT_CONVERT_S, TI64, I64 x ->
        Some (F32 (Float32.of_float (Int64.to_float x)))
    | TF64, CVT_CONVERT_S, TI32, I32 x -> Some (F64 (Int32.to_float x))
    | TF64, CVT_CONVERT_S, TI64, I64 x -> Some (F64 (Int64.to_float x))
    | TF32, CVT_CONVERT_U, TI32, I32 _x -> None
    | TF32, CVT_CONVERT_U, TI64, I64 _x -> None
    | TF64, CVT_CONVERT_U, TI32, I32 x -> Some (F64 (Int32.to_float x))
    | TF64, CVT_CONVERT_U, TI64, I64 x -> Some (F64 (Int64.to_float x))
    | TF32, CVT_DEMOTE, TF64, F64 _x -> None
    | TF64, CVT_PROMOTE, TF32, F32 x -> Some (F64 (Float32.to_float x))
    | TI32, CVT_REINTERPRET, TF32, F32 x ->
        Some (I32 (Int32.bits_of_float (Float32.to_float x)))
    | TI32, CVT_REINTERPRET, TF64, F64 x -> Some (I32 (Int32.bits_of_float x))
    | TI64, CVT_REINTERPRET, TF32, F32 x ->
        Some (I64 (Int64.bits_of_float (Float32.to_float x)))
    | TI64, CVT_REINTERPRET, TF64, F64 x -> Some (I64 (Int64.bits_of_float x))
    | TF32, CVT_REINTERPRET, TI32, I32 x ->
        Some (F32 (Int32.bits_of_float (Float32.to_float x)))
    | TF32, CVT_REINTERPRET, TI64, I64 x ->
        Some (F32 (Float32.of_float (Int64.float_of_bits x)))
    | TF64, CVT_REINTERPRET, TI32, I32 x -> Some (F64 (Int32.float_of_bits x))
    | TF64, CVT_REINTERPRET, TI64, I64 x -> Some (F64 (Int64.float_of_bits x))
    | _ -> None
