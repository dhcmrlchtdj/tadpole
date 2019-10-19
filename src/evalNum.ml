open! Containers
module R = Run
open Structure

let unop = function
    (* I32 *)
    | I32, I_CLZ, R.I32 i ->
        if Int32.equal i 0l
        then Some (R.I32 32l)
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
          Some (R.I32 (Int32.of_int !cnt))
    | I32, I_CTZ, R.I32 i ->
        if Int32.equal i 0l
        then Some (R.I32 32l)
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
          Some (R.I32 (Int32.of_int !cnt))
    | I32, I_POPCONT, R.I32 i ->
        let j = ref i in
        let cnt = ref 0 in
        for _ = 0 to 31 do
          let r = Int32.logand 1l !j in
          if Int32.equal r 1l then incr cnt ;
          j := Int32.shift_right_logical !j 1
        done ;
        Some (R.I32 (Int32.of_int !cnt))
    (* I64 *)
    | I64, I_CLZ, R.I64 i ->
        if Int64.equal i 0L
        then Some (R.I64 64L)
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
          Some (R.I64 (Int64.of_int !cnt))
    | I64, I_CTZ, R.I64 i ->
        if Int64.equal i 0L
        then Some (R.I64 64L)
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
          Some (R.I64 (Int64.of_int !cnt))
    | I64, I_POPCONT, R.I64 i ->
        let j = ref i in
        let cnt = ref 0 in
        for _ = 0 to 31 do
          let r = Int64.logand 1L !j in
          if Int64.equal r 1L then incr cnt ;
          j := Int64.shift_right_logical !j 1
        done ;
        Some (R.I64 (Int64.of_int !cnt))
    (* F64 *)
    | F64, F_ABS, R.F64 i -> Some (R.F64 (Float64.abs i))
    | F64, F_NEG, R.F64 i -> Some (R.F64 (Float64.neg i))
    | F64, F_SQRT, R.F64 i -> Some (R.F64 (Float64.sqrt i))
    | F64, F_CEIL, R.F64 i -> Some (R.F64 (Float64.ceil i))
    | F64, F_FLOOR, R.F64 i -> Some (R.F64 (Float64.floor i))
    | F64, F_TRUNC, R.F64 i -> Some (R.F64 (Float64.trunc i))
    | F64, F_NEAREST, R.F64 i -> Some (R.F64 (Float64.round i))
    (* F32 *)
    | F32, F_ABS, R.F32 i -> Some (R.F32 (Float32.abs i))
    | F32, F_NEG, R.F32 i -> Some (R.F32 (Float32.neg i))
    | F32, F_SQRT, R.F32 i -> Some (R.F32 (Float32.sqrt i))
    | F32, F_CEIL, R.F32 i -> Some (R.F32 (Float32.ceil i))
    | F32, F_FLOOR, R.F32 i -> Some (R.F32 (Float32.floor i))
    | F32, F_TRUNC, R.F32 i -> Some (R.F32 (Float32.trunc i))
    | F32, F_NEAREST, R.F32 i -> Some (R.F32 (Float32.nearest i))
    | _ -> None


let binop = function
    (* I32 *)
    | I32, I_ADD, R.I32 x, R.I32 y -> Some (R.I32 (Int32.add x y))
    | I32, I_SUB, R.I32 x, R.I32 y -> Some (R.I32 (Int32.sub x y))
    | I32, I_MUL, R.I32 x, R.I32 y -> Some (R.I32 (Int32.mul x y))
    | I32, I_DIV_S, R.I32 x, R.I32 y -> Some (R.I32 (Int32.div x y))
    | I32, I_DIV_U, R.I32 x, R.I32 y -> Some (R.I32 (Int32.unsigned_div x y))
    | I32, I_REM_S, R.I32 x, R.I32 y -> Some (R.I32 (Int32.rem x y))
    | I32, I_REM_U, R.I32 x, R.I32 y -> Some (R.I32 (Int32.unsigned_rem x y))
    | I32, I_AND, R.I32 x, R.I32 y -> Some (R.I32 (Int32.logand x y))
    | I32, I_OR, R.I32 x, R.I32 y -> Some (R.I32 (Int32.logor x y))
    | I32, I_XOR, R.I32 x, R.I32 y -> Some (R.I32 (Int32.logxor x y))
    | I32, I_SHL, R.I32 x, R.I32 y ->
        let k32 = Int32.rem y 32l in
        let k = Int32.to_int k32 in
        Some (R.I32 (Int32.shift_left x k))
    | I32, I_SHR_S, R.I32 x, R.I32 y ->
        let k32 = Int32.rem y 32l in
        let k = Int32.to_int k32 in
        Some (R.I32 (Int32.shift_right_logical x k))
    | I32, I_SHR_U, R.I32 x, R.I32 y ->
        let k32 = Int32.rem y 32l in
        let k = Int32.to_int k32 in
        Some (R.I32 (Int32.shift_right x k))
    | I32, I_ROTL, R.I32 x, R.I32 y ->
        let k32 = Int32.rem y 32l in
        let k = Int32.to_int k32 in
        let l = Int32.shift_left x k in
        let r = Int32.shift_right x (32 - k) in
        Some (R.I32 (Int32.add l r))
    | I32, I_ROTR, R.I32 x, R.I32 y ->
        let k32 = Int32.rem y 32l in
        let k = Int32.to_int k32 in
        let l = Int32.shift_left x (32 - k) in
        let r = Int32.shift_right x k in
        Some (R.I32 (Int32.add l r))
    (* I64 *)
    | I64, I_ADD, R.I64 x, R.I64 y -> Some (R.I64 (Int64.add x y))
    | I64, I_SUB, R.I64 x, R.I64 y -> Some (R.I64 (Int64.sub x y))
    | I64, I_MUL, R.I64 x, R.I64 y -> Some (R.I64 (Int64.mul x y))
    | I64, I_DIV_S, R.I64 x, R.I64 y -> Some (R.I64 (Int64.div x y))
    | I64, I_DIV_U, R.I64 x, R.I64 y -> Some (R.I64 (Int64.unsigned_div x y))
    | I64, I_REM_S, R.I64 x, R.I64 y -> Some (R.I64 (Int64.rem x y))
    | I64, I_REM_U, R.I64 x, R.I64 y -> Some (R.I64 (Int64.unsigned_rem x y))
    | I64, I_AND, R.I64 x, R.I64 y -> Some (R.I64 (Int64.logand x y))
    | I64, I_OR, R.I64 x, R.I64 y -> Some (R.I64 (Int64.logor x y))
    | I64, I_XOR, R.I64 x, R.I64 y -> Some (R.I64 (Int64.logxor x y))
    | I64, I_SHL, R.I64 x, R.I64 y ->
        let k64 = Int64.rem y 64L in
        let k = Int64.to_int k64 in
        Some (R.I64 (Int64.shift_left x k))
    | I64, I_SHR_S, R.I64 x, R.I64 y ->
        let k64 = Int64.rem y 64L in
        let k = Int64.to_int k64 in
        Some (R.I64 (Int64.shift_right_logical x k))
    | I64, I_SHR_U, R.I64 x, R.I64 y ->
        let k64 = Int64.rem y 64L in
        let k = Int64.to_int k64 in
        Some (R.I64 (Int64.shift_right x k))
    | I64, I_ROTL, R.I64 x, R.I64 y ->
        let k64 = Int64.rem y 64L in
        let k = Int64.to_int k64 in
        let l = Int64.shift_left x k in
        let r = Int64.shift_right x (64 - k) in
        Some (R.I64 (Int64.add l r))
    | I64, I_ROTR, R.I64 x, R.I64 y ->
        let k64 = Int64.rem y 64L in
        let k = Int64.to_int k64 in
        let l = Int64.shift_left x (64 - k) in
        let r = Int64.shift_right x k in
        Some (R.I64 (Int64.add l r))
    (* F64 *)
    | F64, F_ADD, R.F64 x, R.F64 y -> Some (R.F64 (Float64.add x y))
    | F64, F_SUB, R.F64 x, R.F64 y -> Some (R.F64 (Float64.sub x y))
    | F64, F_MUL, R.F64 x, R.F64 y -> Some (R.F64 (Float64.mul x y))
    | F64, F_DIV, R.F64 x, R.F64 y -> Some (R.F64 (Float64.div x y))
    | F64, F_MIN, R.F64 x, R.F64 y -> Some (R.F64 (Float64.min x y))
    | F64, F_MAX, R.F64 x, R.F64 y -> Some (R.F64 (Float64.max x y))
    | F64, F_COPYSIGN, R.F64 x, R.F64 y -> Some (R.F64 (Float64.copy_sign x y))
    (* F32 *)
    | F32, F_ADD, R.F32 x, R.F32 y -> Some (R.F32 (Float32.add x y))
    | F32, F_SUB, R.F32 x, R.F32 y -> Some (R.F32 (Float32.sub x y))
    | F32, F_MUL, R.F32 x, R.F32 y -> Some (R.F32 (Float32.mul x y))
    | F32, F_DIV, R.F32 x, R.F32 y -> Some (R.F32 (Float32.div x y))
    | F32, F_MIN, R.F32 x, R.F32 y -> Some (R.F32 (Float32.min x y))
    | F32, F_MAX, R.F32 x, R.F32 y -> Some (R.F32 (Float32.max x y))
    | F32, F_COPYSIGN, R.F32 x, R.F32 y -> Some (R.F32 (Float32.copy_sign x y))
    | _ -> None


let testop = function
    | I32, I_EQZ, R.I32 i -> Some (Int32.equal i 0l)
    | I64, I_EQZ, R.I64 i -> Some (Int64.equal i 0L)
    | _ -> None


let relop = function
    (* I32 *)
    | I32, I_EQ, R.I32 x, R.I32 y -> Some (Int32.equal x y)
    | I32, I_NE, R.I32 x, R.I32 y -> Some (not (Int32.equal x y))
    | I32, I_LT_S, R.I32 x, R.I32 y -> Some (Int32.compare x y < 0)
    | I32, I_LT_U, R.I32 x, R.I32 y -> Some (Int32.unsigned_compare x y < 0)
    | I32, I_LE_S, R.I32 x, R.I32 y -> Some (Int32.compare x y <= 0)
    | I32, I_LE_U, R.I32 x, R.I32 y -> Some (Int32.unsigned_compare x y <= 0)
    | I32, I_GT_S, R.I32 x, R.I32 y -> Some (Int32.compare x y > 0)
    | I32, I_GT_U, R.I32 x, R.I32 y -> Some (Int32.unsigned_compare x y > 0)
    | I32, I_GE_S, R.I32 x, R.I32 y -> Some (Int32.compare x y >= 0)
    | I32, I_GE_U, R.I32 x, R.I32 y -> Some (Int32.unsigned_compare x y >= 0)
    (* I64 *)
    | I64, I_EQ, R.I64 x, R.I64 y -> Some (Int64.equal x y)
    | I64, I_NE, R.I64 x, R.I64 y -> Some (not (Int64.equal x y))
    | I64, I_LT_S, R.I64 x, R.I64 y -> Some (Int64.compare x y < 0)
    | I64, I_LT_U, R.I64 x, R.I64 y -> Some (Int64.unsigned_compare x y < 0)
    | I64, I_LE_S, R.I64 x, R.I64 y -> Some (Int64.compare x y <= 0)
    | I64, I_LE_U, R.I64 x, R.I64 y -> Some (Int64.unsigned_compare x y <= 0)
    | I64, I_GT_S, R.I64 x, R.I64 y -> Some (Int64.compare x y > 0)
    | I64, I_GT_U, R.I64 x, R.I64 y -> Some (Int64.unsigned_compare x y > 0)
    | I64, I_GE_S, R.I64 x, R.I64 y -> Some (Int64.compare x y >= 0)
    | I64, I_GE_U, R.I64 x, R.I64 y -> Some (Int64.unsigned_compare x y >= 0)
    (* F64 *)
    | F64, F_EQ, R.F64 x, R.F64 y -> Some (Float64.equal x y)
    | F64, F_NE, R.F64 x, R.F64 y -> Some (not (Float64.equal x y))
    | F64, F_LT, R.F64 x, R.F64 y -> Some (Float64.compare x y < 0)
    | F64, F_LE, R.F64 x, R.F64 y -> Some (Float64.compare x y <= 0)
    | F64, F_GT, R.F64 x, R.F64 y -> Some (Float64.compare x y > 0)
    | F64, F_GE, R.F64 x, R.F64 y -> Some (Float64.compare x y >= 0)
    (* F32 *)
    | F32, F_EQ, R.F32 x, R.F32 y -> Some (Float32.eq x y)
    | F32, F_NE, R.F32 x, R.F32 y -> Some (Float32.ne x y)
    | F32, F_LT, R.F32 x, R.F32 y -> Some (Float32.lt x y)
    | F32, F_LE, R.F32 x, R.F32 y -> Some (Float32.le x y)
    | F32, F_GT, R.F32 x, R.F32 y -> Some (Float32.gt x y)
    | F32, F_GE, R.F32 x, R.F32 y -> Some (Float32.ge x y)
    | _ -> None


let cvtop = function
    (* FIXME *)
    | I32, CVT_WRAP, I64, R.I64 x ->
        Some (R.I32 (Int64.rem x 0x1_0000_0000L |> Int64.to_int32))
    | I64, CVT_EXTEND_S, I32, R.I32 x -> Some (R.I64 (Int64.of_int32 x))
    | I64, CVT_EXTEND_U, I32, R.I32 _x -> None
    | I32, CVT_TRUNC_S, F32, R.F32 _x -> None
    | I32, CVT_TRUNC_S, F64, R.F64 _x -> None
    | I64, CVT_TRUNC_S, F32, R.F32 _x -> None
    | I64, CVT_TRUNC_S, F64, R.F64 _x -> None
    | I32, CVT_TRUNC_U, F32, R.F32 _x -> None
    | I32, CVT_TRUNC_U, F64, R.F64 _x -> None
    | I64, CVT_TRUNC_U, F32, R.F32 _x -> None
    | I64, CVT_TRUNC_U, F64, R.F64 _x -> None
    | F32, CVT_CONVERT_S, I32, R.I32 x ->
        Some (R.F32 (Float32.of_float (Int32.to_float x)))
    | F32, CVT_CONVERT_S, I64, R.I64 x ->
        Some (R.F32 (Float32.of_float (Int64.to_float x)))
    | F64, CVT_CONVERT_S, I32, R.I32 x -> Some (R.F64 (Int32.to_float x))
    | F64, CVT_CONVERT_S, I64, R.I64 x -> Some (R.F64 (Int64.to_float x))
    | F32, CVT_CONVERT_U, I32, R.I32 _x -> None
    | F32, CVT_CONVERT_U, I64, R.I64 _x -> None
    | F64, CVT_CONVERT_U, I32, R.I32 x -> Some (R.F64 (Int32.to_float x))
    | F64, CVT_CONVERT_U, I64, R.I64 x -> Some (R.F64 (Int64.to_float x))
    | F32, CVT_DEMOTE, F64, R.F64 _x -> None
    | F64, CVT_PROMOTE, F32, R.F32 x -> Some (R.F64 (Float32.to_float x))
    | I32, CVT_REINTERPRET, F32, R.F32 x ->
        Some (R.I32 (Int32.bits_of_float (Float32.to_float x)))
    | I32, CVT_REINTERPRET, F64, R.F64 x ->
        Some (R.I32 (Int32.bits_of_float x))
    | I64, CVT_REINTERPRET, F32, R.F32 x ->
        Some (R.I64 (Int64.bits_of_float (Float32.to_float x)))
    | I64, CVT_REINTERPRET, F64, R.F64 x ->
        Some (R.I64 (Int64.bits_of_float x))
    | F32, CVT_REINTERPRET, I32, R.I32 x ->
        Some (R.F32 (Int32.bits_of_float (Float32.to_float x)))
    | F32, CVT_REINTERPRET, I64, R.I64 x ->
        Some (R.F32 (Float32.of_float (Int64.float_of_bits x)))
    | F64, CVT_REINTERPRET, I32, R.I32 x ->
        Some (R.F64 (Int32.float_of_bits x))
    | F64, CVT_REINTERPRET, I64, R.I64 x ->
        Some (R.F64 (Int64.float_of_bits x))
    | _ -> None
