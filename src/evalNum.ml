open! Containers
open Types

let unop = function
    (* TI32 *)
    | (TI32, I_CLZ, I32 i) ->
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
    | (TI32, I_CTZ, I32 i) ->
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
    | (TI32, I_POPCONT, I32 i) ->
        let j = ref i in
        let cnt = ref 0 in
        for _ = 0 to 31 do
          let r = Int32.logand 1l !j in
          if Int32.equal r 1l then incr cnt ;
          j := Int32.shift_right_logical !j 1
        done ;
        Some (I32 (Int32.of_int !cnt))
    (* TI64 *)
    | (TI64, I_CLZ, I64 i) ->
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
    | (TI64, I_CTZ, I64 i) ->
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
    | (TI64, I_POPCONT, I64 i) ->
        let j = ref i in
        let cnt = ref 0 in
        for _ = 0 to 31 do
          let r = Int64.logand 1L !j in
          if Int64.equal r 1L then incr cnt ;
          j := Int64.shift_right_logical !j 1
        done ;
        Some (I64 (Int64.of_int !cnt))
    (* TF64 *)
    | (TF64, F_ABS, F64 i) -> Some (F64 (Float64.abs i))
    | (TF64, F_NEG, F64 i) -> Some (F64 (Float64.neg i))
    | (TF64, F_SQRT, F64 i) -> Some (F64 (Float64.sqrt i))
    | (TF64, F_CEIL, F64 i) -> Some (F64 (Float64.ceil i))
    | (TF64, F_FLOOR, F64 i) -> Some (F64 (Float64.floor i))
    | (TF64, F_TRUNC, F64 i) -> Some (F64 (Float64.trunc i))
    | (TF64, F_NEAREST, F64 i) -> Some (F64 (Float64.round i))
    (* TF32 *)
    | (TF32, F_ABS, F32 i) -> Some (F32 (Float32.abs i))
    | (TF32, F_NEG, F32 i) -> Some (F32 (Float32.neg i))
    | (TF32, F_SQRT, F32 i) -> Some (F32 (Float32.sqrt i))
    | (TF32, F_CEIL, F32 i) -> Some (F32 (Float32.ceil i))
    | (TF32, F_FLOOR, F32 i) -> Some (F32 (Float32.floor i))
    | (TF32, F_TRUNC, F32 i) -> Some (F32 (Float32.trunc i))
    | (TF32, F_NEAREST, F32 i) -> Some (F32 (Float32.nearest i))
    | _ -> None


let binop = function
    (* TI32 *)
    | (TI32, I_ADD, I32 x, I32 y) -> Some (I32 (Int32.add x y))
    | (TI32, I_SUB, I32 x, I32 y) -> Some (I32 (Int32.sub x y))
    | (TI32, I_MUL, I32 x, I32 y) -> Some (I32 (Int32.mul x y))
    | (TI32, I_DIV_S, I32 x, I32 y) -> Some (I32 (Int32.div x y))
    | (TI32, I_DIV_U, I32 x, I32 y) -> Some (I32 (Int32.unsigned_div x y))
    | (TI32, I_REM_S, I32 x, I32 y) -> Some (I32 (Int32.rem x y))
    | (TI32, I_REM_U, I32 x, I32 y) -> Some (I32 (Int32.unsigned_rem x y))
    | (TI32, I_AND, I32 x, I32 y) -> Some (I32 (Int32.logand x y))
    | (TI32, I_OR, I32 x, I32 y) -> Some (I32 (Int32.logor x y))
    | (TI32, I_XOR, I32 x, I32 y) -> Some (I32 (Int32.logxor x y))
    | (TI32, I_SHL, I32 x, I32 y) ->
        let k = Int32.to_int y in
        Some (I32 (Int32.shift_left x k))
    | (TI32, I_SHR_S, I32 x, I32 y) ->
        let k = Int32.to_int y in
        Some (I32 (Int32.shift_right_logical x k))
    | (TI32, I_SHR_U, I32 x, I32 y) ->
        let k = Int32.to_int y in
        Some (I32 (Int32.shift_right x k))
    | (TI32, I_ROTL, I32 x, I32 y) ->
        let k = Int32.to_int y in
        let l = Int32.shift_left x k in
        let r = Int32.shift_right x (32 - k) in
        Some (I32 (Int32.add l r))
    | (TI32, I_ROTR, I32 x, I32 y) ->
        let k = Int32.to_int y in
        let l = Int32.shift_left x (32 - k) in
        let r = Int32.shift_right x k in
        Some (I32 (Int32.add l r))
    (* TI64 *)
    | (TI64, I_ADD, I64 x, I64 y) -> Some (I64 (Int64.add x y))
    | (TI64, I_SUB, I64 x, I64 y) -> Some (I64 (Int64.sub x y))
    | (TI64, I_MUL, I64 x, I64 y) -> Some (I64 (Int64.mul x y))
    | (TI64, I_DIV_S, I64 x, I64 y) -> Some (I64 (Int64.div x y))
    | (TI64, I_DIV_U, I64 x, I64 y) -> Some (I64 (Int64.unsigned_div x y))
    | (TI64, I_REM_S, I64 x, I64 y) -> Some (I64 (Int64.rem x y))
    | (TI64, I_REM_U, I64 x, I64 y) -> Some (I64 (Int64.unsigned_rem x y))
    | (TI64, I_AND, I64 x, I64 y) -> Some (I64 (Int64.logand x y))
    | (TI64, I_OR, I64 x, I64 y) -> Some (I64 (Int64.logor x y))
    | (TI64, I_XOR, I64 x, I64 y) -> Some (I64 (Int64.logxor x y))
    | (TI64, I_SHL, I64 x, I64 y) ->
        let k = Int64.to_int y in
        Some (I64 (Int64.shift_left x k))
    | (TI64, I_SHR_S, I64 x, I64 y) ->
        let k = Int64.to_int y in
        Some (I64 (Int64.shift_right_logical x k))
    | (TI64, I_SHR_U, I64 x, I64 y) ->
        let k = Int64.to_int y in
        Some (I64 (Int64.shift_right x k))
    | (TI64, I_ROTL, I64 x, I64 y) ->
        let k = Int64.to_int y in
        let l = Int64.shift_left x k in
        let r = Int64.shift_right x (64 - k) in
        Some (I64 (Int64.add l r))
    | (TI64, I_ROTR, I64 x, I64 y) ->
        let k = Int64.to_int y in
        let l = Int64.shift_left x (64 - k) in
        let r = Int64.shift_right x k in
        Some (I64 (Int64.add l r))
    (* TF64 *)
    | (TF64, F_ADD, F64 x, F64 y) -> Some (F64 (Float64.add x y))
    | (TF64, F_SUB, F64 x, F64 y) -> Some (F64 (Float64.sub x y))
    | (TF64, F_MUL, F64 x, F64 y) -> Some (F64 (Float64.mul x y))
    | (TF64, F_DIV, F64 x, F64 y) -> Some (F64 (Float64.div x y))
    | (TF64, F_MIN, F64 x, F64 y) -> Some (F64 (Float64.min x y))
    | (TF64, F_MAX, F64 x, F64 y) -> Some (F64 (Float64.max x y))
    | (TF64, F_COPYSIGN, F64 x, F64 y) -> Some (F64 (Float64.copy_sign x y))
    (* TF32 *)
    | (TF32, F_ADD, F32 x, F32 y) -> Some (F32 (Float32.add x y))
    | (TF32, F_SUB, F32 x, F32 y) -> Some (F32 (Float32.sub x y))
    | (TF32, F_MUL, F32 x, F32 y) -> Some (F32 (Float32.mul x y))
    | (TF32, F_DIV, F32 x, F32 y) -> Some (F32 (Float32.div x y))
    | (TF32, F_MIN, F32 x, F32 y) -> Some (F32 (Float32.min x y))
    | (TF32, F_MAX, F32 x, F32 y) -> Some (F32 (Float32.max x y))
    | (TF32, F_COPYSIGN, F32 x, F32 y) -> Some (F32 (Float32.copy_sign x y))
    | _ -> None


let testop = function
    | (TI32, I_EQZ, I32 i) -> Some (Int32.equal i 0l)
    | (TI64, I_EQZ, I64 i) -> Some (Int64.equal i 0L)
    | _ -> None


let relop = function
    (* TI32 *)
    | (TI32, I_EQ, I32 x, I32 y) -> Some (Int32.equal x y)
    | (TI32, I_NE, I32 x, I32 y) -> Some (not (Int32.equal x y))
    | (TI32, I_LT_S, I32 x, I32 y) -> Some (Int32.compare x y < 0)
    | (TI32, I_LT_U, I32 x, I32 y) -> Some (Int32.unsigned_compare x y < 0)
    | (TI32, I_LE_S, I32 x, I32 y) -> Some (Int32.compare x y <= 0)
    | (TI32, I_LE_U, I32 x, I32 y) -> Some (Int32.unsigned_compare x y <= 0)
    | (TI32, I_GT_S, I32 x, I32 y) -> Some (Int32.compare x y > 0)
    | (TI32, I_GT_U, I32 x, I32 y) -> Some (Int32.unsigned_compare x y > 0)
    | (TI32, I_GE_S, I32 x, I32 y) -> Some (Int32.compare x y >= 0)
    | (TI32, I_GE_U, I32 x, I32 y) -> Some (Int32.unsigned_compare x y >= 0)
    (* TI64 *)
    | (TI64, I_EQ, I64 x, I64 y) -> Some (Int64.equal x y)
    | (TI64, I_NE, I64 x, I64 y) -> Some (not (Int64.equal x y))
    | (TI64, I_LT_S, I64 x, I64 y) -> Some (Int64.compare x y < 0)
    | (TI64, I_LT_U, I64 x, I64 y) -> Some (Int64.unsigned_compare x y < 0)
    | (TI64, I_LE_S, I64 x, I64 y) -> Some (Int64.compare x y <= 0)
    | (TI64, I_LE_U, I64 x, I64 y) -> Some (Int64.unsigned_compare x y <= 0)
    | (TI64, I_GT_S, I64 x, I64 y) -> Some (Int64.compare x y > 0)
    | (TI64, I_GT_U, I64 x, I64 y) -> Some (Int64.unsigned_compare x y > 0)
    | (TI64, I_GE_S, I64 x, I64 y) -> Some (Int64.compare x y >= 0)
    | (TI64, I_GE_U, I64 x, I64 y) -> Some (Int64.unsigned_compare x y >= 0)
    (* TF64 *)
    | (TF64, F_EQ, F64 x, F64 y) -> Some (Float64.equal x y)
    | (TF64, F_NE, F64 x, F64 y) -> Some (not (Float64.equal x y))
    | (TF64, F_LT, F64 x, F64 y) -> Some (Float64.compare x y < 0)
    | (TF64, F_LE, F64 x, F64 y) -> Some (Float64.compare x y <= 0)
    | (TF64, F_GT, F64 x, F64 y) -> Some (Float64.compare x y > 0)
    | (TF64, F_GE, F64 x, F64 y) -> Some (Float64.compare x y >= 0)
    (* TF32 *)
    | (TF32, F_EQ, F32 x, F32 y) -> Some (Float32.eq x y)
    | (TF32, F_NE, F32 x, F32 y) -> Some (Float32.ne x y)
    | (TF32, F_LT, F32 x, F32 y) -> Some (Float32.lt x y)
    | (TF32, F_LE, F32 x, F32 y) -> Some (Float32.le x y)
    | (TF32, F_GT, F32 x, F32 y) -> Some (Float32.gt x y)
    | (TF32, F_GE, F32 x, F32 y) -> Some (Float32.ge x y)
    | _ -> None


let cvtop = function
    | (TI32, CVT_WRAP, TI64, I64 x) ->
        Some (I32 (Int64.rem x 0x1_0000_0000L |> Int64.to_int32))
    | (TI64, CVT_EXTEND_S, TI32, I32 x) -> Some (I64 (Int64.of_int32 x))
    | (TI64, CVT_EXTEND_U, TI32, I32 x) ->
        if Int32.compare x 0l >= 0
        then Some (I64 (Int64.of_int32 x))
        else
          let xx = Int32.logand 0x7FFF_FFFFl x in
          let i64 = Int64.of_int32 xx in
          Some (I64 (Int64.neg i64))
    | (TI32, CVT_TRUNC_S, TF32, F32 _x) -> failwith "TODO"
    | (TI32, CVT_TRUNC_S, TF64, F64 _x) -> failwith "TODO"
    | (TI64, CVT_TRUNC_S, TF32, F32 _x) -> failwith "TODO"
    | (TI64, CVT_TRUNC_S, TF64, F64 _x) -> failwith "TODO"
    | (TI32, CVT_TRUNC_U, TF32, F32 _x) -> failwith "TODO"
    | (TI32, CVT_TRUNC_U, TF64, F64 _x) -> failwith "TODO"
    | (TI64, CVT_TRUNC_U, TF32, F32 _x) -> failwith "TODO"
    | (TI64, CVT_TRUNC_U, TF64, F64 _x) -> failwith "TODO"
    | (TF32, CVT_CONVERT_S, TI32, I32 x) ->
        Some (F32 (Float32.of_float (Int32.to_float x)))
    | (TF32, CVT_CONVERT_S, TI64, I64 x) ->
        Some (F32 (Float32.of_float (Int64.to_float x)))
    | (TF64, CVT_CONVERT_S, TI32, I32 x) -> Some (F64 (Int32.to_float x))
    | (TF64, CVT_CONVERT_S, TI64, I64 x) -> Some (F64 (Int64.to_float x))
    | (TF32, CVT_CONVERT_U, TI32, I32 _x) -> failwith "TODO"
    | (TF32, CVT_CONVERT_U, TI64, I64 _x) -> failwith "TODO"
    | (TF64, CVT_CONVERT_U, TI32, I32 _x) -> failwith "TODO"
    | (TF64, CVT_CONVERT_U, TI64, I64 _x) -> failwith "TODO"
    | (TF32, CVT_DEMOTE, TF64, F64 x) -> Some (F32 (Float32.of_float x))
    | (TF64, CVT_PROMOTE, TF32, F32 x) -> Some (F64 (Float32.to_float x))
    | (TI32, CVT_REINTERPRET, TF32, F32 x) ->
        Some (I32 (Float32.int32_of_bits x))
    | (TI32, CVT_REINTERPRET, TF64, F64 x) ->
        Some (I32 (Int32.bits_of_float x))
    | (TI64, CVT_REINTERPRET, TF32, F32 x) ->
        Some (I64 (Int64.bits_of_float (Float32.to_float x)))
    | (TI64, CVT_REINTERPRET, TF64, F64 x) ->
        Some (I64 (Int64.bits_of_float x))
    | (TF32, CVT_REINTERPRET, TI32, I32 x) ->
        Some (F32 (Float32.bits_of_int32 x))
    | (TF32, CVT_REINTERPRET, TI64, I64 x) ->
        Some (F32 (Float32.of_float (Int64.float_of_bits x)))
    | (TF64, CVT_REINTERPRET, TI32, I32 x) ->
        Some (F64 (Int32.float_of_bits x))
    | (TF64, CVT_REINTERPRET, TI64, I64 x) ->
        Some (F64 (Int64.float_of_bits x))
    | _ -> None
