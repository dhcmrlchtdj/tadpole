(* include Stdlib.Int32 *)

type t = Int32.t

let equal = Int32.equal

module UnOp = struct
  let clz i =
      if Int32.equal i 0l
      then 32l
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
        Int32.of_int !cnt


  let ctz i =
      if Int32.equal i 0l
      then 32l
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
        Int32.of_int !cnt


  let popcont i =
      let j = ref i in
      let cnt = ref 0 in
      for _ = 0 to 31 do
        let r = Int32.logand 1l !j in
        if Int32.equal r 1l then incr cnt ;
        j := Int32.shift_right_logical !j 1
      done ;
      Int32.of_int !cnt
end

module BinOp = struct
  let add = Int32.add

  let sub = Int32.sub

  let mul = Int32.mul

  let div_s = Int32.div

  let div_u = Int32.unsigned_div

  let rem_s = Int32.rem

  let rem_u = Int32.unsigned_rem

  let logand = Int32.logand

  let logor = Int32.logor

  let logxor = Int32.logxor

  let shl x y =
      let k = Int32.to_int y in
      Int32.shift_left x k


  let shr_s x y =
      let k = Int32.to_int y in
      Int32.shift_right_logical x k


  let shr_u x y =
      let k = Int32.to_int y in
      Int32.shift_right x k


  let rotl x y =
      let k = Int32.to_int y in
      let l = Int32.shift_left x k in
      let r = Int32.shift_right x (32 - k) in
      Int32.add l r


  let rotr x y =
      let k = Int32.to_int y in
      let l = Int32.shift_left x (32 - k) in
      let r = Int32.shift_right x k in
      Int32.add l r
end

module RelOp = struct
  let eq = Int32.equal

  let ne x y = not (Int32.equal x y)

  let lt_s x y = Int32.compare x y < 0

  let lt_u x y = Int32.unsigned_compare x y < 0

  let le_s x y = Int32.compare x y <= 0

  let le_u x y = Int32.unsigned_compare x y <= 0

  let gt_s x y = Int32.compare x y > 0

  let gt_u x y = Int32.unsigned_compare x y > 0

  let ge_s x y = Int32.compare x y >= 0

  let ge_u x y = Int32.unsigned_compare x y >= 0
end

module CvtOp = struct
  let wrap_i64 x = Int64.rem x 0x1_0000_0000L |> Int64.to_int32

  let trunc_s_f32 _x = failwith "TODO"

  let trunc_s_f64 _x = failwith "TODO"

  let trunc_u_f32 _x = failwith "TODO"

  let trunc_u_f64 _x = failwith "TODO"

  let reinterpret_f32 _x = failwith "TODO"

  let reinterpret_f64 _x = failwith "TODO"
end
