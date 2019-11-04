(* include Stdlib.Int64 *)

type t = Int64.t

let equal = Int64.equal

module UnOp = struct
  let clz i =
      if Int64.equal i 0L
      then 64L
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
        Int64.of_int !cnt


  let ctz i =
      if Int64.equal i 0L
      then 64L
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
        Int64.of_int !cnt


  let popcont i =
      let j = ref i in
      let cnt = ref 0 in
      for _ = 0 to 31 do
        let r = Int64.logand 1L !j in
        if Int64.equal r 1L then incr cnt ;
        j := Int64.shift_right_logical !j 1
      done ;
      Int64.of_int !cnt
end

module BinOp = struct
  let add = Int64.add

  let sub = Int64.sub

  let mul = Int64.mul

  let div_s = Int64.div

  let div_u = Int64.unsigned_div

  let rem_s = Int64.rem

  let rem_u = Int64.unsigned_rem

  let logand = Int64.logand

  let logor = Int64.logor

  let logxor = Int64.logxor

  let shl x y =
      let k = Int64.to_int y in
      Int64.shift_left x k


  let shr_s x y =
      let k = Int64.to_int y in
      Int64.shift_right_logical x k


  let shr_u x y =
      let k = Int64.to_int y in
      Int64.shift_right x k


  let rotl x y =
      let k = Int64.to_int y in
      let l = Int64.shift_left x k in
      let r = Int64.shift_right x (64 - k) in
      Int64.add l r


  let rotr x y =
      let k = Int64.to_int y in
      let l = Int64.shift_left x (64 - k) in
      let r = Int64.shift_right x k in
      Int64.add l r
end

module RelOp = struct
  let eq = Int64.equal

  let ne x y = not (Int64.equal x y)

  let lt_s x y = Int64.compare x y < 0

  let lt_u x y = Int64.unsigned_compare x y < 0

  let le_s x y = Int64.compare x y <= 0

  let le_u x y = Int64.unsigned_compare x y <= 0

  let gt_s x y = Int64.compare x y > 0

  let gt_u x y = Int64.unsigned_compare x y > 0

  let ge_s x y = Int64.compare x y >= 0

  let ge_u x y = Int64.unsigned_compare x y >= 0
end

module CvtOp = struct
  let extend_s_i32 _x = failwith ""

  let extend_u_i32 _x = failwith ""

  let trunc_s_f32 _x = failwith ""

  let trunc_s_f64 _x = failwith ""

  let trunc_u_f32 _x = failwith ""

  let trunc_u_f64 _x = failwith ""

  let reinterpret_f32 _x = failwith ""

  let reinterpret_f64 _x = failwith ""
end
