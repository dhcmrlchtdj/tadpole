(* include Stdlib.Int64 *)

type t = Int64.t

let equal = Int64.equal

let to_int = Int64.to_int

let of_int = Int64.of_int

let to_int32 = Int64.to_int32

let of_int32 = Int64.of_int32

module UnOp = struct
  let clz i =
      let j = ref i in
      let cnt = ref 0 in
      let break = ref false in
      let loop = ref 1 in
      while Bool.equal !break false && !loop <= 64 do
        let r = Int64.logand 0x8000_0000_0000_0000L !j in
        if Int64.equal r 0L
        then (
          incr cnt ;
          incr loop ;
          j := Int64.shift_left !j 1 )
        else break := true
      done ;
      Int64.of_int !cnt


  let ctz i =
      let j = ref i in
      let cnt = ref 0 in
      let break = ref false in
      let loop = ref 1 in
      while Bool.equal !break false && !loop <= 64 do
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
      for _ = 1 to 64 do
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
      let k = Int64.to_int y mod 64 in
      Int64.shift_left x k


  let shr_s x y =
      let k = Int64.to_int y mod 64 in
      Int64.shift_right x k


  let shr_u x y =
      let k = Int64.to_int y mod 64 in
      Int64.shift_right_logical x k


  let rotl x y =
      let k = Int64.to_int y mod 64 in
      let l = Int64.shift_left x k in
      let r = Int64.shift_right_logical x (64 - k) in
      Int64.add l r


  let rotr x y =
      let k = Int64.to_int y mod 64 in
      let l = Int64.shift_left x (64 - k) in
      let r = Int64.shift_right_logical x k in
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
  (* https://github.com/WebAssembly/spec/blob/994591e51c9df9e7ef980b04d660709b79982f75/interpreter/exec/i64_convert.ml *)

  let extend_s_i32 x = Int64.of_int32 x

  let extend_u_i32 x =
      let xx = Int64.of_int32 x in
      Int64.logand xx 0x0000_0000_FFFF_FFFFL


  let trunc_s_f32 x =
      let xf = Int32.float_of_bits x in
      if Float.is_nan xf then failwith "integer nan" ;
      if xf >= -.Int64.(to_float min_int) || xf < Int64.(to_float min_int)
      then failwith "integer overflow"
      else Int64.of_float xf


  let trunc_u_f32 x =
      let xf = Int32.float_of_bits x in
      if Float.is_nan xf then failwith "integer nan" ;
      if xf >= -.Int64.(to_float min_int) *. 2.0 || xf <= -1.0
      then failwith "integer overflow"
      else if xf >= -.Int64.(to_float min_int)
      then Int64.(logxor (of_float (xf -. 9223372036854775808.0)) min_int)
      else Int64.of_float xf


  let trunc_s_f64 x =
      let xf = x in
      if Float.is_nan xf then failwith "integer nan" ;
      if xf >= -.Int64.(to_float min_int) || xf < Int64.(to_float min_int)
      then failwith "integer overflow"
      else Int64.of_float xf


  let trunc_u_f64 x =
      let xf = x in
      if Float.is_nan xf then failwith "integer nan" ;
      if xf >= -.Int64.(to_float min_int) *. 2.0 || xf <= -1.0
      then failwith "integer overflow"
      else if xf >= -.Int64.(to_float min_int)
      then Int64.(logxor (of_float (xf -. 9223372036854775808.0)) min_int)
      else Int64.of_float xf


  let reinterpret_f64 x = Int64.bits_of_float x
end
