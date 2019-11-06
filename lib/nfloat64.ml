(* include Stdlib.Float *)

type t = Float.t

let equal = Float.equal

let to_bytes x =
    let x = Int64.bits_of_float x in
    let b = Bytes.create 8 in
    let () = Bytes.set_int64_be b 0 x in
    b


let to_bytes_le x =
    let x = Int64.bits_of_float x in
    let b = Bytes.create 8 in
    let () = Bytes.set_int64_le b 0 x in
    b


let of_bytes b = Bytes.get_int64_be b 0 |> Int64.float_of_bits

let to_string = Float.to_string

module UnOp = struct
  let abs = Float.abs

  let neg = Float.neg

  let sqrt = Float.sqrt

  let ceil = Float.ceil

  let floor = Float.floor

  let trunc = Float.trunc

  let nearest = Float.round
end

module BinOp = struct
  let add = Float.add

  let sub = Float.sub

  let mul = Float.mul

  let div = Float.div

  let min = Float.min

  let max = Float.min

  let copy_sign = Float.copy_sign
end

module RelOp = struct
  let eq = Float.equal

  let ne x y = not (Float.equal x y)

  let lt x y = Float.compare x y < 0

  let le x y = Float.compare x y <= 0

  let gt x y = Float.compare x y > 0

  let ge x y = Float.compare x y >= 0
end

module CvtOp = struct
  (* https://github.com/WebAssembly/spec/blob/994591e51c9df9e7ef980b04d660709b79982f75/interpreter/exec/f64_convert.ml *)

  let convert_s_i32 x = Int32.to_float x

  let convert_u_i32 x =
      let xx = Int64.logand (Int64.of_int32 x) 0x0000_0000_FFFF_FFFFL in
      Int64.to_float xx


  let convert_s_i64 x = Int64.to_float x

  let convert_u_i64 x =
      if Int64.compare x 0L >= 0
      then Int64.to_float x
      else
        let xx =
            Int64.(logor (shift_right_logical x 1) (logand x 1L))
            |> Int64.to_float
        in
        xx *. 2.0


  let promote_f32 x = Int32.float_of_bits x

  let reinterpret_i64 x = Int64.float_of_bits x
end
