(* include Stdlib.Float *)

type t = Float.t

let equal = Float.equal

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
    let convert_s_i32 _x = failwith ""
    let convert_s_i64 _x = failwith ""
    let convert_u_i32 _x = failwith ""
    let convert_u_i64 _x = failwith ""
    let promote_f32 _x = failwith ""
    let reinterpret_i32 _x = failwith ""
    let reinterpret_i64 _x = failwith ""
end
