(* original https://github.com/WebAssembly/spec/blob/994591e51c9df9e7ef980b04d660709b79982f75/interpreter/exec/float.ml *)

(* include Stdlib.Int32 *)

type t = Int32.t

let of_float = Int32.bits_of_float

let to_float = Int32.float_of_bits

let pos_nan = 0x7fc00000l

let is_nan x =
    let xf = Int32.float_of_bits x in
    xf <> xf


let zero = of_float 0.0

(*
 * When the result of an arithmetic operation is NaN, the most significant
 * bit of the significand field is set.
 *)
let canonicalize_nan x = Int32.logor x pos_nan

let determine_unary_nan x =
    (*
     * TODO: There is one nondeterministic thing we could do here. When the
     * operand is not NaN, we can nondeterministically pick whether to return
     * pos_nan or neg_nan.
     *)
    let nan = if is_nan x then x else pos_nan in
    canonicalize_nan nan


let unary op x =
    let t = op (to_float x) in
    if t = t then of_float t else determine_unary_nan x


(*
 * When the result of a binary operation is NaN, the resulting NaN is computed
 * from one of the NaN inputs, if there is one. If both are NaN, one is
 * selected nondeterminstically. If neither, we use a default NaN value.
 *)
let determine_binary_nan x y =
    (*
     * TODO: There are two nondeterministic things we could do here. When both
     * x and y are NaN, we can nondeterministically pick which to return. And
     * when neither is NaN, we can nondeterministically pick whether to return
     * pos_nan or neg_nan.
     *)
    let nan = if is_nan x then x else if is_nan y then y else pos_nan in
    canonicalize_nan nan


let binary x op y =
    let xf = to_float x in
    let yf = to_float y in
    let t = op xf yf in
    if t = t then of_float t else determine_binary_nan x y


module UnOp = struct
  let abs x = Int32.logand x Int32.max_int

  let neg x = Int32.logxor x Int32.min_int

  let sqrt x = unary Float.sqrt x

  let ceil x = unary Float.ceil x

  let floor x = unary Float.floor x

  let trunc x =
      let xf = to_float x in
      (* preserve the sign of zero *)
      if xf = 0.0
      then x
      else
        (* trunc is either ceil or floor depending on which one is toward zero *)
        let f = if xf < 0.0 then Float.ceil xf else Float.floor xf in
        let result = of_float f in
        if is_nan result then determine_unary_nan result else result


  let nearest x =
      let xf = to_float x in
      (* preserve the sign of zero *)
      if xf = 0.0
      then x
      else
        (* nearest is either ceil or floor depending on which is nearest or even *)
        let u = Float.ceil xf in
        let d = Float.floor xf in
        let um = abs_float (xf -. u) in
        let dm = abs_float (xf -. d) in
        let u_or_d =
            um < dm
            || ( um = dm
               &&
               let h = u /. 2. in
               Float.floor h = h )
        in
        let f = if u_or_d then u else d in
        let result = of_float f in
        if is_nan result then determine_unary_nan result else result
end

module BinOp = struct
  let add x y = binary x ( +. ) y

  let sub x y = binary x ( -. ) y

  let mul x y = binary x ( *. ) y

  let div x y = binary x ( /. ) y

  let min x y =
      let xf = to_float x in
      let yf = to_float y in
      (* min -0 0 is -0 *)
      if xf = yf
      then Int32.logor x y
      else if xf < yf
      then x
      else if xf > yf
      then y
      else determine_binary_nan x y


  let max x y =
      let xf = to_float x in
      let yf = to_float y in
      (* max -0 0 is 0 *)
      if xf = yf
      then Int32.logand x y
      else if xf > yf
      then x
      else if xf < yf
      then y
      else determine_binary_nan x y


  let copy_sign x y = Int32.logor (UnOp.abs x) (Int32.logand y Int32.min_int)
end

module RelOp = struct
  let eq x y = to_float x = to_float y

  let ne x y = to_float x <> to_float y

  let lt x y = to_float x < to_float y

  let gt x y = to_float x > to_float y

  let le x y = to_float x <= to_float y

  let ge x y = to_float x >= to_float y
end

module CvtOp = struct
  (* https://github.com/WebAssembly/spec/blob/994591e51c9df9e7ef980b04d660709b79982f75/interpreter/exec/f32_convert.ml *)

  let convert_s_i32 x = x |> Int32.to_float |> of_float

  let convert_u_i32 x =
      if x >= 0l
      then x |> Int32.to_float |> of_float
      else
        let xx =
            Int32.(logor (shift_right_logical x 1) (logand x 1l))
            |> Int32.to_float
        in
        of_float (xx *. 2.0)


  let convert_s_i64 x = x |> Int64.to_float |> of_float

  let convert_u_i64 x =
      if x >= 0L
      then x |> Int64.to_float |> of_float
      else
        let xx = Int64.shift_right_logical x 1 |> Int64.to_float in
        of_float (xx *. 2.0)


  let demote_f64 x = if Float.is_nan x then pos_nan else of_float x

  let reinterpret_i32 x = x
end
