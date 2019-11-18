type t = Int32.t [@@deriving show]

let equal = Int32.equal

let to_int = Int32.to_int

let of_int = Int32.of_int

let bits_of_float32 x = x

let float32_of_bits x = x

let to_string = Int32.to_string

let of_string = Int32.of_string_opt

let wrap_to n x =
  match n with
  | 8 -> Int32.logand x 0xffl
  | 16 -> Int32.logand x 0xffffl
  | _ -> failwith "never"

let to_bytes x =
  let b = Bytes.create 4 in
  let () = Bytes.set_int32_be b 0 x in
  b

let of_bytes b = Bytes.get_int32_be b 0

module UnOp = struct
  let clz i =
    let j = ref i in
    let cnt = ref 0 in
    let break = ref false in
    let loop = ref 1 in
    while Bool.equal !break false && !loop <= 32 do
      let r = Int32.logand 0x8000_0000l !j in
      if Int32.equal r 0l
      then (
        incr cnt;
        incr loop;
        j := Int32.shift_left !j 1
      )
      else break := true
    done;
    Int32.of_int !cnt

  let ctz i =
    let j = ref i in
    let cnt = ref 0 in
    let break = ref false in
    let loop = ref 1 in
    while Bool.equal !break false && !loop <= 32 do
      let r = Int32.logand 1l !j in
      if Int32.equal r 0l
      then (
        incr cnt;
        incr loop;
        j := Int32.shift_right_logical !j 1
      )
      else break := true
    done;
    Int32.of_int !cnt

  let popcnt i =
    let j = ref i in
    let cnt = ref 0 in
    for _ = 1 to 32 do
      let r = Int32.logand 1l !j in
      if Int32.equal r 1l then incr cnt;
      j := Int32.shift_right_logical !j 1
    done;
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
    let k = Int32.to_int y mod 32 in
    Int32.shift_left x k

  let shr_s x y =
    let k = Int32.to_int y mod 32 in
    Int32.shift_right x k

  let shr_u x y =
    let k = Int32.to_int y mod 32 in
    Int32.shift_right_logical x k

  let rotl x y =
    let k = Int32.to_int y mod 32 in
    let l = Int32.shift_left x k in
    let r = Int32.shift_right_logical x (32 - k) in
    Int32.add l r

  let rotr x y =
    let k = Int32.to_int y mod 32 in
    let l = Int32.shift_left x (32 - k) in
    let r = Int32.shift_right_logical x k in
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
  (* https://github.com/WebAssembly/spec/blob/994591e51c9df9e7ef980b04d660709b79982f75/interpreter/exec/i32_convert.ml *)

  let wrap_i64 = Int64.to_int32

  let trunc_s_f32 x =
    let xf = Int32.float_of_bits x in
    if Float.is_nan xf then failwith "integer nan";
    if xf >= -.Int32.(to_float min_int) || xf < Int32.(to_float min_int)
    then failwith "integer overflow"
    else Int32.of_float xf

  let trunc_u_f32 x =
    let xf = Int32.float_of_bits x in
    if Float.is_nan xf then failwith "integer nan";
    if xf >= -.Int32.(to_float min_int) *. 2.0 || xf <= -1.0
    then failwith "integer overflow"
    else xf |> Int64.of_float |> Int64.to_int32

  let trunc_s_f64 x =
    let xf = x in
    if Float.is_nan xf then failwith "integer nan";
    if xf >= -.Int32.(to_float min_int) || xf < Int32.(to_float min_int)
    then failwith "integer overflow"
    else Int32.of_float xf

  let trunc_u_f64 x =
    let xf = x in
    if Float.is_nan xf then failwith "integer nan";
    if xf >= -.Int32.(to_float min_int) *. 2.0 || xf <= -1.0
    then failwith "integer overflow"
    else xf |> Int64.of_float |> Int64.to_int32

  let reinterpret_f32 x = x
end
