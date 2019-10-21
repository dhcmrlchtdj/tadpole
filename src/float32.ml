(* copy from
 * https://github.com/WebAssembly/spec/blob/994591e51c9df9e7ef980b04d660709b79982f75/interpreter/exec/float.ml
 *)

type t = Int32.t

let pos_inf = Int32.bits_of_float (1.0 /. 0.0)

let neg_inf = Int32.bits_of_float (-.(1.0 /. 0.0))

let pos_nan = 0x7fc00000l

let neg_nan = 0xffc00000l

let bare_nan = 0x7f800000l

let bits_of_int32 (x : Int32.t) : t = x

let int32_of_bits (x : t) : Int32.t = x

let of_float = Int32.bits_of_float

let to_float = Int32.float_of_bits

let is_inf x = x = pos_inf || x = neg_inf

let is_nan x =
    let xf = Int32.float_of_bits x in
    xf <> xf


(*
 * When the result of an arithmetic operation is NaN, the most significant
 * bit of the significand field is set.
 *)
let canonicalize_nan x = Int32.logor x pos_nan

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


(*
 * When the result of a unary operation is NaN, the resulting NaN is computed
 * from one of the NaN input, if there it is NaN. Otherwise, we use a default
 * NaN value.
 *)
let determine_unary_nan x =
    (*
     * TODO: There is one nondeterministic thing we could do here. When the
     * operand is not NaN, we can nondeterministically pick whether to return
     * pos_nan or neg_nan.
     *)
    let nan = if is_nan x then x else pos_nan in
    canonicalize_nan nan


let binary x op y =
    let xf = to_float x in
    let yf = to_float y in
    let t = op xf yf in
    if t = t then of_float t else determine_binary_nan x y


let unary op x =
    let t = op (to_float x) in
    if t = t then of_float t else determine_unary_nan x


let zero = of_float 0.0

let add x y = binary x ( +. ) y

let sub x y = binary x ( -. ) y

let mul x y = binary x ( *. ) y

let div x y = binary x ( /. ) y

let sqrt x = unary Stdlib.sqrt x

let ceil x = unary Stdlib.ceil x

let floor x = unary Stdlib.floor x

let trunc x =
    let xf = to_float x in
    (* preserve the sign of zero *)
    if xf = 0.0
    then x
    else
      (* trunc is either ceil or floor depending on which one is toward zero *)
      let f = if xf < 0.0 then Stdlib.ceil xf else Stdlib.floor xf in
      let result = of_float f in
      if is_nan result then determine_unary_nan result else result


let nearest x =
    let xf = to_float x in
    (* preserve the sign of zero *)
    if xf = 0.0
    then x
    else
      (* nearest is either ceil or floor depending on which is nearest or even *)
      let u = Stdlib.ceil xf in
      let d = Stdlib.floor xf in
      let um = abs_float (xf -. u) in
      let dm = abs_float (xf -. d) in
      let u_or_d =
          um < dm
          || ( um = dm
             &&
             let h = u /. 2. in
             Stdlib.floor h = h )
      in
      let f = if u_or_d then u else d in
      let result = of_float f in
      if is_nan result then determine_unary_nan result else result


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


(* abs, neg, and copysign are purely bitwise operations, even on NaN values *)
let abs x = Int32.logand x Int32.max_int

let neg x = Int32.logxor x Int32.min_int

let copy_sign x y = Int32.logor (abs x) (Int32.logand y Int32.min_int)

let eq x y = to_float x = to_float y

let ne x y = to_float x <> to_float y

let lt x y = to_float x < to_float y

let gt x y = to_float x > to_float y

let le x y = to_float x <= to_float y

let ge x y = to_float x >= to_float y

let of_signless_string s =
    if s = "inf"
    then pos_inf
    else if s = "nan"
    then pos_nan
    else if String.length s > 6 && String.sub s 0 6 = "nan:0x"
    then
      let x = Int32.of_string (String.sub s 4 (String.length s - 4)) in
      if x = Int32.zero
      then raise (Failure "nan payload must not be zero")
      else if Int32.logand x bare_nan <> Int32.zero
      then raise (Failure "nan payload must not overlap with exponent bits")
      else if x < Int32.zero
      then raise (Failure "nan payload must not overlap with sign bit")
      else Int32.logor x bare_nan
    else
      (* TODO: once we update past 4.02, replace buffer hack with this
      let s' = String.concat "" (String.split_on_char '_' s) in
      *)
      let buf = Buffer.create (String.length s) in
      for i = 0 to String.length s - 1 do
        if s.[i] <> '_' then Buffer.add_char buf s.[i]
      done ;
      let s' = Buffer.contents buf in
      let x = of_float (float_of_string s') in
      if is_inf x then failwith "of_string" else x


let of_string s =
    if s = ""
    then failwith "of_string"
    else if s.[0] = '+' || s.[0] = '-'
    then
      let x = of_signless_string (String.sub s 1 (String.length s - 1)) in
      if s.[0] = '+' then x else neg x
    else of_signless_string s


let to_string x =
    (if x < Int32.zero then "-" else "")
    ^
    if is_nan x
    then
      "nan:0x"
      ^ Printf.sprintf "%lx" (Int32.logand (abs x) (Int32.lognot bare_nan))
    else
      (* TODO: use sprintf "%h" once we have upgraded to OCaml 4.03 *)
      string_of_float (to_float (abs x))
