open! Containers
module D = Datum

(* *** *)

type value = TestValue

let to_string (_v : value) : string = ""

(* *** *)

module E = Env.Make (struct
        type t = value
    end)

(* *** *)

let evaluate (_ds : D.t list) (env : E.t) : value =
    let _ = E.set env "a" TestValue in
    let v = E.get env "a" in
    Option.get_exn v


let eval (ds : D.t list) : value = evaluate ds (E.create ())
