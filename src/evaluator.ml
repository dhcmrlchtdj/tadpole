open! Containers
module D = Datum

(* *** *)

type value =
    | TestValue
    | NIL

let to_string (_v : value) : string = ""

(* *** *)

module E = Env.Make (struct
        type t = value
    end)

let env = E.create ()

(* *** *)

let evaluate (_datum : D.t) : value =
    let _ = E.set env "a" TestValue in
    let v = E.get env "a" in
    Option.get_exn v


let eval (datums : D.t list) : value =
    let f _prev datum = evaluate datum in
    List.fold_left f NIL datums
