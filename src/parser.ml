open! Containers
module T = Token
module D = Datum

let tokens2datums (tokens : T.t list) : D.t list =
    let rec aux (acc : D.t list) (t : T.t list) =
        match tokens2datum t with
            | Ok (Some datum, tt) -> aux (datum :: acc) tt
            | Ok (None, []) -> Ok (List.rev acc)
            | Ok (None, tt) -> aux acc tt
            | Error s -> Error s
    and tokens2datum = function
        | [] -> Ok (None, [])
        | T.COMMENT _ :: t -> Ok (None, t)
        | T.INT x :: t -> Ok (Some (D.INT x), t)
        | T.FLOAT x :: t -> Ok (Some (D.FLOAT x), t)
        | T.STRING x :: t -> Ok (Some (D.STRING x), t)
        | T.ID x :: t -> Ok (Some (D.ID x), t)
        | T.KEYWORD x :: t -> Ok (Some (D.KEYWORD x), t)
        | T.RESERVED x :: t -> Ok (Some (D.RESERVED x), t)
        | T.LEFT_PAREN :: t -> read_list [] t
        | T.RIGHT_PAREN :: _ -> Error "[tokens2datum] unexpected ')'"
    and read_list acc = function
        | T.RIGHT_PAREN :: tt -> Ok (Some (D.LIST (List.rev acc)), tt)
        | t ->
            (match tokens2datum t with
                | Ok (Some datum, tt) -> read_list (datum :: acc) tt
                | Ok (None, []) -> Error "[read_list] empty datum"
                | Ok (None, tt) -> read_list acc tt
                | Error err -> Error ("[read_list] error | " ^ err))
    in
    match aux [] tokens with Ok s -> s | Error s -> failwith s


let parse (tokens : T.t list) : D.t list = tokens |> tokens2datums
