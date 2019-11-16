open! Containers

module Datum = struct
  module T = WatToken

  type t =
    | NUM of string
    | STRING of string
    | ID of string
    | KEYWORD of string
    | RESERVED of string
    | LIST of t list
  [@@deriving show]

  let of_tokens (tokens : T.t list) : t list =
    let rec aux (acc : t list) (t : T.t list) =
      match tokens2datum t with
        | Ok (Some datum, tt) -> aux (datum :: acc) tt
        | Ok (None, []) -> Ok (List.rev acc)
        | Ok (None, tt) -> aux acc tt
        | Error s -> Error s
    and tokens2datum = function
      | [] -> Ok (None, [])
      | T.COMMENT _ :: t -> Ok (None, t)
      | T.NUM x :: t -> Ok (Some (NUM x), t)
      | T.STRING x :: t -> Ok (Some (STRING x), t)
      | T.ID x :: t -> Ok (Some (ID x), t)
      | T.KEYWORD x :: t -> Ok (Some (KEYWORD x), t)
      | T.RESERVED x :: t -> Ok (Some (RESERVED x), t)
      | T.LEFT_PAREN :: t -> read_list [] t
      | T.RIGHT_PAREN :: _ -> Error "[tokens2datum] unexpected ')'"
    and read_list acc = function
      | T.RIGHT_PAREN :: tt -> Ok (Some (LIST (List.rev acc)), tt)
      | t -> (
        match tokens2datum t with
          | Ok (Some datum, tt) -> read_list (datum :: acc) tt
          | Ok (None, []) -> Error "[read_list] empty datum"
          | Ok (None, tt) -> read_list acc tt
          | Error err -> Error ("[read_list] error | " ^ err)
      )
    in
    match aux [] tokens with
      | Ok s -> s
      | Error s -> failwith s

  let to_string (ds : t list) = ds |> List.map show |> String.concat "\n"
end

let parse (src : string) : Types.moduledef =
  let _datum = src |> WatScanner.scan |> Datum.of_tokens in
  (* let _ = print_endline (Datum.to_string datum) in *)
  failwith "TODO"
