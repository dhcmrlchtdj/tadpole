open! Containers
module T = WatToken

type 'a or_err = ('a, string) result

type t =
  | NUM of string
  | STRING of string
  | ID of string
  | KEYWORD of string
  | RESERVED of string
  | LIST of t list
[@@deriving show]

let of_tokens (tokens : T.t list) : t or_err =
  let rec aux : T.t list -> t option * T.t list = function
    | [] -> (None, [])
    | T.COMMENT _ :: t -> aux t
    | T.NUM x :: t -> (Some (NUM x), t)
    | T.STRING x :: t -> (Some (STRING x), t)
    | T.ID x :: t -> (Some (ID x), t)
    | T.KEYWORD x :: t -> (Some (KEYWORD x), t)
    | T.RESERVED x :: t -> (Some (RESERVED x), t)
    | T.LEFT_PAREN :: t -> read_list [] t
    | T.RIGHT_PAREN :: _ as t -> (None, t)
  and read_list acc t =
    match aux t with
    | (Some datum, tt) -> read_list (datum :: acc) tt
    | (None, T.RIGHT_PAREN :: tt) -> (Some (LIST (List.rev acc)), tt)
    | (None, _) -> (None, [])
  in
  match aux tokens with
  | (Some datum, []) -> Ok datum
  | (Some _, _) -> Error "unused token"
  | (None, _) -> Error "unexpected token list"
