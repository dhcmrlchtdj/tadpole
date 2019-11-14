open! Containers

type t =
    | COMMENT of string
    | LEFT_PAREN
    | RIGHT_PAREN
    | NUM of string
    | STRING of string
    | ID of string
    | KEYWORD of string
    | RESERVED of string
[@@deriving show]

let to_string (ts : t list) = ts |> List.map show |> String.concat "\n"
