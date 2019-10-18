type t =
    | COMMENT of string
    | LEFT_PAREN
    | RIGHT_PAREN
    | INT of int
    | FLOAT of float
    | STRING of string
    | ID of string
    | KEYWORD of string
    | RESERVED of string
[@@deriving show]
