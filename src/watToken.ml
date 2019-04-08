type t =
    | LEFT_PAREN
    | RIGHT_PAREN
    | KEYWORD of string
    | STRING of string
    | ID of string
    | RESERVED of string
    | FLOAT of float
    | INT of int
    | UINT of int
    | COMMENT of string
[@@deriving show]
