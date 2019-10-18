open! Containers

type t =
    | INT of int
    | FLOAT of float
    | STRING of string
    | ID of string
    | KEYWORD of string
    | RESERVED of string
    | LIST of t list
[@@deriving show, eq]
