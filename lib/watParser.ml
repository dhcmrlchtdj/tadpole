open! Containers
open Types

let ( let* ) = Result.( >>= )

type 'a or_err = ('a, string) result

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
end

module D = Datum

module Module = struct
  type tm = {
    types: functype Vector.vector;
    funcs: func Vector.vector;
    tables: table Vector.vector;
    mems: mem Vector.vector;
    globals: global Vector.vector;
    elem: elem Vector.vector;
    data: data Vector.vector;
    start: start option;
    imports: import Vector.vector;
    exports: export Vector.vector;
  }

  let parse_section (tm : tm) (ds : D.t list) =
    match ds with
    | _ -> failwith "TODO"

  let parse_module (d : D.t) : moduledef or_err =
    match d with
    | D.LIST (D.KEYWORD "module" :: t) ->
      let tm =
        {
          types = Vector.create ();
          funcs = Vector.create ();
          tables = Vector.create ();
          mems = Vector.create ();
          globals = Vector.create ();
          elem = Vector.create ();
          data = Vector.create ();
          start = None;
          imports = Vector.create ();
          exports = Vector.create ();
        }
      in
      let* tm = parse_section tm t in
      let m : moduledef =
        {
          types = Vector.to_array tm.types;
          funcs = Vector.to_array tm.funcs;
          tables = Vector.to_array tm.tables;
          mems = Vector.to_array tm.mems;
          globals = Vector.to_array tm.globals;
          elem = Vector.to_array tm.elem;
          data = Vector.to_array tm.data;
          start = tm.start;
          imports = Vector.to_array tm.imports;
          exports = Vector.to_array tm.exports;
        }
      in
      Ok m
    | _ -> Error "parse_module"

  let parse = parse_module
end

let parse (src : string) : Types.moduledef or_err =
  let* tokens = WatScanner.scan src in
  let* datum = Datum.of_tokens tokens in
  let () = print_endline (WatToken.to_string tokens) in
  let () = print_newline () in
  let () = print_endline (Datum.show datum) in
  let () = print_newline () in
  Module.parse datum
