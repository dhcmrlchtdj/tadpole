open! Containers
open Types
module R = Result

type p_context = {
    p: int;
    s: string;
  }

type 'a r = ('a * p_context, string) result

module Value = struct end

module Type = struct end

module Instruction = struct end

module Module = struct
  let magic = "\x00\x61\x73\x6d"

  let version = "\x01\x00\x00\x00"

  let aux_section (sid : char) ({ p; s } : p_context) : string r =
      let ch = s.[p] in
      if Char.equal ch sid then failwith "TODO" else failwith "TODO"

  let parse_customsec ctx = failwith "TODO"

  let parse_functype = failwith "TODO"

  let parse_import = failwith "TODO"

  let parse_typeidx = failwith "TODO"

  let parse_table = failwith "TODO"

  let parse_mem = failwith "TODO"

  let parse_global = failwith "TODO"

  let parse_export = failwith "TODO"

  let parse_start = failwith "TODO"

  let parse_elem = failwith "TODO"

  let parse_code = failwith "TODO"

  let parse_data = failwith "TODO"

  let parse_module (ctx : p_context) : moduledef r =
      let consume_magic ctx = failwith "TODO" in
      let consume_version ctx = failwith "TODO" in
      Ok ctx
      |> R.flat_map consume_magic
      |> R.flat_map consume_version
      |> R.map parse_customsec
      |> R.map parse_functype
      |> R.map parse_customsec
      |> R.map parse_import
      |> R.map parse_customsec
      |> R.map parse_typeidx
      |> R.map parse_customsec
      |> R.map parse_table
      |> R.map parse_customsec
      |> R.map parse_mem
      |> R.map parse_customsec
      |> R.map parse_global
      |> R.map parse_customsec
      |> R.map parse_export
      |> R.map parse_customsec
      |> R.map parse_start
      |> R.map parse_customsec
      |> R.map parse_elem
      |> R.map parse_customsec
      |> R.map parse_code
      |> R.map parse_customsec
      |> R.map parse_data
      |> R.map parse_customsec
      |> R.map (fun _ -> failwith "TODO")

  let parse (p : int) (s : string) : moduledef =
      let ctx : p_context = { p; s } in
      Ok ctx |> R.flat_map parse_module |> R.map (fun (m, _) -> m) |> R.get_exn
end

let parse = Module.parse 0
