open! Containers
open Types
module R = Result

let ( let* ) = Result.( >>= )

let ( let+ ) = Result.( >|= )

type p_context = {
    p: int;
    s: string;
    slen: int;
  }

type 'a or_err = ('a, string) result

let aux_read
    (len : int)
    (ctx : p_context)
    (f : string -> p_context -> 'a or_err) : 'a or_err =
    let { p; s; slen } = ctx in
    if p + len < slen
    then (
      let c = String.sub s p len in
      f c { ctx with p = p + len }
    )
    else Error "EOF"

module Value = struct
  let read_uint _ctx : (int * p_context) or_err = failwith "TODO"
end

module Type = struct end

module Instruction = struct end

module Module = struct
  type sections = {
      functype: string;
      typeidx: string;
      code: string;
      table: string;
      mem: string;
      global: string;
      elem: string;
      data: string;
      start: string;
      import: string;
      export: string;
    }

  type m_ctx = moduledef * p_context

  let magic = "\x00\x61\x73\x6d"

  let version = "\x01\x00\x00\x00"

  let aux_section (sid : char) (ctx : p_context) : (string * p_context) or_err =
      let { p; s; slen } = ctx in
      let ch = s.[p] in
      if Char.equal ch sid
      then Ok ("", ctx)
      else
        Ok { p = p + 1; s; slen }
        |> R.flat_map Value.read_uint
        |> R.flat_map (fun (size, ctx) ->
               aux_read size ctx (fun c ctx -> Ok (c, ctx)))

  let parse_types _x = failwith "TODO"

  let parse_funcs _x _x = failwith "TODO"

  let parse_tables _x = failwith "TODO"

  let parse_mems _x = failwith "TODO"

  let parse_globals _x = failwith "TODO"

  let parse_elem _x = failwith "TODO"

  let parse_data _x = failwith "TODO"

  let parse_start _x = failwith "TODO"

  let parse_imports _x = failwith "TODO"

  let parse_exports _x = failwith "TODO"

  let parse_module (ctx : p_context) : moduledef or_err =
      let consume_magic (ctx : p_context) : p_context or_err =
          aux_read 4 ctx (fun c ctx ->
              if String.equal c magic then Ok ctx else Error "magic error")
      in
      let consume_version (ctx : p_context) : p_context or_err =
          aux_read 4 ctx (fun c ctx ->
              if String.equal c version then Ok ctx else Error "version error")
      in
      let split_module (_ctx : p_context) =
          let sec =
              {
                functype = "";
                typeidx = "";
                code = "";
                table = "";
                mem = "";
                global = "";
                elem = "";
                data = "";
                start = "";
                import = "";
                export = "";
              }
          in
          Ok sec
      in

      let* ctx = consume_magic ctx in
      let* ctx = consume_version ctx in
      let* sec = split_module ctx in

      let* types = parse_types sec.functype in
      let* funcs = parse_funcs sec.typeidx sec.code in
      let* tables = parse_tables sec.table in
      let* mems = parse_mems sec.mem in
      let* globals = parse_globals sec.global in
      let* elem = parse_elem sec.elem in
      let* data = parse_data sec.data in
      let* start = parse_start sec.start in
      let* imports = parse_imports sec.import in
      let* exports = parse_exports sec.export in
      let m =
          {
            types;
            funcs;
            tables;
            mems;
            globals;
            elem;
            data;
            start;
            imports;
            exports;
          }
      in
      Ok m

  let parse (p : int) (s : string) : moduledef =
      let ctx : p_context = { p; s; slen = String.length s } in
      Ok ctx |> R.flat_map parse_module |> R.get_or_failwith
end

let parse = Module.parse 0
