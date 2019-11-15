open! Containers
open Types

let ( let* ) = Result.( >>= )

let ( let+ ) = Result.( >|= )

type 'a or_err = ('a, string) result

type source = {
    s: string;
    left: int;
    right: int;
  }

let aux_src2str ({ s; left; right } : source) = String.sub s left (right - left)

let aux_read (src : source) (len : int) : (source * source) or_err =
    let { left; right; _ } = src in
    if left < right && left + len <= right
    then (
      let sub = { src with right = left + len } in
      let src = { src with left = left + len } in
      Ok (sub, src)
    )
    else Error "EOF"

let aux_consume (pattern : string) (src : source) : source or_err =
    let len = String.length pattern in
    let* (sub, src) = aux_read src len in
    if String.equal pattern (aux_src2str sub)
    then Ok src
    else Error "aux_consume | failure"

module Value = struct
  let read_uint _ctx : (int * source) or_err = failwith "TODO"
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

  type m_ctx = moduledef * source

  let magic = "\x00\x61\x73\x6d"

  let version = "\x01\x00\x00\x00"

  let aux_section (sec_id : char) (src : source) :
      (source option * source) or_err =
      let { s; left; _ } = src in
      let ch = s.[left] in
      if Char.equal ch sec_id
      then (
        let src = { src with left = left + 1 } in
        let* (size, src) = Value.read_uint src in
        let* (sub, src) = aux_read src size in
        Ok (Some sub, src)
      )
      else Ok (None, src)

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

  let parse_module (ctx : source) : moduledef or_err =
      let split_module (_ctx : source) =
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

      let* ctx = aux_consume magic ctx in
      let* ctx = aux_consume version ctx in
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

  let parse (src : source) : moduledef =
      let m = parse_module src in
      Result.get_or_failwith m
end

let parse s = Module.parse { s; left = 0; right = String.length s }
