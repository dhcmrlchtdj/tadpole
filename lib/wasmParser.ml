open! Containers
open Types

let ( let* ) = Result.( >>= )

let ( let+ ) = Result.( >|= )

type 'a or_err = ('a, string) result

module S = struct
  type t = {
      s: string;
      left: int;
      right: int;
    }

  let empty = { s = ""; left = 0; right = 0 }

  let of_string (s : string) : t = { s; left = 0; right = String.length s }

  let to_string ({ s; left; right } : t) : string =
      String.sub s left (right - left)

  let concat (t1 : t) (t2 : t) : t =
      let s1 = to_string t1 in
      let s2 = to_string t2 in
      let s = s1 ^ s2 in
      of_string s

  let read (len : int) (src : t) : (t * t) or_err =
      let { left; right; _ } = src in
      if left < right && left + len <= right
      then (
        let sub = { src with right = left + len } in
        let src = { src with left = left + len } in
        Ok (sub, src)
      )
      else Error "EOF"

  let consume (pattern : string) (src : t) : t or_err =
      let len = String.length pattern in
      let* (sub, src) = read len src in
      if String.equal pattern (to_string sub)
      then Ok src
      else Error "aux_consume | failure"
end

module Value = struct
  let read_uint _ctx : (int * S.t) or_err = failwith "TODO"
end

module Type = struct end

module Instruction = struct end

module Module = struct
  type sections = {
      typesec: S.t;
      importsec: S.t;
      funcsec: S.t;
      tablesec: S.t;
      memsec: S.t;
      globalsec: S.t;
      exportsec: S.t;
      startsec: S.t;
      elemsec: S.t;
      codesec: S.t;
      datasec: S.t;
    }

  let empty_sections =
      {
        typesec = S.empty;
        importsec = S.empty;
        funcsec = S.empty;
        tablesec = S.empty;
        memsec = S.empty;
        globalsec = S.empty;
        exportsec = S.empty;
        startsec = S.empty;
        elemsec = S.empty;
        codesec = S.empty;
        datasec = S.empty;
      }

  let aux_read_section (src : S.t) : (int * S.t * S.t) or_err =
      let* (id, src) = S.read 1 src in
      let id = S.to_string id in
      let id = Char.code id.[0] in
      if id > 11
      then Error "TODO"
      else
        let* (size, src) = Value.read_uint src in
        let* (sec, src) = S.read size src in
        Ok (id, sec, src)

  let aux_split_module (src : S.t) : sections or_err =
      let rec aux (acc : sections) (src : S.t) : sections or_err =
          let ({ left; right; _ } : S.t) = src in
          if left = right
          then Ok acc
          else
            let* (sec_id, sec, src) = aux_read_section src in
            let acc =
                match sec_id with
                    | 0 -> acc
                    | 1 -> { acc with typesec = S.concat acc.typesec sec }
                    | 2 -> { acc with importsec = S.concat acc.importsec sec }
                    | 3 -> { acc with funcsec = S.concat acc.funcsec sec }
                    | 4 -> { acc with tablesec = S.concat acc.tablesec sec }
                    | 5 -> { acc with memsec = S.concat acc.memsec sec }
                    | 6 -> { acc with globalsec = S.concat acc.globalsec sec }
                    | 7 -> { acc with exportsec = S.concat acc.exportsec sec }
                    | 8 -> { acc with startsec = S.concat acc.startsec sec }
                    | 9 -> { acc with elemsec = S.concat acc.elemsec sec }
                    | 10 -> { acc with codesec = S.concat acc.codesec sec }
                    | 11 -> { acc with datasec = S.concat acc.datasec sec }
                    | _ -> failwith "never"
            in
            aux acc src
      in
      aux empty_sections src

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

  let parse_module (src : S.t) : moduledef or_err =
      let magic = "\x00\x61\x73\x6d" in
      let version = "\x01\x00\x00\x00" in
      let* src = S.consume magic src in
      let* src = S.consume version src in
      let* sec = aux_split_module src in

      let* types = parse_types sec.typesec in
      let* funcs = parse_funcs sec.funcsec sec.codesec in
      let* tables = parse_tables sec.tablesec in
      let* mems = parse_mems sec.memsec in
      let* globals = parse_globals sec.globalsec in
      let* elem = parse_elem sec.elemsec in
      let* data = parse_data sec.datasec in
      let* start = parse_start sec.startsec in
      let* imports = parse_imports sec.importsec in
      let* exports = parse_exports sec.exportsec in
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

  let parse (src : S.t) : moduledef =
      let m = parse_module src in
      Result.get_or_failwith m
end

let parse s = Module.parse (S.of_string s)
