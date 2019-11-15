open! Containers
open Types

let ( let* ) = Result.( >>= )

(* let ( let+ ) = Result.( >|= ) *)

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

  let take (len : int) (src : t) : (t * t) or_err =
    let { left; right; _ } = src in
    if left + len <= right
    then (
      let sub = { src with right = left + len } in
      let src = { src with left = left + len } in
      Ok (sub, src)
    )
    else Error "EOF"

  let take_char (src : t) : (char * t) or_err =
    let { s; left; right } = src in
    if left = right
    then Error "EOF"
    else (
      let ch = s.[left] in
      let src = { src with left = left + 1 } in
      Ok (ch, src)
    )

  let take_int (src : t) : (int * t) or_err =
    let* (ch, src) = take_char src in
    let n = Char.code ch in
    Ok (n, src)

  let peek_char (src : t) : char or_err =
    let { s; left; right } = src in
    if left = right
    then Error "EOF"
    else (
      let ch = s.[left] in
      Ok ch
    )

  let peek_int (src : t) : int or_err =
    let* ch = peek_char src in
    let n = Char.code ch in
    Ok n

  let consume (pattern : string) (src : t) : t or_err =
    let len = String.length pattern in
    let* (sub, src) = take len src in
    if String.equal pattern (to_string sub)
    then Ok src
    else Error "consume | failure"

  let consume_char (p : char) (src : t) : t or_err =
    let* (ch, src) = take_char src in
    if Char.equal p ch then Ok src else Error "consume_char | failure"

  let skip (len : int) (src : t) : t or_err =
    let { left; right; _ } = src in
    if left + len <= right
    then (
      let src = { src with left = left + len } in
      Ok src
    )
    else Error "EOF"
end

module Value = struct
  let aux_leb128 (src : S.t) : (string * S.t) or_err =
    let rec aux (acc : int list) (src : S.t) =
      let* (n, src) = S.take_int src in
      let acc = n :: acc in
      if n land 0x80 = 0
      then (
        let s = acc |> List.rev_map Char.chr |> String.of_list in
        Ok (s, src)
      )
      else aux acc src
    in
    aux [] src

  let uint (src : S.t) : (int * S.t) or_err =
    let* (s, src) = aux_leb128 src in
    let i = s |> Leb128.Unsigned.decode |> Int64.to_int in
    Ok (i, src)

  let i32 (src : S.t) : (Nint32.t * S.t) or_err =
    let* (s, src) = aux_leb128 src in
    let i = s |> Leb128.Signed.decode |> Int64.to_int32 in
    Ok (i, src)

  let i64 (src : S.t) : (Nint64.t * S.t) or_err =
    let* (s, src) = aux_leb128 src in
    let i = s |> Leb128.Signed.decode in
    Ok (i, src)

  let f32 (src : S.t) : (Nfloat32.t * S.t) or_err =
    let* (sub, src) = S.take 4 src in
    let n = sub |> S.to_string |> Bytes.of_string |> Nfloat32.of_bytes_le in
    Ok (n, src)

  let f64 (src : S.t) : (Nfloat64.t * S.t) or_err =
    let* (sub, src) = S.take 8 src in
    let n = sub |> S.to_string |> Bytes.of_string |> Nfloat64.of_bytes_le in
    Ok (n, src)

  let byte (src : S.t) : (bytes * S.t) or_err =
    let* (size, src) = uint src in
    let* (b, src) = S.take size src in
    let b = b |> S.to_string |> Bytes.of_string in
    Ok (b, src)

  let name (src : S.t) : (string * S.t) or_err =
    let* (size, src) = uint src in
    let* (n, src) = S.take size src in
    let n = S.to_string n in
    Ok (n, src)

  let idx = uint
end

let aux_vec (src : S.t) (f : S.t -> ('a * S.t) or_err) : ('a list * S.t) or_err =
  let rec aux (acc : 'a list) (src : S.t) n =
    if n = 0
    then (
      let l = List.rev acc in
      Ok (l, src)
    )
    else
      let* (a, src) = f src in
      aux (a :: acc) src (n - 1)
  in
  let* (n, src) = Value.uint src in
  aux [] src n

module Type = struct
  let valtype (s : S.t) : (valtype * S.t) or_err =
    let* (t, s) = S.take_char s in
    match t with
      | '\x7f' -> Ok (TI32, s)
      | '\x7e' -> Ok (TI64, s)
      | '\x7d' -> Ok (TF32, s)
      | '\x7c' -> Ok (TF64, s)
      | _ -> Error "Type.valtype"

  let resulttype (s : S.t) : (resulttype * S.t) or_err =
    let* c = S.peek_char s in
    if Char.equal c '\x40'
    then
      let* s = S.skip 1 s in
      Ok ([], s)
    else
      let* (v, s) = valtype s in
      Ok ([ v ], s)

  let functype (s : S.t) : (functype * S.t) or_err =
    let* s = S.consume_char '\x60' s in
    let* (t1, s) = aux_vec s valtype in
    let* (t2, s) = aux_vec s valtype in
    let func = (t1, t2) in
    Ok (func, s)

  let limits (s : S.t) : (limits * S.t) or_err =
    let* (t, s) = S.take_char s in
    match t with
      | '\x00' ->
        let* (n, s) = Value.uint s in
        let limits = { min = n; max = None } in
        Ok (limits, s)
      | '\x01' ->
        let* (n, s) = Value.uint s in
        let* (m, s) = Value.uint s in
        let limits = { min = n; max = Some m } in
        Ok (limits, s)
      | _ -> Error "Type.limits"

  let memtype (s : S.t) : (memtype * S.t) or_err = limits s

  let elemtype (s : S.t) : (elemtype * S.t) or_err =
    let* s = S.consume_char '\x70' s in
    Ok (FUNCREF, s)

  let tabletype (s : S.t) : (tabletype * S.t) or_err =
    let* (et, s) = elemtype s in
    let* (lim, s) = limits s in
    let table = (lim, et) in
    Ok (table, s)

  let mut (s : S.t) : (mut * S.t) or_err =
    let* (t, s) = S.take_char s in
    match t with
      | '\x00' -> Ok (CONST, s)
      | '\x01' -> Ok (VAR, s)
      | _ -> Error "Type.mut"

  let globaltype (s : S.t) : (globaltype * S.t) or_err =
    let* (t, s) = valtype s in
    let* (m, s) = mut s in
    let g = (m, t) in
    Ok (g, s)
end

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
    let* (id, src) = S.take_int src in
    if id > 11
    then Error "unsupported section"
    else
      let* (size, src) = Value.uint src in
      let* (sec, src) = S.take size src in
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
