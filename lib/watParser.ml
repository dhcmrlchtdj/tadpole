open! Containers
open Types
module D = WatDatum

let ( let* ) = Result.( >>= )

type 'a or_err = ('a, string) result

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

module Value = struct
  let u32 (x : string) : u32 or_err = Int.of_string x |> Result.of_opt

  let i32 (x : string) : Nint32.t or_err = Nint32.of_string x |> Result.of_opt

  let i64 (x : string) : Nint64.t or_err = Nint64.of_string x |> Result.of_opt

  let f32 (x : string) : Nfloat32.t or_err =
    Nfloat32.of_string x |> Result.of_opt

  let f64 (x : string) : Nfloat64.t or_err =
    Nfloat64.of_string x |> Result.of_opt

  let byte (x : string) : bytes = Bytes.of_string x

  let idx (_tm : tm) (d : D.t) : u32 or_err =
    match d with
    | D.NUM s -> u32 s
    | _ -> failwith "unsupported idx"
end

module Type = struct
  let valtype = function
    | "i32" -> Ok TI32
    | "i64" -> Ok TI64
    | "f32" -> Ok TF32
    | "f64" -> Ok TF64
    | _ -> Error "invalid valtype"

  let resulttype (ds : D.t list) : resulttype or_err =
    ds
    |> List.map (function
         | D.KEYWORD kw -> valtype kw
         | _ -> Error "expect keyword")
    |> Result.flatten_l

  let functype (ds : D.t list) : functype or_err =
    let rec aux p r = function
      | [] -> Ok (List.rev p, List.rev r)
      | D.LIST [ D.KEYWORD "param"; D.ID _id; D.KEYWORD pt ] :: t ->
        (* TODO id *)
        let* pt = valtype pt in
        aux (pt :: p) r t
      | D.LIST (D.KEYWORD "param" :: pts) :: t ->
        let* pts =
          pts
          |> List.rev_map (function
               | D.KEYWORD kw -> valtype kw
               | _ -> Error "expect keyword")
          |> Result.flatten_l
        in
        aux (pts @ p) r t
      | D.LIST (D.KEYWORD "result" :: rts) :: t ->
        let* rts =
          rts
          |> List.rev_map (function
               | D.KEYWORD kw -> valtype kw
               | _ -> Error "expect keyword")
          |> Result.flatten_l
        in
        aux p (rts @ r) t
      | _ -> Error "invalid functype"
    in
    aux [] [] ds

  let limits (n : string) (m : string option) : limits or_err =
    let* min = Value.u32 n in
    if min < 0 || min > (* 4GiB *) 0x10000
    then Error "limits | min size"
    else
      let* max =
        match m with
        | None -> Ok None
        | Some m ->
          let* m = Value.u32 m in
          if m < min || m > (* 4GiB *) 0x10000
          then Error "limits | max size"
          else Ok (Some m)
      in
      Ok { min; max }

  let memtype (ds : D.t list) : memtype or_err =
    match ds with
    | [ D.NUM n ] -> limits n None
    | [ D.NUM n; D.NUM m ] -> limits n (Some m)
    | _ -> Error "memtype | invalid"

  let tabletype (ds : D.t list) : tabletype or_err =
    match ds with
    | [ D.NUM n; D.KEYWORD "funcref" ] ->
      let* lim = limits n None in
      Ok (lim, FUNCREF)
    | [ D.NUM n; D.NUM m; D.KEYWORD "funcref" ] ->
      let* lim = limits n (Some m) in
      Ok (lim, FUNCREF)
    | _ -> Error "tabletype | invalid"

  let globaltype (d : D.t) : globaltype or_err =
    match d with
    | D.STRING v ->
      let* v = valtype v in
      Ok (CONST, v)
    | D.LIST [ D.KEYWORD "mut"; D.STRING v ] ->
      let* v = valtype v in
      Ok (VAR, v)
    | _ -> Error "globaltype | invalid"
end

module Instruction = struct
  let expr _ : expr or_err = failwith "TODO expr"
end

module Module = struct
  let aux_fresh_id =
    let cnt = ref 0 in
    fun () ->
      let id = Printf.sprintf "$tadpole_fresh_id@%d~" !cnt in
      let () = incr cnt in
      id

  let aux_match_id = function
    | D.ID id :: t -> (D.ID id, t)
    | t -> (D.ID (aux_fresh_id ()), t)

  let rec parse_section (tm : tm) (ds : D.t list) : tm or_err =
    match ds with
    | [] -> Ok tm
    | D.LIST (D.KEYWORD k :: h) :: t ->
      let* f =
        match k with
        | "type" -> Ok parse_typesec
        | "import" -> Ok parse_importsec
        | "func" -> Ok parse_funcsec
        | "table" -> Ok parse_tablesec
        | "memory" -> Ok parse_memorysec
        | "global" -> Ok parse_globalsec
        | "export" -> Ok parse_exportsec
        | "start" -> Ok parse_startsec
        | "elem" -> Ok parse_elemsec
        | "data" -> Ok parse_datasec
        | _ -> Error "unkonwn section"
      in
      let* tm = f tm h in
      parse_section tm t
    | _ -> Error "unexpected section"

  and aux_parse_typeuse (tm : tm) (ds : D.t list) : typeidx or_err =
    match ds with
    | D.LIST (D.KEYWORD "param" :: _) :: _ -> failwith "unsupported"
    | D.LIST (D.KEYWORD "result" :: _) :: _ -> failwith "unsupported"
    | D.LIST [ D.KEYWORD "type"; _ ] :: D.LIST (D.KEYWORD "param" :: _) :: _ ->
      failwith "unsupported"
    | D.LIST [ D.KEYWORD "type"; _ ] :: D.LIST (D.KEYWORD "result" :: _) :: _ ->
      failwith "unsupported"
    | [ D.LIST [ D.KEYWORD "type"; x ] ] -> Value.idx tm x
    | _ -> Error "typeuse | invalid"

  and parse_typesec (tm : tm) (ds : D.t list) : tm or_err =
    let (_id, ds) = aux_match_id ds in
    match ds with
    | [ D.LIST (D.KEYWORD "func" :: f) ] ->
      (* TODO id *)
      let* ft = Type.functype f in
      let () = Vector.push tm.types ft in
      Ok tm
    | _ -> Error "typesec | invalid"

  and parse_importsec (tm : tm) (ds : D.t list) : tm or_err =
    match ds with
    | [ D.STRING modname; D.STRING name; D.LIST idesc ] ->
      let* desc =
        (* TODO id *)
        match idesc with
        | D.KEYWORD "func" :: t ->
          let (_id, t) = aux_match_id t in
          let* typ = aux_parse_typeuse tm t in
          Ok (ID_func typ)
        | D.KEYWORD "table" :: t ->
          let (_id, t) = aux_match_id t in
          let* idx = Type.tabletype t in
          Ok (ID_table idx)
        | D.KEYWORD "memory" :: t ->
          let (_id, t) = aux_match_id t in
          let* idx = Type.memtype t in
          Ok (ID_mem idx)
        | D.KEYWORD "global" :: t -> (
          let (_id, t) = aux_match_id t in
          match t with
          | [ t ] ->
            let* idx = Type.globaltype t in
            Ok (ID_global idx)
          | _ -> Error "importdesc | invalid global"
        )
        | _ -> Error "importsec | invalid desc"
      in
      let i = { modname; name; desc } in
      let () = Vector.push tm.imports i in
      Ok tm
    | _ -> Error "importsec | invalid"

  and parse_funcsec (tm : tm) (ds : D.t list) : tm or_err =
    let aux_parse_type ds =
      match ds with
      | (D.LIST (D.KEYWORD "type" :: _) as h) :: t ->
        let* typei = aux_parse_typeuse tm [ h ] in
        Ok (typei, t)
      | _ -> Error "unsupported func type"
    in
    let aux_parse_local ds =
      let rec aux acc = function
        | D.LIST [ D.KEYWORD "local"; D.ID _; D.KEYWORD v ] :: t ->
          let* v = Type.valtype v in
          aux (v :: acc) t
        | D.LIST [ D.KEYWORD "local"; D.KEYWORD v ] :: t ->
          let* v = Type.valtype v in
          aux (v :: acc) t
        | ds -> Ok (List.rev acc, ds)
      in
      aux [] ds
    in
    let (_id, ds) = aux_match_id ds in
    let* (typei, ds) = aux_parse_type ds in
    let* (locals, ds) = aux_parse_local ds in
    let* body = Instruction.expr ds in
    let f = { typei; locals; body } in
    let () = Vector.push tm.funcs f in
    Ok tm

  and parse_tablesec (tm : tm) (ds : D.t list) : tm or_err =
    let (id, ds) = aux_match_id ds in
    match ds with
    | D.NUM _ :: _ as tt ->
      (* TODO id *)
      let* ttype = Type.tabletype tt in
      let table = { ttype } in
      Vector.push tm.tables table;
      Ok tm
    | [ (D.KEYWORD _ as et); D.LIST (D.KEYWORD "elem" :: fidxs) ] ->
      let n = List.length fidxs |> Int.to_string in
      let tsec = [ id; D.NUM n; D.NUM n; et ] in
      let esec = id :: D.LIST [ D.KEYWORD "i32.const"; D.NUM "0" ] :: fidxs in
      let* tm = parse_tablesec tm tsec in
      parse_elemsec tm esec
    | D.LIST [ D.KEYWORD "import"; name1; name2 ] :: tt ->
      let isec = [ name1; name2; D.LIST (D.KEYWORD "table" :: id :: tt) ] in
      parse_importsec tm isec
    | D.LIST [ D.KEYWORD "export"; name ] :: t ->
      let esec = [ name; D.LIST [ D.KEYWORD "table"; id ] ] in
      let tsec = id :: t in
      let* tm = parse_exportsec tm esec in
      parse_tablesec tm tsec
    | _ -> Error "tablesec | invalid"

  and parse_memorysec (tm : tm) (ds : D.t list) : tm or_err =
    let (id, ds) = aux_match_id ds in
    match ds with
    | [ D.NUM _ ] as l ->
      (* TODO id *)
      let* mtype = Type.memtype l in
      let t = { mtype } in
      Vector.push tm.mems t;
      Ok tm
    | [ D.NUM _; D.NUM _ ] as l ->
      (* TODO id *)
      let* mtype = Type.memtype l in
      let t = { mtype } in
      Vector.push tm.mems t;
      Ok tm
    | [ D.LIST [ D.KEYWORD "data" ] ] -> Ok tm
    | [ D.LIST [ D.KEYWORD "data"; D.STRING d ] ] ->
      let m =
        d
        |> Value.byte
        |> Bytes.length
        |> Float.of_int
        |> (fun x -> x /. 0x1_0000.)
        |> ceil
        |> Float.to_int
        |> Int.to_string
      in
      let msec = [ id; D.NUM m; D.NUM m ] in
      let dsec =
        [ id; D.LIST [ D.KEYWORD "i32.const"; D.NUM "0" ]; D.STRING d ]
      in
      let* tm = parse_memorysec tm msec in
      parse_datasec tm dsec
    | D.LIST [ D.KEYWORD "import"; name1; name2 ] :: mt ->
      let isec = [ name1; name2; D.LIST (D.KEYWORD "memory" :: id :: mt) ] in
      parse_importsec tm isec
    | D.LIST [ D.KEYWORD "export"; name ] :: mm ->
      let esec = [ name; D.LIST [ D.KEYWORD "memory"; id ] ] in
      let msec = id :: mm in
      let* tm = parse_exportsec tm esec in
      parse_memorysec tm msec
    | _ -> failwith "memorysec | invalid"

  and parse_globalsec (tm : tm) (ds : D.t list) : tm or_err =
    let (id, ds) = aux_match_id ds in
    match ds with
    | [ (D.STRING _ as gt); D.LIST e ] ->
      (* TODO id *)
      let* gtype = Type.globaltype gt in
      let* init = Instruction.expr e in
      let global = { gtype; init } in
      Vector.push tm.globals global;
      Ok tm
    | [ (D.LIST [ D.KEYWORD "mut"; D.STRING _ ] as gt); D.LIST e ] ->
      let* gtype = Type.globaltype gt in
      let* init = Instruction.expr e in
      let global = { gtype; init } in
      Vector.push tm.globals global;
      Ok tm
    | [ D.LIST [ D.KEYWORD "import"; name1; name2 ]; gt ] ->
      let isec = [ name1; name2; D.LIST [ D.KEYWORD "global"; id; gt ] ] in
      parse_importsec tm isec
    | D.LIST [ D.KEYWORD "export"; name ] :: t ->
      let esec = [ name; D.LIST [ D.KEYWORD "global"; id ] ] in
      let gsec = id :: t in
      let* tm = parse_exportsec tm esec in
      parse_globalsec tm gsec
    | _ -> Error "globalsec | invalid"

  and parse_exportsec (tm : tm) (ds : D.t list) : tm or_err =
    match ds with
    | [ D.STRING name; D.LIST desc ] ->
      let* desc =
        match desc with
        | [ D.KEYWORD "func"; t ] ->
          let* idx = Value.idx tm t in
          Ok (ED_func idx)
        | [ D.KEYWORD "table"; t ] ->
          let* idx = Value.idx tm t in
          Ok (ED_table idx)
        | [ D.KEYWORD "memory"; t ] ->
          let* idx = Value.idx tm t in
          Ok (ED_mem idx)
        | [ D.KEYWORD "global"; t ] ->
          let* idx = Value.idx tm t in
          Ok (ED_global idx)
        | _ -> Error "importsec | invalid desc"
      in
      let e = { name; desc } in
      let () = Vector.push tm.exports e in
      Ok tm
    | _ -> Error "importsec | invalid"

  and parse_startsec (tm : tm) (ds : D.t list) : tm or_err =
    match ds with
    | [ x ] ->
      let* func = Value.idx tm x in
      Ok { tm with start = Some { func } }
    | _ -> Error "startsec | invalid"

  and parse_elemsec (tm : tm) (ds : D.t list) : tm or_err =
    match ds with
    | D.LIST _ :: _ ->
      let esec = D.NUM "0" :: ds in
      parse_elemsec tm esec
    | x :: D.LIST [ D.KEYWORD "offset"; D.LIST e ] :: fidxs ->
      let* table = Value.idx tm x in
      let* offset = Instruction.expr e in
      let* init = fidxs |> List.map (Value.idx tm) |> Result.flatten_l in
      let elem = { table; offset; init } in
      Vector.push tm.elem elem;
      Ok tm
    | x :: D.LIST e :: fidxs ->
      let esec = x :: D.LIST [ D.KEYWORD "offset"; D.LIST e ] :: fidxs in
      parse_elemsec tm esec
    | _ -> Error "elemsec | invalid"

  and parse_datasec (tm : tm) (ds : D.t list) : tm or_err =
    match ds with
    | D.LIST _ :: _ ->
      let dsec = D.NUM "0" :: ds in
      parse_datasec tm dsec
    | [ x; D.LIST [ D.KEYWORD "offset"; D.LIST e ]; D.STRING b ] ->
      let* data = Value.idx tm x in
      let* offset = Instruction.expr e in
      let init = Value.byte b in
      let d = { data; offset; init } in
      Vector.push tm.data d;
      Ok tm
    | [ x; D.LIST e; b ] ->
      let dsec = [ x; D.LIST [ D.KEYWORD "offset"; D.LIST e ]; b ] in
      parse_datasec tm dsec
    | _ -> Error "datasec | invalid"

  let parse_module (d : D.t) : moduledef or_err =
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
    match d with
    | D.LIST (D.KEYWORD "module" :: t) ->
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
  let* datum = D.of_tokens tokens in
  let () = prerr_endline (D.show datum) in
  let () = prerr_newline () in
  Module.parse datum
