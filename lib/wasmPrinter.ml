open! Containers
open Types

let concat = String.concat ""

module Value = struct
  let u32 (x : u32) : string = x |> Int64.of_int |> Leb128.Unsigned.encode

  let i32 (x : Nint32.t) : string = x |> Int64.of_int32 |> Leb128.Signed.encode

  let i64 (x : Nint64.t) : string = x |> Leb128.Signed.encode

  let f32 (x : Nfloat32.t) : string =
    x |> Nfloat32.to_bytes_le |> Bytes.to_string

  let f64 (x : Nfloat64.t) : string =
    x |> Nfloat64.to_bytes_le |> Bytes.to_string

  let byte (x : bytes) : string =
    let xx = Bytes.to_string x in
    let size = String.length xx in
    concat [ u32 size; xx ]

  let name (x : string) : string =
    let size = String.length x in
    concat [ u32 size; x ]

  let idx = u32
end

let vec bs : string =
  let size = List.length bs in
  concat (Value.u32 size :: bs)

module Type = struct
  let valtype = function
    | TI32 -> "\x7f"
    | TI64 -> "\x7e"
    | TF32 -> "\x7d"
    | TF64 -> "\x7c"

  let mut = function
    | CONST -> "\x00"
    | VAR -> "\x01"

  let elemtype (_e : elemtype) = "\x70"

  let resulttype = function
    | [] -> "\x40"
    | [ t ] -> valtype t
    | _ -> failwith "Typ.resulttype | invalid"

  let functype ((ps, rs) : functype) =
    let pss = vec (List.map valtype ps) in
    let rss = vec (List.map valtype rs) in
    concat [ "\x60"; pss; rss ]

  let limits ({ min; max } : limits) =
    match max with
      | None -> concat [ "\x00"; Value.u32 min ]
      | Some max -> concat [ "\x01"; Value.u32 min; Value.u32 max ]

  let memtype x = limits x

  let tabletype ((l, e) : tabletype) = concat [ elemtype e; limits l ]

  let globaltype ((m, v) : globaltype) = concat [ valtype v; mut m ]
end

module Instruction = struct
  let memarg ({ align; offset } : memarg) =
    concat [ Value.u32 align; Value.u32 offset ]

  let rec expr is = concat [ instrs is; "\x0b" ]

  and instrs is =
    let rec aux acc = function
      | [] -> concat (List.rev acc)
      | h :: t -> aux (instr h :: acc) t
    in
    aux [] is

  and instr = function
    | Inumeric i -> numeric i
    | Iparametric i -> parametric i
    | Ivariable i -> variable i
    | Imemory i -> memory i
    | Icontrol i -> control i
    | Iadmin i -> admin i

  and numeric = function
    | Const (I32 v) -> concat [ "\x41"; Value.i32 v ]
    | Const (I64 v) -> concat [ "\x42"; Value.i64 v ]
    | Const (F32 v) -> concat [ "\x43"; Value.f32 v ]
    | Const (F64 v) -> concat [ "\x44"; Value.f64 v ]
    (* i32 *)
    | TestOp (TI32, I_EQZ) -> "\x45"
    | RelOp (TI32, I_EQ) -> "\x46"
    | RelOp (TI32, I_NE) -> "\x47"
    | RelOp (TI32, I_LT_S) -> "\x48"
    | RelOp (TI32, I_LT_U) -> "\x49"
    | RelOp (TI32, I_GT_S) -> "\x4a"
    | RelOp (TI32, I_GT_U) -> "\x4b"
    | RelOp (TI32, I_LE_S) -> "\x4c"
    | RelOp (TI32, I_LE_U) -> "\x4d"
    | RelOp (TI32, I_GE_S) -> "\x4e"
    | RelOp (TI32, I_GE_U) -> "\x4f"
    (* i64 *)
    | TestOp (TI64, I_EQZ) -> "\x50"
    | RelOp (TI64, I_EQ) -> "\x51"
    | RelOp (TI64, I_NE) -> "\x52"
    | RelOp (TI64, I_LT_S) -> "\x53"
    | RelOp (TI64, I_LT_U) -> "\x54"
    | RelOp (TI64, I_GT_S) -> "\x55"
    | RelOp (TI64, I_GT_U) -> "\x56"
    | RelOp (TI64, I_LE_S) -> "\x57"
    | RelOp (TI64, I_LE_U) -> "\x58"
    | RelOp (TI64, I_GE_S) -> "\x59"
    | RelOp (TI64, I_GE_U) -> "\x5a"
    (* f32 *)
    | RelOp (TF32, F_EQ) -> "\x5b"
    | RelOp (TF32, F_NE) -> "\x5c"
    | RelOp (TF32, F_LT) -> "\x5d"
    | RelOp (TF32, F_GT) -> "\x5e"
    | RelOp (TF32, F_LE) -> "\x5f"
    | RelOp (TF32, F_GE) -> "\x60"
    (* f64 *)
    | RelOp (TF64, F_EQ) -> "\x61"
    | RelOp (TF64, F_NE) -> "\x62"
    | RelOp (TF64, F_LT) -> "\x63"
    | RelOp (TF64, F_GT) -> "\x64"
    | RelOp (TF64, F_LE) -> "\x65"
    | RelOp (TF64, F_GE) -> "\x66"
    (* i32 *)
    | UnOp (TI32, I_CLZ) -> "\x67"
    | UnOp (TI32, I_CTZ) -> "\x68"
    | UnOp (TI32, I_POPCONT) -> "\x69"
    | BinOp (TI32, I_ADD) -> "\x6a"
    | BinOp (TI32, I_SUB) -> "\x6b"
    | BinOp (TI32, I_MUL) -> "\x6c"
    | BinOp (TI32, I_DIV_S) -> "\x6d"
    | BinOp (TI32, I_DIV_U) -> "\x6e"
    | BinOp (TI32, I_REM_S) -> "\x6f"
    | BinOp (TI32, I_REM_U) -> "\x70"
    | BinOp (TI32, I_AND) -> "\x71"
    | BinOp (TI32, I_OR) -> "\x72"
    | BinOp (TI32, I_XOR) -> "\x73"
    | BinOp (TI32, I_SHL) -> "\x74"
    | BinOp (TI32, I_SHR_S) -> "\x75"
    | BinOp (TI32, I_SHR_U) -> "\x76"
    | BinOp (TI32, I_ROTL) -> "\x77"
    | BinOp (TI32, I_ROTR) -> "\x78"
    (* i64 *)
    | UnOp (TI64, I_CLZ) -> "\x79"
    | UnOp (TI64, I_CTZ) -> "\x7a"
    | UnOp (TI64, I_POPCONT) -> "\x7b"
    | BinOp (TI64, I_ADD) -> "\x7c"
    | BinOp (TI64, I_SUB) -> "\x7d"
    | BinOp (TI64, I_MUL) -> "\x7e"
    | BinOp (TI64, I_DIV_S) -> "\x7f"
    | BinOp (TI64, I_DIV_U) -> "\x80"
    | BinOp (TI64, I_REM_S) -> "\x81"
    | BinOp (TI64, I_REM_U) -> "\x82"
    | BinOp (TI64, I_AND) -> "\x83"
    | BinOp (TI64, I_OR) -> "\x84"
    | BinOp (TI64, I_XOR) -> "\x85"
    | BinOp (TI64, I_SHL) -> "\x86"
    | BinOp (TI64, I_SHR_S) -> "\x87"
    | BinOp (TI64, I_SHR_U) -> "\x88"
    | BinOp (TI64, I_ROTL) -> "\x89"
    | BinOp (TI64, I_ROTR) -> "\x8a"
    (* f32 *)
    | UnOp (TF32, F_ABS) -> "\x8b"
    | UnOp (TF32, F_NEG) -> "\x8c"
    | UnOp (TF32, F_CEIL) -> "\x8d"
    | UnOp (TF32, F_FLOOR) -> "\x8e"
    | UnOp (TF32, F_TRUNC) -> "\x8f"
    | UnOp (TF32, F_NEAREST) -> "\x90"
    | UnOp (TF32, F_SQRT) -> "\x91"
    | BinOp (TF32, F_ADD) -> "\x92"
    | BinOp (TF32, F_SUB) -> "\x93"
    | BinOp (TF32, F_MUL) -> "\x94"
    | BinOp (TF32, F_DIV) -> "\x95"
    | BinOp (TF32, F_MIN) -> "\x96"
    | BinOp (TF32, F_MAX) -> "\x97"
    | BinOp (TF32, F_COPYSIGN) -> "\x98"
    (* f64 *)
    | UnOp (TF64, F_ABS) -> "\x99"
    | UnOp (TF64, F_NEG) -> "\x9a"
    | UnOp (TF64, F_CEIL) -> "\x9b"
    | UnOp (TF64, F_FLOOR) -> "\x9c"
    | UnOp (TF64, F_TRUNC) -> "\x9d"
    | UnOp (TF64, F_NEAREST) -> "\x9e"
    | UnOp (TF64, F_SQRT) -> "\x9f"
    | BinOp (TF64, F_ADD) -> "\xa0"
    | BinOp (TF64, F_SUB) -> "\xa1"
    | BinOp (TF64, F_MUL) -> "\xa2"
    | BinOp (TF64, F_DIV) -> "\xa3"
    | BinOp (TF64, F_MIN) -> "\xa4"
    | BinOp (TF64, F_MAX) -> "\xa5"
    | BinOp (TF64, F_COPYSIGN) -> "\xa6"
    (* cvt *)
    | CvtOp (TI32, CVT_WRAP, TI64) -> "\xa7"
    | CvtOp (TI32, CVT_TRUNC_S, TF32) -> "\xa8"
    | CvtOp (TI32, CVT_TRUNC_U, TF32) -> "\xa9"
    | CvtOp (TI32, CVT_TRUNC_S, TF64) -> "\xaa"
    | CvtOp (TI32, CVT_TRUNC_U, TF64) -> "\xab"
    | CvtOp (TI64, CVT_EXTEND_S, TI32) -> "\xac"
    | CvtOp (TI64, CVT_EXTEND_U, TI32) -> "\xad"
    | CvtOp (TI64, CVT_TRUNC_S, TF32) -> "\xae"
    | CvtOp (TI64, CVT_TRUNC_U, TF32) -> "\xaf"
    | CvtOp (TI64, CVT_TRUNC_S, TF64) -> "\xb0"
    | CvtOp (TI64, CVT_TRUNC_U, TF64) -> "\xb1"
    | CvtOp (TF32, CVT_CONVERT_S, TI32) -> "\xb2"
    | CvtOp (TF32, CVT_CONVERT_U, TI32) -> "\xb3"
    | CvtOp (TF32, CVT_CONVERT_S, TI64) -> "\xb4"
    | CvtOp (TF32, CVT_CONVERT_U, TI64) -> "\xb5"
    | CvtOp (TF32, CVT_DEMOTE, TF64) -> "\xb6"
    | CvtOp (TF64, CVT_CONVERT_S, TI32) -> "\xb7"
    | CvtOp (TF64, CVT_CONVERT_U, TI32) -> "\xb8"
    | CvtOp (TF64, CVT_CONVERT_S, TI64) -> "\xb9"
    | CvtOp (TF64, CVT_CONVERT_U, TI64) -> "\xba"
    | CvtOp (TF64, CVT_PROMOTE, TF32) -> "\xbb"
    | CvtOp (TI32, CVT_REINTERPRET, TF32) -> "\xbc"
    | CvtOp (TI64, CVT_REINTERPRET, TF64) -> "\xbd"
    | CvtOp (TF32, CVT_REINTERPRET, TI32) -> "\xbe"
    | CvtOp (TF64, CVT_REINTERPRET, TI64) -> "\xbf"
    | _ -> failwith "never"

  and parametric = function
    | Drop -> "\x1a"
    | Select -> "\x1b"

  and variable = function
    | LocalGet i -> concat [ "\x20"; Value.idx i ]
    | LocalSet i -> concat [ "\x21"; Value.idx i ]
    | LocalTee i -> concat [ "\x22"; Value.idx i ]
    | GlobalGet i -> concat [ "\x23"; Value.idx i ]
    | GlobalSet i -> concat [ "\x24"; Value.idx i ]

  and memory = function
    | Load (TI32, m) -> concat [ "\x28"; memarg m ]
    | Load (TI64, m) -> concat [ "\x29"; memarg m ]
    | Load (TF32, m) -> concat [ "\x2a"; memarg m ]
    | Load (TF64, m) -> concat [ "\x2b"; memarg m ]
    | Load8S (TI32, m) -> concat [ "\x2c"; memarg m ]
    | Load8U (TI32, m) -> concat [ "\x2d"; memarg m ]
    | Load16S (TI32, m) -> concat [ "\x2e"; memarg m ]
    | Load16U (TI32, m) -> concat [ "\x2f"; memarg m ]
    | Load8S (TI64, m) -> concat [ "\x30"; memarg m ]
    | Load8U (TI64, m) -> concat [ "\x31"; memarg m ]
    | Load16S (TI64, m) -> concat [ "\x32"; memarg m ]
    | Load16U (TI64, m) -> concat [ "\x33"; memarg m ]
    | Load32S (TI64, m) -> concat [ "\x34"; memarg m ]
    | Load32U (TI64, m) -> concat [ "\x35"; memarg m ]
    | Store (TI32, m) -> concat [ "\x36"; memarg m ]
    | Store (TI64, m) -> concat [ "\x37"; memarg m ]
    | Store (TF32, m) -> concat [ "\x38"; memarg m ]
    | Store (TF64, m) -> concat [ "\x39"; memarg m ]
    | Store8 (TI32, m) -> concat [ "\x3a"; memarg m ]
    | Store16 (TI32, m) -> concat [ "\x3b"; memarg m ]
    | Store8 (TI64, m) -> concat [ "\x3c"; memarg m ]
    | Store16 (TI64, m) -> concat [ "\x3d"; memarg m ]
    | Store32 (TI64, m) -> concat [ "\x3e"; memarg m ]
    | MemorySize -> concat [ "\x3F"; "\x00" ]
    | MemoryGrow -> concat [ "\x40"; "\x00" ]
    | _ -> failwith "never"

  and control = function
    | Unreachable -> "\x00"
    | Nop -> "\x01"
    | Block (r, is) -> concat [ "\x02"; Type.resulttype r; instrs is; "\x0b" ]
    | Loop (r, is) -> concat [ "\x03"; Type.resulttype r; instrs is; "\x0b" ]
    | If (r, in1, []) ->
      concat [ "\x04"; Type.resulttype r; instrs in1; "\x0b" ]
    | If (r, in1, in2) ->
      concat
        [ "\x04"; Type.resulttype r; instrs in1; "\x05"; instrs in2; "\x0b" ]
    | Br l -> concat [ "\x0c"; Value.idx l ]
    | BrIf l -> concat [ "\x0d"; Value.idx l ]
    | BrTable (larr, l) ->
      concat
        [
          "\x0e";
          vec (larr |> Array.map Value.idx |> Array.to_list);
          Value.idx l;
        ]
    | Return -> "\x0f"
    | Call i -> concat [ "\x10"; Value.idx i ]
    | CallIndirect i -> concat [ "\x11"; Value.idx i; "\x00" ]

  and admin = function
    | _ -> failwith "never"
end

module Module = struct
  let aux_section sid (mapf : 'a -> string) (arr : 'a array) : string =
    if Array.length arr = 0
    then ""
    else (
      let cont = vec (arr |> Array.map mapf |> Array.to_list) in
      let size = String.length cont in
      concat [ sid; Value.u32 size; cont ]
    )

  let typesec (m : moduledef) =
    let mapf = Type.functype in
    aux_section "\x01" mapf m.types

  let importsec (m : moduledef) =
    let importdesc = function
      | ID_func x -> concat [ "\x00"; Value.idx x ]
      | ID_table tt -> concat [ "\x01"; Type.tabletype tt ]
      | ID_mem mt -> concat [ "\x02"; Type.memtype mt ]
      | ID_global gt -> concat [ "\x03"; Type.globaltype gt ]
    in
    let mapf (i : import) =
      concat [ Value.name i.modname; Value.name i.name; importdesc i.desc ]
    in
    aux_section "\x02" mapf m.imports

  let funcsec (m : moduledef) =
    let mapf f = Value.idx f.typei in
    aux_section "\x03" mapf m.funcs

  let tablesec (m : moduledef) =
    let mapf t = Type.tabletype t.ttype in
    aux_section "\x04" mapf m.tables

  let memsec (m : moduledef) =
    let mapf m = Type.memtype m.mtype in
    aux_section "\x05" mapf m.mems

  let globalsec (m : moduledef) =
    let mapf g = Type.globaltype g.gtype in
    aux_section "\x06" mapf m.globals

  let exportsec (m : moduledef) =
    let exportdesc = function
      | ED_func i -> concat [ "\x00"; Value.idx i ]
      | ED_table i -> concat [ "\x01"; Value.idx i ]
      | ED_mem i -> concat [ "\x02"; Value.idx i ]
      | ED_global i -> concat [ "\x03"; Value.idx i ]
    in
    let mapf (e : export) = concat [ Value.name e.name; exportdesc e.desc ] in
    aux_section "\x07" mapf m.exports

  let startsec (m : moduledef) =
    match m.start with
      | None -> ""
      | Some { func } ->
        let cont = Value.idx func in
        let size = String.length cont in
        concat [ "\x08"; Value.u32 size; cont ]

  let elemsec (m : moduledef) =
    let mapf (e : elem) =
      let x = Value.idx e.table in
      let ee = Instruction.expr e.offset in
      let y = vec (List.map Value.idx e.init) in
      concat [ x; ee; y ]
    in
    aux_section "\x09" mapf m.elem

  let codesec (m : moduledef) =
    let mapf (func : func) =
      let ts =
        List.map (fun p -> concat [ Value.u32 1; Type.valtype p ]) func.locals
      in
      let t = vec ts in
      let e = Instruction.expr func.body in
      let f = concat [ t; e ] in
      let size = String.length f in
      concat [ Value.u32 size; f ]
    in
    aux_section "\x0a" mapf m.funcs

  let datasec (m : moduledef) =
    let mapf (d : data) =
      let x = Value.idx d.data in
      let e = Instruction.expr d.offset in
      let b = Value.byte d.init in
      concat [ x; e; b ]
    in
    aux_section "\x0b" mapf m.data

  let magic = "\x00\x61\x73\x6d"

  let version = "\x01\x00\x00\x00"

  let to_string (m : moduledef) =
    concat
      [
        magic;
        version;
        typesec m;
        importsec m;
        funcsec m;
        tablesec m;
        memsec m;
        globalsec m;
        exportsec m;
        startsec m;
        elemsec m;
        codesec m;
        datasec m;
      ]
end

let to_string = Module.to_string
