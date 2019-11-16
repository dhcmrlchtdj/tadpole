open! Containers
open Types

let sprintf = Printf.sprintf

let concat = String.concat " "

let add_space s = if String.length s = 0 then s else " " ^ s

module Value = struct
  let u32 (x : u32) : string = Int.to_string x

  let i32 (x : Nint32.t) : string = Nint32.to_string x

  let i64 (x : Nint64.t) : string = Nint64.to_string x

  let f32 (x : Nfloat32.t) : string = Nfloat32.to_string x

  let f64 (x : Nfloat64.t) : string = Nfloat64.to_string x

  let byte (x : bytes) : string = Bytes.to_string x

  let name (x : string) : string = x

  let idx = u32
end

module Type = struct
  let valtype = function
    | TI32 -> "i32"
    | TI64 -> "i64"
    | TF32 -> "f32"
    | TF64 -> "f64"

  let elemtype (_e : elemtype) = "funcref"

  let resulttype = function
    | [] -> ""
    | [ t ] -> sprintf "(%s)" (valtype t)
    | _ -> failwith "Typ.resulttype | invalid"

  let functype ((p, r) : functype) =
    let ps =
      match p with
        | [] -> ""
        | _ ->
          p |> List.map (fun s -> sprintf "(param %s)" (valtype s)) |> concat
    and rs =
      match r with
        | [] -> ""
        | _ ->
          r |> List.map (fun s -> sprintf "(result %s)" (valtype s)) |> concat
    in
    sprintf "(func%s%s)" (add_space ps) (add_space rs)

  let limits ({ min; max } : limits) =
    match max with
      | None -> Value.u32 min
      | Some max -> sprintf "%s %s" (Value.u32 min) (Value.u32 max)

  let memtype x = limits x

  let tabletype ((l, e) : tabletype) = sprintf "%s %s" (limits l) (elemtype e)

  let globaltype ((m, v) : globaltype) =
    let v = valtype v in
    match m with
      | CONST -> v
      | VAR -> sprintf "(mut %s)" v
end

module Instruction = struct
  let memarg ({ align; offset } : memarg) = failwith "TODO"

  let rec expr is = failwith "TODO"

  and instrs is = failwith "TODO"

  and instr = function
    | Inumeric i -> numeric i
    | Iparametric i -> parametric i
    | Ivariable i -> variable i
    | Imemory i -> memory i
    | Icontrol i -> control i
    | Iadmin i -> admin i

  and numeric = failwith "TODO"

  and parametric = failwith "TODO"

  and variable = failwith "TODO"

  and memory = failwith "TODO"

  and control = failwith "TODO"

  and admin = function
    | _ -> failwith "never"
end

module Module = struct
  let aux_typeuse (idx : idx) = failwith "TODO"

  let aux_section (mapf : 'a -> string) (arr : 'a array) : string =
    arr |> Array.map mapf |> Array.to_list |> concat

  let typesec (m : moduledef) =
    let mapf ft = sprintf "(type %s)" (Type.functype ft) in
    aux_section mapf m.types

  let importsec (m : moduledef) =
    let mapf i =
      let modname = i.modname in
      let name = i.name in
      let desc =
        match i.desc with
          | ID_func x -> sprintf "(func %s)" (aux_typeuse x)
          | ID_table t -> sprintf "(table %s)" (Type.tabletype t)
          | ID_mem t -> sprintf "(memory %s)" (Type.memtype t)
          | ID_global t -> sprintf "(global %s)" (Type.globaltype t)
      in
      sprintf "(import %s %s %s)" modname name desc
    in
    aux_section mapf m.imports

  let funcsec (m : moduledef) =
    let mapf (f : func) =
      let t = aux_typeuse f.typei in
      let l =
        f.locals
        |> List.map (fun t -> sprintf "(local %s)" (Type.valtype t))
        |> concat
      in
      let b = Instruction.expr f.body in
      sprintf "(func%s%s%s)" t l b
    in
    aux_section mapf m.funcs

  let tablesec (m : moduledef) =
    let mapf t = sprintf "(table %s)" (Type.tabletype t.ttype) in
    aux_section mapf m.tables

  let memsec (m : moduledef) =
    let mapf m = sprintf "(memory %s)" (Type.memtype m.mtype) in
    aux_section mapf m.mems

  let globalsec (m : moduledef) =
    let mapf g =
      sprintf
        "(global %s %s)"
        (Type.globaltype g.gtype)
        (Instruction.expr g.init)
    in
    aux_section mapf m.globals

  let exportsec (m : moduledef) =
    let mapf (e : export) =
      let name = e.name in
      let desc =
        match e.desc with
          | ED_func i -> sprintf "(func %s)" (Value.idx i)
          | ED_table i -> sprintf "(table %s)" (Value.idx i)
          | ED_mem i -> sprintf "(memory %s)" (Value.idx i)
          | ED_global i -> sprintf "(global %s)" (Value.idx i)
      in
      sprintf "(export %s %s)" name desc
    in
    aux_section mapf m.exports

  let startsec (m : moduledef) =
    match m.start with
      | None -> ""
      | Some { func } -> sprintf "(start %s)" (Value.idx func)

  let elemsec (m : moduledef) =
    let mapf (e : elem) =
      let x = Value.idx e.table in
      let o = Instruction.expr e.offset in
      let i = e.init |> List.map Value.idx |> concat in
      sprintf "(elem %s (offset %s) %s)" x o i
    in
    aux_section mapf m.elem

  let datasec (m : moduledef) =
    let mapf (d : data) =
      let x = Value.idx d.data in
      let o = Instruction.expr d.offset in
      let i = Value.byte d.init in
      sprintf "(data %s (offset %s) %s)" x o i
    in
    aux_section mapf m.data

  let to_string (m : moduledef) : string =
    let fields =
      [
        typesec m;
        importsec m;
        funcsec m;
        tablesec m;
        memsec m;
        globalsec m;
        exportsec m;
        startsec m;
        elemsec m;
        datasec m;
      ]
    in
    let fields =
      fields |> List.filter (fun s -> String.length s > 0) |> concat
    in
    sprintf "(module%s)" (add_space fields)
end

let to_string = Module.to_string
