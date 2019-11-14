open! Containers
open Types
module R = Result

type p_context = {
    p: int;
    s: string;
    slen: int;
  }

type 'a orErr = ('a, string) result

let aux_read (len : int) (ctx : p_context) (f : string -> p_context -> 'a orErr)
    : 'a orErr =
    let { p; s; slen } = ctx in
    if p + len < slen
    then (
      let c = String.sub s p len in
      f c { ctx with p = p + len }
    )
    else Error "EOF"

module Value = struct
  let read_uint _ctx : (int * p_context) orErr = failwith "TODO"
end

module Type = struct end

module Instruction = struct end

module Module = struct
  let magic = "\x00\x61\x73\x6d"

  let version = "\x01\x00\x00\x00"

  type m_ctx = moduledef * p_context

  type m_ctx_result = m_ctx orErr

  let aux_section (sid : char) (ctx : p_context) : (string * p_context) orErr =
      let { p; s; slen } = ctx in
      let ch = s.[p] in
      if Char.equal ch sid
      then Ok ("", ctx)
      else
        Ok { p = p + 1; s; slen }
        |> R.flat_map Value.read_uint
        |> R.flat_map (fun (size, ctx) ->
               aux_read size ctx (fun c ctx -> Ok (c, ctx)))

  let parse_customsec ((m, ctx) : m_ctx) : m_ctx_result =
      let sec = aux_section '\x00' ctx in
      R.map (fun (_, ctx) -> (m, ctx)) sec

  let parse_functype ((_m, _ctx) : m_ctx) : m_ctx_result = failwith "TODO"

  let parse_import ((_m, _ctx) : m_ctx) : m_ctx_result = failwith "TODO"

  let parse_typeidx ((_m, _ctx) : m_ctx) : m_ctx_result = failwith "TODO"

  let parse_table ((_m, _ctx) : m_ctx) : m_ctx_result = failwith "TODO"

  let parse_mem ((_m, _ctx) : m_ctx) : m_ctx_result = failwith "TODO"

  let parse_global ((_m, _ctx) : m_ctx) : m_ctx_result = failwith "TODO"

  let parse_export ((_m, _ctx) : m_ctx) : m_ctx_result = failwith "TODO"

  let parse_start ((_m, _ctx) : m_ctx) : m_ctx_result = failwith "TODO"

  let parse_elem ((_m, _ctx) : m_ctx) : m_ctx_result = failwith "TODO"

  let parse_code ((_m, _ctx) : m_ctx) : m_ctx_result = failwith "TODO"

  let parse_data ((_m, _ctx) : m_ctx) : m_ctx_result = failwith "TODO"

  let parse_module (ctx : p_context) : m_ctx_result =
      let consume_magic (ctx : p_context) : p_context orErr =
          aux_read 4 ctx (fun c ctx ->
              if String.equal c magic then Ok ctx else Error "magic error")
      in
      let consume_version (ctx : p_context) : p_context orErr =
          aux_read 4 ctx (fun c ctx ->
              if String.equal c version then Ok ctx else Error "version error")
      in
      let ctx =
          Ok ctx |> R.flat_map consume_magic |> R.flat_map consume_version
      in
      let m =
          {
            types = [||];
            funcs = [||];
            tables = [||];
            mems = [||];
            globals = [||];
            elem = [||];
            data = [||];
            start = None;
            imports = [||];
            exports = [||];
          }
      in
      let mctx = R.map (fun ctx -> (m, ctx)) ctx in
      mctx
      |> R.flat_map parse_customsec
      |> R.flat_map parse_functype
      |> R.flat_map parse_customsec
      |> R.flat_map parse_import
      |> R.flat_map parse_customsec
      |> R.flat_map parse_typeidx
      |> R.flat_map parse_customsec
      |> R.flat_map parse_table
      |> R.flat_map parse_customsec
      |> R.flat_map parse_mem
      |> R.flat_map parse_customsec
      |> R.flat_map parse_global
      |> R.flat_map parse_customsec
      |> R.flat_map parse_export
      |> R.flat_map parse_customsec
      |> R.flat_map parse_start
      |> R.flat_map parse_customsec
      |> R.flat_map parse_elem
      |> R.flat_map parse_customsec
      |> R.flat_map parse_code
      |> R.flat_map parse_customsec
      |> R.flat_map parse_data
      |> R.flat_map parse_customsec

  let parse (p : int) (s : string) : moduledef =
      let ctx : p_context = { p; s; slen = String.length s } in
      Ok ctx
      |> R.flat_map parse_module
      |> R.flat_map (fun (m, { p; slen; _ }) ->
             if p = slen then Ok m else Error "EOF")
      |> R.get_or_failwith
end

let parse = Module.parse 0
