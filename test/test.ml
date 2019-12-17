open! Containers
open Tadpole

let ( let* ) = Result.( let* )

let _ =
  let wasm =
    "\x00\x61\x73\x6d\x01\x00\x00\x00\x01\x07\x01\x60\x02\x7f\x7f\x01\x7f\x03\x02\x01\x00\x07\x0a\x01\x06\x61\x64\x64\x54\x77\x6f\x00\x00\x0a\x09\x01\x07\x00\x20\x00\x20\x01\x6a\x0b"
  in
  let* m = WasmParser.parse wasm in
  let s = Types.moduledef_to_string m in
  let () = Printf.printf "%s\n" s in
  Ok ()
