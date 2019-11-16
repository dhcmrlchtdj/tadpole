open! Containers
open Tadpole
module Wasm = Cli.Make (WasmParser)

let () = Wasm.run ()
