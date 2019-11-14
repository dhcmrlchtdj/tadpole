open! Containers
open Tadpole

module Wasm = Cli.Make (struct
  let token _s = failwith "WASM | there is no token"

  let ast s = s |> WasmParser.parse |> Types.moduledef_to_string

  let wat s = s |> WasmParser.parse |> WatPrinter.to_string

  let wasm s = s |> WasmParser.parse |> WasmPrinter.to_string

  let value s = s |> WasmParser.parse |> Evaluator.eval
end)

let () = Wasm.run ()
