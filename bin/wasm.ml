open! Containers
open Tadpole

module Wasm = Cli.Make (struct
  let token _s = failwith "WASM | there is no token"

  let ast s =
    s |> WasmParser.parse |> Types.moduledef_to_string |> print_endline

  let wat s = s |> WasmParser.parse |> WatPrinter.to_string |> print_endline

  let wasm s = s |> WasmParser.parse |> WasmPrinter.to_string |> print_string
end)

let () = Wasm.run ()
