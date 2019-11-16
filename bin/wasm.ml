open! Containers
open Tadpole

module Wasm = Cli.Make (struct
  let wat s = s |> WasmParser.parse |> WatPrinter.to_string |> print_endline

  let wasm s = s |> WasmParser.parse |> WasmPrinter.to_string |> print_string

  let internal s =
    s |> WasmParser.parse |> Types.moduledef_to_string |> print_endline
end)

let () = Wasm.run ()
