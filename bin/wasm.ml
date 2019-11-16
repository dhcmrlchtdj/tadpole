open! Containers
open Tadpole

module Wasm = Cli.Make (struct
  let wat s =
    s
    |> WasmParser.parse
    |> Result.get_or_failwith
    |> WatPrinter.to_string
    |> print_endline

  let wasm s =
    s
    |> WasmParser.parse
    |> Result.get_or_failwith
    |> WasmPrinter.to_string
    |> print_string

  let internal s =
    s
    |> WasmParser.parse
    |> Result.get_or_failwith
    |> Types.moduledef_to_string
    |> print_endline
end)

let () = Wasm.run ()
