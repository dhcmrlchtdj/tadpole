open! Containers
open Tadpole

module Wat = Cli.Make (struct
  let wat s = s |> WatParser.parse |> WatPrinter.to_string |> print_endline

  let wasm s = s |> WatParser.parse |> WasmPrinter.to_string |> print_string

  let internal s =
    s |> WatParser.parse |> Types.moduledef_to_string |> print_endline
end)

let () = Wat.run ()
