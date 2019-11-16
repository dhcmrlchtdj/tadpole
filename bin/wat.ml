open! Containers
open Tadpole

module Wat = Cli.Make (struct
  let token s = s |> WatScanner.scan |> WatToken.to_string |> print_endline

  let ast s =
    s
    |> WatScanner.scan
    |> WatParser.parse
    |> Types.moduledef_to_string
    |> print_endline

  let wat s =
    s
    |> WatScanner.scan
    |> WatParser.parse
    |> WatPrinter.to_string
    |> print_endline

  let wasm s =
    s
    |> WatScanner.scan
    |> WatParser.parse
    |> WasmPrinter.to_string
    |> print_string
end)

let () = Wat.run ()
