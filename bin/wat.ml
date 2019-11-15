open! Containers
open Tadpole

module Wat = Cli.Make (struct
  let token s = s |> WatScanner.scan |> WatToken.to_string

  let ast s =
    s |> WatScanner.scan |> WatParser.parse |> Types.moduledef_to_string

  let wat s = s |> WatScanner.scan |> WatParser.parse |> WatPrinter.to_string

  let wasm s = s |> WatScanner.scan |> WatParser.parse |> WasmPrinter.to_string
end)

let () = Wat.run ()
