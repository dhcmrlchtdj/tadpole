open! Containers
open Js_of_ocaml
open Tadpole

let wrap parse s =
  let src = Js.to_string s in
  object%js
    method toWASM =
      src
      |> parse
      |> Result.get_or_failwith
      |> WasmPrinter.to_string
      |> Js.string

    method toWAT =
      src
      |> parse
      |> Result.get_or_failwith
      |> WatPrinter.to_string
      |> WatPrettyPrinter.pretty
      |> Js.string
  end

let () =
  let obj =
    object%js
      method _WAT src = wrap WatParser.parse src

      method _WASM src = wrap WasmParser.parse src
    end
  in
  Js.export_all obj
