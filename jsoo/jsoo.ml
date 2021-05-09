open! Containers
open Js_of_ocaml
open Tadpole

let of_jsString = Js.to_string

let to_jsString = Js.string

let of_jsUint8Array = Typed_array.String.of_uint8Array

let to_jsUint8Array (s : string) : Typed_array.uint8Array Js.t =
  let arr =
    s |> String.to_list |> List.map Char.code |> Array.of_list |> Js.array
  in
  new%js Typed_array.uint8Array_fromArray arr

let wrap f src =
  let m = Result.get_or_failwith (f src) in
  object%js
    method toWASM = to_jsUint8Array (WasmPrinter.to_string m)

    method toWAT = to_jsString (WatPrettyPrinter.pretty (WatPrinter.to_string m))
  end

let () =
  let obj =
    object%js
      method _WAT = wrap (fun s -> WatParser.parse (of_jsString s))

      method _WASM = wrap (fun s -> WasmParser.parse (of_jsUint8Array s))
    end
  in
  Js.export_all obj
