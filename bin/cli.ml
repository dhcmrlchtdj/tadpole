open! Containers
open Tadpole

module type Impl = sig
  val parse : string -> (Types.moduledef, string) result
end

module Make (I : Impl) = struct
  let to_wasm s =
    s
    |> I.parse
    |> Result.get_or_failwith
    |> WasmPrinter.to_string
    |> print_string

  let to_wat s =
    s
    |> I.parse
    |> Result.get_or_failwith
    |> WatPrinter.to_string
    |> WatPrettyPrinter.pretty
    |> print_endline

  let to_internal s =
    s
    |> I.parse
    |> Result.get_or_failwith
    |> Types.moduledef_to_string
    |> print_endline

  let p tr = function
    | `Stdin -> IO.read_all stdin |> tr
    | `File file -> IO.File.read_exn file |> tr

  let run () =
    let exe = Sys.argv.(0) in
    let usage () =
      Printf.printf "Usage: %s [-wat | -wasm | -i] [file | -]\n" exe
    in
    let argv = Sys.argv |> Array.to_list |> List.tl in
    let aux = function
      | [ "-h" ] -> usage ()
      | [ "-wat"; "-" ] -> p to_wat `Stdin
      | [ "-wat"; file ] -> p to_wat (`File file)
      | [ "-wasm"; "-" ] -> p to_wasm `Stdin
      | [ "-wasm"; file ] -> p to_wasm (`File file)
      | [ "-i"; "-" ] -> p to_internal `Stdin
      | [ "-i"; file ] -> p to_internal (`File file)
      | _ -> usage ()
    in
    aux argv
end
