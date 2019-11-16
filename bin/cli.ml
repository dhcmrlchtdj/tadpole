open! Containers

module type Impl = sig
  val wat : string -> unit

  val wasm : string -> unit

  val internal : string -> unit
end

module Make (I : Impl) = struct
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
      | [ "-wat"; "-" ] -> p I.wat `Stdin
      | [ "-wat"; file ] -> p I.wat (`File file)
      | [ "-wasm"; "-" ] -> p I.wasm `Stdin
      | [ "-wasm"; file ] -> p I.wasm (`File file)
      | [ "-i"; "-" ] -> p I.internal `Stdin
      | [ "-i"; file ] -> p I.internal (`File file)
      | _ -> usage ()
    in
    aux argv
end
