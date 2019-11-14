open! Containers

module type Impl = sig
  val token : string -> string

  val ast : string -> string

  val wat : string -> string

  val wasm : string -> string

  val value : string -> string
end

module Make (I : Impl) = struct
  let p tr = function
      | `Stdin -> IO.read_all stdin |> tr |> print_endline
      | `File file -> IO.File.read_exn file |> tr |> print_endline

  let run () =
      let exe = Sys.argv.(0) in
      let usage () =
          Printf.printf
            "Usage: %s [-token | -ast | -wat | -wasm] [file | -]\n"
            exe
      in
      let argv = Sys.argv |> Array.to_list |> List.tl in
      let aux = function
          | [ "-h" ] -> usage ()
          | [ "-token"; "-" ] -> p I.token `Stdin
          | [ "-token"; file ] -> p I.token (`File file)
          | [ "-ast"; "-" ] -> p I.ast `Stdin
          | [ "-ast"; file ] -> p I.ast (`File file)
          | [ "-wat"; "-" ] -> p I.wat `Stdin
          | [ "-wat"; file ] -> p I.wat (`File file)
          | [ "-wasm"; "-" ] -> p I.wasm `Stdin
          | [ "-wasm"; file ] -> p I.wasm (`File file)
          | [ "-" ] -> p I.value `Stdin
          | [ file ] -> p I.value (`File file)
          | _ -> usage ()
      in
      aux argv
end
