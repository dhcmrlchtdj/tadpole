open! Containers
open Tadpole

module type Scanner = sig
  val scan : string -> Token.t list
end

module Make (Scanner : Scanner) = struct
  let p f = function
      | `Stdin -> IO.read_all stdin |> f
      | `File file -> IO.File.read_exn file |> f


  let print_token =
      p (fun s ->
          s |> Scanner.scan |> List.map Token.show |> List.iter print_endline)


  let print_ast =
      p (fun s ->
          s
          |> Scanner.scan
          |> Parser.parse
          |> List.map Datum.show
          |> List.iter print_endline)


  let print_wat =
      p (fun s ->
          s
          |> Scanner.scan
          |> Parser.parse
          |> WatPrinter.to_string
          |> print_endline)


  let print_wasm =
      p (fun s ->
          s
          |> Scanner.scan
          |> Parser.parse
          |> fun _ -> "" (* |> WasmPrinter.to_string *) |> print_endline)


  let print_val =
      p (fun s ->
          s
          |> Scanner.scan
          |> Parser.parse
          |> Evaluator.eval
          |> fun _ -> "" (* |> Evaluator.show_value *) |> print_endline)


  let main () =
      let exe = Sys.argv.(0) in
      let usage () =
          Printf.printf
            "Usage: %s [-token | -ast | -wat | -wasm] [file | -]\n"
            exe
      in
      let argv = Sys.argv |> Array.to_list |> List.tl in
      let aux = function
          | [ "-h" ] -> usage ()
          | [ "-token"; "-" ] -> print_token `Stdin
          | [ "-token"; file ] -> print_token (`File file)
          | [ "-ast"; "-" ] -> print_ast `Stdin
          | [ "-ast"; file ] -> print_ast (`File file)
          | [ "-wat"; "-" ] -> print_wat `Stdin
          | [ "-wat"; file ] -> print_wat (`File file)
          | [ "-wasm"; "-" ] -> print_wasm `Stdin
          | [ "-wasm"; file ] -> print_wasm (`File file)
          | [ "-" ] -> print_val `Stdin
          | [ file ] -> print_val (`File file)
          | _ -> usage ()
      in
      aux argv
end
