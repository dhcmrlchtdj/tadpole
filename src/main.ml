open! Containers

let p f = function
    | `Stdin -> IO.read_all stdin |> f |> ignore
    | `File file -> IO.File.read_exn file |> f |> ignore


let print_token =
    p (fun s ->
        s |> WatScanner.scan |> List.map Token.show |> List.map print_endline)


let print_ast =
    p (fun s ->
        s
        |> WatScanner.scan
        |> Parser.parse
        |> List.map Datum.show
        |> List.map print_endline)


let () =
    let exe = Sys.argv.(0) in
    let usage () =
        Printf.printf "Usage: %s [-token | -ast] [file | -]\n" exe
    in
    let argv = Sys.argv |> Array.to_list |> List.tl in
    let aux = function
        | ["-h"] -> usage ()
        | ["-token"; "-"] -> print_token `Stdin
        | ["-token"; file] -> print_token (`File file)
        | ["-ast"; "-"] -> print_ast `Stdin
        | ["-ast"; file] -> print_ast (`File file)
        (* | [ "-wat"; "-" ] -> print_wat `Stdin *)
        (* | [ "-wat"; file ] -> print_wat (`File file) *)
        (* | [ "-" ] -> print_val `Stdin *)
        (* | [ file ] -> print_val (`File file) *)
        | _ -> usage ()
    in
    aux argv
