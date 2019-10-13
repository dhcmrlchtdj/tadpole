open! Containers

module Doc = struct
    let sprintf = Printf.sprintf

    type doc =
        | Newline of int
        | Text of string
        | Group of doc
        | Concat of doc list

    let max_width = 80

    let indent_size = 4

    let to_string =
        let rec aux used = function
            | Text s -> s
            | Newline i -> sprintf "\n%s" (String.repeat " " i)
            | Group d ->
                let s = to_string_group d in
                let len = String.length s in
                if len + used <= max_width then s else aux used d
            | Concat ds ->
                let (_, dss) =
                    List.fold_left
                        (fun (w, prev) next ->
                                    let s = aux w next in
                                    let r =
                                        match next with
                                            | Newline i -> i
                                            | _ -> w + String.length s
                                    in
                                    (r, s :: prev))
                        (used, [])
                        ds
                in
                dss |> List.rev |> String.concat ""
        and to_string_group = function
            | Text s -> s
            | Newline _ -> " "
            | Group d -> to_string_group d
            | Concat ds -> List.map to_string_group ds |> String.concat ""
        in
        aux 0


    let of_datum =
        let open Datum in
        let rec aux = function
            | INT x -> Text (sprintf "%d" x)
            | FLOAT x -> Text (sprintf "%f" x)
            | STRING x -> Text (sprintf "%S" x)
            | ID x -> Text (sprintf "%s" x)
            | KEYWORD x -> Text (sprintf "%s" x)
            | RESERVED x -> Text (sprintf "%s" x)
            | LIST ds ->
                let sub =
                    let doc = List.map aux ds in
                    let len = List.length doc - 1 in
                    doc
                    |> List.mapi (fun i x ->
                        if i = len then [x] else [x; Newline 0])
                    |> List.flatten
                    |> List.map add_indent
                    |> fun x -> Concat x
                in
                let doc = Concat [Text "("; sub; Text ")"] |> simplify in
                Group (Concat doc)
        and add_indent = function
            | Group d -> Group (add_indent d)
            | Concat ds -> ds |> List.map add_indent |> fun x -> Concat x
            | Newline i -> Newline (i + indent_size)
            | Text s -> Text s
        and simplify = function
            | Concat doc -> doc |> List.map simplify |> List.flatten
            | x -> [x]
        in
        aux


    let pretty datum = datum |> of_datum |> to_string
end

let to_string (datums : Datum.t list) : string =
    datums |> List.map Doc.pretty |> String.concat "\n"
