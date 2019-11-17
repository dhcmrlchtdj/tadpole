open! Containers
module D = WatDatum

let sprintf = Printf.sprintf

module Document = struct
  type doc =
    | Newline of int
    | Text of string
    | Group of doc
    | Concat of doc list

  let max_width = 80

  let to_string =
    let rec aux used = function
      | Newline i -> sprintf "\n%s" (String.repeat " " i)
      | Text s -> s
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
      | Newline _ -> " "
      | Text s -> s
      | Group d -> to_string_group d
      | Concat ds -> List.map to_string_group ds |> String.concat ""
    in
    aux 0

  let of_datum =
    let rec aux = function
      | D.NUM s -> Text s
      | D.STRING s -> Text (sprintf "%S" s)
      | D.ID s -> Text s
      | D.KEYWORD s -> Text s
      | D.RESERVED s -> Text s
      | D.LIST t ->
        let sub =
          let doc = List.map aux t in
          let len = List.length doc - 1 in
          doc
          |> List.mapi (fun i x -> if i = len then [ x ] else [ x; Newline 0 ])
          |> List.flatten
          |> List.map add_indent
          |> fun x -> Concat x
        in
        let doc = Concat [ Text "("; sub; Text ")" ] |> simplify in
        Group (Concat doc)
    and add_indent = function
      | Newline i -> Newline (i + 4)
      | Text s -> Text s
      | Group d -> Group (add_indent d)
      | Concat ds -> Concat (List.map add_indent ds)
    and simplify = function
      | Concat doc -> doc |> List.map simplify |> List.flatten
      | x -> [ x ]
    in
    aux
end

let pretty (src : string) : string =
  src
  |> WatScanner.scan
  |> Result.get_or_failwith
  |> WatDatum.of_tokens
  |> Result.get_or_failwith
  |> Document.of_datum
  |> Document.to_string
