open! Containers
module D = WatDatum

let sprintf = Printf.sprintf

module Document = struct
  type t =
    | Newline of int
    | Text of string
    | Group of t list

  let to_string max_width indent_size =
    let rec aux used = function
      | Newline i -> sprintf "\n%s" (String.repeat " " (i * indent_size))
      | Text s -> s
      | Group ds as g ->
        let s = oneline g in
        let len = String.length s in
        if len + used <= max_width then s else multiline used ds
    and oneline = function
      | Newline _ -> " "
      | Text s -> s
      | Group ds -> ds |> List.map oneline |> String.concat ""
    and multiline used ds =
      let (_, dss) =
        let f ((prev_used, docs) : int * string list) (doc : t) =
          let s = aux prev_used doc in
          let next_used =
            match doc with
            | Newline i -> i * indent_size
            | _ -> prev_used + String.length s
          in
          (next_used, s :: docs)
        in
        List.fold_left f (used, []) ds
      in
      dss |> List.rev |> String.concat ""
    in
    aux 0

  let of_datum =
    let rec add_indent = function
      | Newline i -> Newline (i + 1)
      | Text s -> Text s
      | Group ds -> Group (List.map add_indent ds)
    in
    let left_paren = [ Text "(" ] in
    let right_paren = [ Text ")" ] in
    let group docs =
      let sub = List.map add_indent docs in
      List.flatten [ left_paren; sub; right_paren ]
    in
    let rec aux = function
      | D.NUM s -> Text s
      | D.STRING s -> Text (sprintf "%S" s)
      | D.ID s -> Text s
      | D.KEYWORD s -> Text s
      | D.RESERVED s -> Text s
      | D.LIST ds ->
        let docs =
          ds
          |> List.mapi (fun i x ->
                 let x = aux x in
                 if i = 0 then [ x ] else [ Newline 0; x ])
          |> List.flatten
          |> group
        in
        Group docs
    in
    aux
end

let pretty ?(max_width = 80) ?(indent_size = 4) (src : string) : string =
  src
  |> WatScanner.scan
  |> Result.get_or_failwith
  |> WatDatum.of_tokens
  |> Result.get_or_failwith
  |> Document.of_datum
  |> Document.to_string max_width indent_size
