open! Containers

let codepoint_to_chars (codepoint : int) : char list =
    let aux = function
        | t when t < 0x80 ->
            let b1 = t land 0x8f lor 0x00 in
            [ b1 ]
        | t when t < 0x800 ->
            let b1 = (t lsr 6) land 0x1f lor 0xc0 in
            let b2 = t land 0x3f lor 0x80 in
            [ b1; b2 ]
        | t when t < 0xd800 ->
            let b1 = (t lsr 12) land 0x0f lor 0xe0 in
            let b2 = (t lsr 6) land 0x3f lor 0x80 in
            let b3 = t land 0x3f lor 0x80 in
            [ b1; b2; b3 ]
        | t when t < 0xe000 -> failwith "invalid uchar"
        | t when t < 0x110000 ->
            let b1 = (t lsr 18) land 0x07 lor 0xf0 in
            let b2 = (t lsr 12) land 0x3f lor 0x80 in
            let b3 = (t lsr 6) land 0x3f lor 0x80 in
            let b4 = t land 0x3f lor 0x80 in
            [ b1; b2; b3; b4 ]
        | _ -> failwith "invalid uchar"
    in
    aux codepoint |> List.map char_of_int


let hex_of_char_list (chars : char list) : int =
    let rec aux (acc : int) = function
        | [] -> acc
        | '0' :: t -> aux ((acc * 16) + 0) t
        | '1' :: t -> aux ((acc * 16) + 1) t
        | '2' :: t -> aux ((acc * 16) + 2) t
        | '3' :: t -> aux ((acc * 16) + 3) t
        | '4' :: t -> aux ((acc * 16) + 4) t
        | '5' :: t -> aux ((acc * 16) + 5) t
        | '6' :: t -> aux ((acc * 16) + 6) t
        | '7' :: t -> aux ((acc * 16) + 7) t
        | '8' :: t -> aux ((acc * 16) + 8) t
        | '9' :: t -> aux ((acc * 16) + 9) t
        | 'a' :: t | 'A' :: t -> aux ((acc * 16) + 10) t
        | 'b' :: t | 'B' :: t -> aux ((acc * 16) + 11) t
        | 'c' :: t | 'C' :: t -> aux ((acc * 16) + 12) t
        | 'd' :: t | 'D' :: t -> aux ((acc * 16) + 13) t
        | 'e' :: t | 'E' :: t -> aux ((acc * 16) + 14) t
        | 'f' :: t | 'F' :: t -> aux ((acc * 16) + 15) t
        | _ -> failwith "never"
    in
    aux 0 chars


let str_of_rev_char_list (chars : char list) : string =
    chars |> List.rev |> String.of_list


let num_of_string (s : string) : Token.t option =
    match Int.of_string s with
        | Some n -> Some (Token.INT n)
        | None -> (
            try
              let n = Float.of_string_exn s in
              Some (Token.FLOAT n)
            with
                | _ -> None )


let is_digit = function
    | '0' .. '9' -> true
    | _ -> false


let is_hexdigit = function
    | '0' .. '9' -> true
    | 'a' .. 'f' -> true
    | 'A' .. 'F' -> true
    | _ -> false


let is_num_char = function
    | '0' .. '9' -> true
    | 'a' .. 'f' -> true
    | 'A' .. 'F' -> true
    | '.' | '+' | '-' | 'p' | 'P' | 'x' -> true
    | _ -> false


let is_idchar = function
    | '0' .. '9' -> true
    | 'a' .. 'z' -> true
    | 'A' .. 'Z' -> true
    | '!'
    | '#'
    | '$'
    | '%'
    | '&'
    | '\''
    | '*'
    | '+'
    | '-'
    | '.'
    | '/'
    | ':'
    | '<'
    | '='
    | '>'
    | '?'
    | '@'
    | '\\'
    | '^'
    | '_'
    | '`'
    | '|'
    | '~' -> true
    | _ -> false


let is_space = function
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false


let scan (src : string) : Token.t list =
    let open Token in
    let rec scan_all_token (acc : Token.t list) (t : char list) =
        match scan_token t with
            | Ok (Some tok, tt) -> scan_all_token (tok :: acc) tt
            | Ok (None, []) -> List.rev acc
            | Ok (None, _) -> failwith "[scan_] never"
            | Error s -> failwith s
    and scan_hexdigit (acc : char list) = function
        | '_' :: h :: t when is_hexdigit h -> scan_hexdigit (h :: acc) t
        | h :: t when is_hexdigit h -> scan_hexdigit (h :: acc) t
        | t -> Ok (Some (acc |> List.rev |> hex_of_char_list), t)
    and scan_line_comment = function
        | ';' :: ';' :: t ->
            let rec aux (acc : char list) = function
                | [] -> Ok (Some (COMMENT (acc |> str_of_rev_char_list)), [])
                | '\n' :: t ->
                    Ok (Some (COMMENT (acc |> str_of_rev_char_list)), t)
                | h :: t -> aux (h :: acc) t
            in
            aux [] t
        | _ -> failwith "never"
    and scan_block_comment (t : char list) =
        let rec aux (nest_level : int) (acc : char list) = function
            | '(' :: ';' :: t -> aux (nest_level + 1) acc t
            | ';' :: ')' :: t ->
                let lv = nest_level - 1 in
                if lv = 0
                then
                  let acc2 = ')' :: ';' :: acc in
                  Ok (Some (COMMENT (acc2 |> str_of_rev_char_list)), t)
                else aux lv acc t
            | h :: t -> aux nest_level (h :: acc) t
            | [] -> failwith "never"
        in
        aux 0 [] t
    and scan_id (acc : char list) = function
        | h :: t when is_idchar h -> scan_id (h :: acc) t
        | t ->
            let r = acc |> str_of_rev_char_list in
            if String.length r = 1
            then Ok (Some (RESERVED r), t)
            else Ok (Some (ID r), t)
    and scan_keyword (acc : char list) = function
        | h :: t when is_idchar h -> scan_keyword (h :: acc) t
        | t -> Ok (Some (KEYWORD (acc |> str_of_rev_char_list)), t)
    and scan_reserved (acc : char list) = function
        | h :: t when is_idchar h -> scan_reserved (h :: acc) t
        | t -> Ok (Some (RESERVED (acc |> str_of_rev_char_list)), t)
    and scan_string (acc : char list) = function
        (* TODO https://webassembly.github.io/spec/core/text/values.html#strings *)
        | '\\' :: n :: m :: t when is_hexdigit n && is_hexdigit m ->
            let c = [ n; m ] |> hex_of_char_list |> Char.chr in
            scan_string (c :: acc) t
        | '\\' :: 'u' :: t -> (
            let is_valid hex =
                hex < 0xd800 || (hex >= 0xe000 && hex < 0x110000)
            in
            match scan_hexdigit [] t with
                | Ok (Some codepoint, tt) when is_valid codepoint ->
                    let chs = codepoint_to_chars codepoint in
                    let chs_rev = List.rev chs in
                    scan_string (chs_rev @ acc) tt
                | _ -> failwith "[scan_string] invalid codepoint" )
        | '\\' :: 't' :: t -> scan_string ('\t' :: acc) t
        | '\\' :: 'n' :: t -> scan_string ('\n' :: acc) t
        | '\\' :: 'r' :: t -> scan_string ('\r' :: acc) t
        | '\\' :: '"' :: t -> scan_string ('"' :: acc) t
        | '\\' :: '\'' :: t -> scan_string ('\'' :: acc) t
        | '\\' :: '\\' :: t -> scan_string ('\\' :: acc) t
        | '\\' :: _ -> failwith "[scan_string] invalid slash"
        | '"' :: t -> Ok (Some (STRING (acc |> str_of_rev_char_list)), t)
        | h :: t -> scan_string (h :: acc) t
        | [] -> failwith "[scan_string] unexpected eof"
    and scan_num (t : char list) =
        let rec aux (acc : char list) = function
            | '_' :: t -> aux acc t
            | h :: t when is_num_char h -> aux (h :: acc) t
            | t -> (
                let s = str_of_rev_char_list acc in
                match num_of_string s with
                    | Some n -> Ok (Some n, t)
                    | None -> Ok (Some (RESERVED s), t) )
        in
        let (acc, tt) =
            match t with
                | '+' :: tt -> ([ '+' ], tt)
                | '-' :: tt -> ([ '-' ], tt)
                | tt -> ([ '+' ], tt)
        in
        aux acc tt
    and scan_token = function
        | [] -> Ok (None, [])
        (* remove space *)
        | h :: t when is_space h -> scan_token t
        (* comment *)
        | '(' :: ';' :: _ as t -> scan_block_comment t
        | ';' :: ';' :: _ as t -> scan_line_comment t
        (* parenthesis *)
        | '(' :: t -> Ok (Some LEFT_PAREN, t)
        | ')' :: t -> Ok (Some RIGHT_PAREN, t)
        (* id or reserved *)
        | '$' :: t -> scan_id [ '$' ] t
        (* string *)
        | '"' :: t -> scan_string [] t
        (* int or float *)
        | ('+' :: h :: _ | '-' :: h :: _) as t when is_digit h -> scan_num t
        | h :: _ as t when is_digit h -> scan_num t
        (* keyword or float *)
        | 'a' .. 'z' :: _ as t -> (
            match scan_keyword [] t with
                | Ok (Some (KEYWORD k), tt) when String.equal k "inf" ->
                    Ok (Some (FLOAT infinity), tt)
                | Ok (Some (KEYWORD k), tt) when String.equal k "nan" ->
                    Ok (Some (FLOAT nan), tt)
                | s -> s )
        (* reserved *)
        | h :: t when is_idchar h -> scan_reserved [ h ] t
        | _ -> failwith "never"
    in
    scan_all_token [] (String.to_list src)
