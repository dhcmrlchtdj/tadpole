open! Containers

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


let int_of_char_list (chars : char list) : int =
    let rec aux (acc : int) = function
        | [] -> acc
        | '0' :: t -> aux ((acc * 10) + 0) t
        | '1' :: t -> aux ((acc * 10) + 1) t
        | '2' :: t -> aux ((acc * 10) + 2) t
        | '3' :: t -> aux ((acc * 10) + 3) t
        | '4' :: t -> aux ((acc * 10) + 4) t
        | '5' :: t -> aux ((acc * 10) + 5) t
        | '6' :: t -> aux ((acc * 10) + 6) t
        | '7' :: t -> aux ((acc * 10) + 7) t
        | '8' :: t -> aux ((acc * 10) + 8) t
        | '9' :: t -> aux ((acc * 10) + 9) t
        | _ -> failwith "never"
    in
    aux 0 chars


let frac_hex_of_char_list (chars : char list) : float =
    let rec aux (acc : float) = function
        | [] -> acc
        | '0' :: t -> aux ((acc +. 0.) /. 16.) t
        | '1' :: t -> aux ((acc +. 1.) /. 16.) t
        | '2' :: t -> aux ((acc +. 2.) /. 16.) t
        | '3' :: t -> aux ((acc +. 3.) /. 16.) t
        | '4' :: t -> aux ((acc +. 4.) /. 16.) t
        | '5' :: t -> aux ((acc +. 5.) /. 16.) t
        | '6' :: t -> aux ((acc +. 6.) /. 16.) t
        | '7' :: t -> aux ((acc +. 7.) /. 16.) t
        | '8' :: t -> aux ((acc +. 8.) /. 16.) t
        | '9' :: t -> aux ((acc +. 9.) /. 16.) t
        | 'a' :: t | 'A' :: t -> aux ((acc +. 10.) /. 16.) t
        | 'b' :: t | 'B' :: t -> aux ((acc +. 11.) /. 16.) t
        | 'c' :: t | 'C' :: t -> aux ((acc +. 12.) /. 16.) t
        | 'd' :: t | 'D' :: t -> aux ((acc +. 13.) /. 16.) t
        | 'e' :: t | 'E' :: t -> aux ((acc +. 14.) /. 16.) t
        | 'f' :: t | 'F' :: t -> aux ((acc +. 15.) /. 16.) t
        | _ -> failwith "never"
    in
    aux 0. chars


let frac_int_of_char_list (chars : char list) : float =
    let rec aux (acc : float) = function
        | [] -> acc
        | '0' :: t -> aux ((acc +. 0.) /. 10.) t
        | '1' :: t -> aux ((acc +. 1.) /. 10.) t
        | '2' :: t -> aux ((acc +. 2.) /. 10.) t
        | '3' :: t -> aux ((acc +. 3.) /. 10.) t
        | '4' :: t -> aux ((acc +. 4.) /. 10.) t
        | '5' :: t -> aux ((acc +. 5.) /. 10.) t
        | '6' :: t -> aux ((acc +. 6.) /. 10.) t
        | '7' :: t -> aux ((acc +. 7.) /. 10.) t
        | '8' :: t -> aux ((acc +. 8.) /. 10.) t
        | '9' :: t -> aux ((acc +. 9.) /. 10.) t
        | _ -> failwith "never"
    in
    aux 0. chars


let str_of_rev_list (chars : char list) : string =
    chars |> List.rev |> String.of_list


let is_digit = function '0' .. '9' -> true | _ -> false

let is_hexdigit = function
    | '0' .. '9' -> true
    | 'a' .. 'z' -> true
    | 'A' .. 'Z' -> true
    | _ -> false


let is_idchar = function
    | '0' .. '9' -> true
    | 'a' .. 'z' -> true
    | 'A' .. 'Z' -> true
    | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '/' | ':'
    |'<' | '=' | '>' | '?' | '@' | '\\' | '^' | '_' | '`' | '|' | '~' ->
        true
    | _ -> false


let is_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

let scan (src : string) : WatToken.t list =
    let open WatToken in
    let rec scan_ (acc : WatToken.t list) (t : char list) =
        match scan_token t with
            | Ok (Some tok, tt) -> scan_ (tok :: acc) tt
            | Ok (None, []) -> List.rev acc
            | Ok (None, _) -> failwith "[scan_] never"
            | Error s -> failwith s
    and scan_hexdigit (acc : char list) = function
        | '_' :: h :: t when is_hexdigit h -> scan_hexdigit (h :: acc) t
        | h :: t when is_hexdigit h -> scan_hexdigit (h :: acc) t
        | t -> Ok (Some (acc |> List.rev |> hex_of_char_list), t)
    and scan_digit (acc : char list) = function
        | '_' :: h :: t when is_digit h -> scan_digit (h :: acc) t
        | h :: t when is_digit h -> scan_digit (h :: acc) t
        | t -> Ok (Some (acc |> List.rev |> int_of_char_list), t)
    and scan_hexdigit_frac (acc : char list) = function
        | '_' :: h :: t when is_hexdigit h -> scan_hexdigit_frac (h :: acc) t
        | h :: t when is_hexdigit h -> scan_hexdigit_frac (h :: acc) t
        | t -> Ok (Some (acc |> frac_hex_of_char_list), t)
    and scan_digit_frac (acc : char list) = function
        | '_' :: h :: t when is_digit h -> scan_digit_frac (h :: acc) t
        | h :: t when is_digit h -> scan_digit_frac (h :: acc) t
        | t -> Ok (Some (acc |> frac_int_of_char_list), t)
    and scan_line_comment = function
        | ';' :: ';' :: t ->
            let rec aux (acc : char list) = function
                | [] -> Ok (Some (COMMENT (acc |> str_of_rev_list)), [])
                | '\n' :: t -> Ok (Some (COMMENT (acc |> str_of_rev_list)), t)
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
                then (
                    let acc2 = ')' :: ';' :: acc in
                    Ok (Some (COMMENT (acc2 |> str_of_rev_list)), t))
                else aux lv acc t
            | h :: t -> aux nest_level (h :: acc) t
            | [] -> failwith "never"
        in
        aux 0 [] t
    and scan_id (acc : char list) = function
        | h :: t when is_idchar h -> scan_id (h :: acc) t
        | t ->
            let r = acc |> str_of_rev_list in
            if String.length r = 1
            then Ok (Some (RESERVED r), t)
            else Ok (Some (ID r), t)
    and scan_keyword (acc : char list) = function
        | h :: t when is_idchar h -> scan_keyword (h :: acc) t
        | t -> Ok (Some (KEYWORD (acc |> str_of_rev_list)), t)
    and scan_reserved (acc : char list) = function
        | h :: t when is_idchar h -> scan_reserved (h :: acc) t
        | t -> Ok (Some (RESERVED (acc |> str_of_rev_list)), t)
    and scan_string (acc : char list) = function
        (* TODO https://webassembly.github.io/spec/core/text/values.html#strings *)
        | '\\' :: n :: m :: t when is_hexdigit n && is_hexdigit m ->
            let c = [ n; m ] |> hex_of_char_list |> Char.chr in
            scan_string (c :: acc) t
        | '\\' :: 'u' :: t ->
            let is_valid hex = hex < 0xd800 || (hex >= 0xe000 && hex < 0x110000) in
            (* FIXME: codepoint to char *)
            let codepoint_to_char_list _codepoint = [] in
            (match scan_hexdigit [] t with
                | Ok (Some codepoint, tt) when is_valid codepoint ->
                    let uc = codepoint_to_char_list codepoint in
                    scan_string (List.rev uc @ acc) tt
                | _ -> failwith "[scan_string] invalid codepoint")
        | '\\' :: 't' :: t -> scan_string ('\t' :: acc) t
        | '\\' :: 'n' :: t -> scan_string ('\n' :: acc) t
        | '\\' :: 'r' :: t -> scan_string ('\r' :: acc) t
        | '\\' :: '"' :: t -> scan_string ('"' :: acc) t
        | '\\' :: '\'' :: t -> scan_string ('\'' :: acc) t
        | '\\' :: '\\' :: t -> scan_string ('\\' :: acc) t
        | '\\' :: _ -> failwith "[scan_string] invalid slash"
        | '"' :: t -> Ok (Some (STRING (acc |> str_of_rev_list)), t)
        | h :: t -> scan_string (h :: acc) t
        | [] -> failwith "[scan_string] unexpected eof"
    and scan_num = function
        | '0' :: 'x' :: t -> hex_scan_num t
        | t -> decimal_scan_num t
    and hex_scan_num = function _ -> failwith "TODO"
    and decimal_scan_num t =
        match scan_digit [] t with
            | Ok (Some n, t2) ->
                let n2 = float_of_int n in
                (match t2 with
                    | '.' :: 'E' :: t3 | '.' :: 'e' :: t3 | 'E' :: t3 | 'e' :: t3 ->
                        decimal_scan_float_e n2 t3
                    | '.' :: t3 -> decimal_scan_float_frac n2 t3
                    | _ -> Ok (Some (INT n), t2))
            | Ok _ -> failwith "never"
            | Error e -> Error e
    and decimal_scan_float_frac n t =
        match scan_digit_frac [] t with
            | Ok (Some e, t2) ->
                let n2 = n +. e in
                (match t2 with
                    | 'E' :: t3 | 'e' :: t3 -> decimal_scan_float_e n2 t3
                    | _ -> Ok (Some (FLOAT n2), t2))
            | Ok _ -> failwith "never"
            | Error e -> Error e
    and decimal_scan_float_e n t =
        let (sign, t2) =
            match t with '-' :: t2 -> ('-', t2) | '+' :: t2 | t2 -> ('+', t2)
        in
        match scan_digit [] t2 with
            | Ok (Some e, t3) ->
                let e2 = if Char.equal sign '+' then e else -e in
                let e3 = Stdlib.Float.pow 10. (float_of_int e2) in
                let n2 = n *. e3 in
                Ok (Some (FLOAT n2), t3)
            | Ok _ -> failwith "never"
            | Error e -> Error e
    and scan_token = function
        | [] -> Ok (None, [])
        | h :: t when is_space h -> scan_token t
        | '(' :: ';' :: _ as t -> scan_block_comment t
        | ';' :: ';' :: _ as t -> scan_line_comment t
        | '(' :: t -> Ok (Some LEFT_PAREN, t)
        | ')' :: t -> Ok (Some RIGHT_PAREN, t)
        | '"' :: t -> scan_string [] t
        | '$' :: _ as t -> scan_id [] t
        | 'a' .. 'z' :: _ as t ->
            (match scan_keyword [] t with
                | Ok (Some (KEYWORD k), tt) when String.equal k "inf" ->
                    Ok (Some (FLOAT infinity), tt)
                | Ok (Some (KEYWORD k), tt) when String.equal k "nan" ->
                    Ok (Some (FLOAT nan), tt)
                | s -> s)
        | '+' :: t ->
            (match scan_num t with
                | Ok (None, tt) -> Ok (Some (RESERVED "+"), tt)
                | Ok (s, tt) -> Ok (s, tt)
                | Error s -> failwith s)
        | '-' :: t ->
            (match scan_num t with
                | Ok (None, tt) -> Ok (Some (RESERVED "-"), tt)
                | Ok (Some (INT n), tt) -> Ok (Some (INT (-n)), tt)
                | Ok (Some (FLOAT n), tt) -> Ok (Some (FLOAT (-.n)), tt)
                | Ok _ -> failwith "never"
                | Error s -> failwith s)
        | h :: _ as t when is_digit h -> scan_num t
        | h :: t when is_idchar h -> scan_reserved [ h ] t
        | _ -> failwith "never"
    in
    scan_ [] (String.to_list src)
