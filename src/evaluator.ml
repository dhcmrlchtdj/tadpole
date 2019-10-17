open! Containers
module D = Datum

(* *** *)

type val_type = I32 | I64 | F32 | F64 [@@deriving show]

and value =
    | TRUE
    | FALSE
    | ERROR of string
    | INT of int
    | NIL
    | TODO of string
    (* | Func of (param_name * param_type) list * ret_type * env * body *)
    | Func of (string * val_type) list * val_type * env * string
[@@deriving show]

and env = (string, value) Hashtbl.t [@@deriving show]

(* *** *)

let env_create () : env = Hashtbl.create 32

let env_add (env : env) (key : string) (value : value) =
    Hashtbl.add env key value

let env_get (env : env) (key : string) : value option = Hashtbl.get env key

(* *** *)

let sprintf = Printf.sprintf

let rec evaluate (env : env) (datum : D.t) : value =
    let rec m_datum = function
        | D.LIST ds -> m_list ds
        | h -> failwith (sprintf "m_datum | unknown | %s" (D.show h))
    and m_list = function
        | D.KEYWORD "module" :: ds ->
            List.fold_left (fun _prev d -> m_datum d) NIL ds
        | D.KEYWORD "memory" :: ds -> m_memory ds
        | D.KEYWORD "func" :: ds -> m_func ds
        | D.KEYWORD "assert_malformed" :: ds -> m_assert_malformed ds
        | h :: _ -> failwith (sprintf "m_list | unknown | %s" (D.show h))
        | [] -> failwith "m_list | empty"
    and m_memory = function
        | [D.INT 0] -> INT 0
        | _ -> failwith "[m_memory] never"
    and m_func = function
        | [] -> TODO "empty function"
        | D.LIST [D.KEYWORD "export"; D.STRING func_name] :: ds ->
            let f = m_func ds in
            let _ = env_add env func_name f in
            f
        | h :: _ -> failwith (sprintf "m_func | unknown | %s" (D.show h))
    and m_assert_malformed = function
        | [
            D.LIST
              [D.KEYWORD "module"; D.KEYWORD "quote"; D.STRING module_string];
            D.STRING error_message;
          ] -> (
            let ret =
                module_string |> WatScanner.scan |> Parser.parse |> eval
            in
            match ret with
                | ERROR msg ->
                    if String.equal msg error_message then TRUE else FALSE
                | _ -> ERROR "" )
        | _ -> TODO ""
    in
    m_datum datum

and eval (datums : D.t list) : value =
    let env = env_create () in
    let f = evaluate env in
    let g _prev datum = f datum in
    List.fold_left g NIL datums
