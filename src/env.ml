open! Containers

module type EnvValue = sig
  type t
end

module Make (EV : EnvValue) = struct
  type key = string

  type value = EV.t

  type t = (key, value) Hashtbl.t

  let create () : t = Hashtbl.create 32

  let get (env : t) (key : key) : value option = Hashtbl.get env key

  let set (env : t) (key : key) (value : value) = Hashtbl.add env key value
end
