open! Containers
open Tadpole
module Wat = Cli.Make (WatParser)

let () = Wat.run ()
