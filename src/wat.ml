open! Containers
module Cli = CliCommon.Make (WatScanner)

let () = Cli.main ()
