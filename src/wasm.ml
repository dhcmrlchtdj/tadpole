open! Containers
module Cli = CliCommon.Make (WasmScanner)

let () = Cli.main ()
