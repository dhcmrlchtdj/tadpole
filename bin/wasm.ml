open Tadpole
module Cli = CliCommon.Make (WasmScanner)

let () = Cli.main ()