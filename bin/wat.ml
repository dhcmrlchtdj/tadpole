open Tadpole
module Cli = CliCommon.Make (WatScanner)

let () = Cli.main ()