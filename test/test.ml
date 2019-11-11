open Tadpole

let cases = [ ("input", "output") ]

let build (input, output) =
    ( "test",
      `Quick,
      fun () ->
          Alcotest.check
            Alcotest.string
            input
            output
            (input |> fun _ -> "output") )

let () = Alcotest.run "tadpole" [ ("test_set", List.map build cases) ]
