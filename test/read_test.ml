open OUnit

let tests =
    "Test parser" >:::
      [ "Parse one character identifier" >::
        (fun () -> (assert_equal (Ast.Id "x") (Read.ast_of_string "x")))
      ]

