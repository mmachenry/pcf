open OUnit
open Pcf

let tests =
    "Test parser" >:::
      [ "Parse one character identifier" >::
        (fun () -> assert_equal 1 1);
        "another" >::
        (fun () -> (assert_equal (Ast.Id "x") (Read.ast_of_string "x")));
      ]
