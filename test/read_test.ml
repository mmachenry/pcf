open OUnit
open Pcf

(* TODO:
    "x";
    "x x";
    "fn x => x";
    "rec fact => fn n => if iszero n then 1 else mul n (fact (pred n))";
*)

let tests =
    "Test parser" >:::
      [ "Parse one character identifier" >::
        (fun () -> assert_equal 1 1);
        "another" >::
        (fun () -> (assert_equal (Ast.Id "x") (Read.ast_of_string "x")));
      ]
