open OUnit
open Pcf


let tests = "Test parser" >::: [
    "Simple ID" >::
    (fun () -> (assert_equal (Ast.Id "x") (Read.ast_of_string "x")));

    "Application of simple ID" >::
    (fun () -> (assert_equal (Read.ast_of_string "x x")
        (Ast.App ((Ast.Id "x"), (Ast.Id "x")))));

    "Identity function" >::
    (fun () -> (assert_equal (Read.ast_of_string "fn param => param")
        (Ast.Fun ("param", (Ast.Id "param")))));

    "Factorial" >::
    (fun () -> (assert_equal
        (Read.ast_of_string "rec fact => fn n => if iszero n then 1 else mul n (fact (pred n))")
        (Ast.Rec ("fact",
            (Ast.Fun ("n",
                (Ast.If ((Ast.App (Ast.IsZero, (Ast.Id "n"))),
                         (Ast.Int 1),
                         (Ast.App ((Ast.App ((Ast.Id "mul"),
                                             (Ast.Id "n"))),
                                  (Ast.App ((Ast.Id "fact"),
                                            (Ast.App (Ast.Pred, (Ast.Id "n")))))))))))))))
    ]
