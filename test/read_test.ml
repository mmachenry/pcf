open OUnit
open Ast

let check_read str ast () = assert_equal (Read.ast_of_string str) ast

let tests = "Test parser" >::: [
    "Simple ID" >:: check_read "x" (Id "x");

    "Application of simple ID" >:: check_read
        "x x"
        (App ((Id "x"), (Id "x")));

    "Identity function" >:: check_read
        "fn param => param"
        (Fun ("param", (Id "param")));

    "Tokens on two lines stay separate" >:: check_read
        "(fn x => fn y => y)
        2
        413"
        (App (App ((Fun ("x", Fun ("y", Id "y"))),
                  Int 2),
             Int 413));
        
    "Recursive binary add" >:: check_read
        "(rec add => fn n => fn m =>
            if iszero n
            then m
            else succ(add (pred n) m))
        400
        13"
        (App (App (Rec ("add", (Fun ("n", Fun ("m",
                  (If (App (Id "iszero",Id "n"),
                      Id "m",
                      (App (Id "succ",
                           (App ((App (Id "add",
                                      (App (Id "pred", Id "n")))),
                                Id "m")))))))))),
                  Int 400),
             Int 13));

    "Factorial" >::
    (fun () -> (assert_equal
        (Read.ast_of_string
            "rec fact => fn n => if iszero n then 1 else mul n (fact (pred n))")
        (Rec ("fact",
            (Fun ("n",
                (If ((App (Id "iszero", (Id "n"))),
                         (Int 1),
                         (App ((App ((Id "mul"),
                                             (Id "n"))),
                                  (App ((Id "fact"),
                                            (App (Id "pred", (Id "n"))))))))
                )))))))
    ]
