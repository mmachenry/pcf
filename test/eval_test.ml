open OUnit

let eval_string (program : string) : string =
    Ast.string_of_ast (Eval.eval (Read.ast_of_string program))

let check_eval p1 p2 () = assert_equal (eval_string p1) (eval_string p2)

let tests = "Test eval" >::: [
    "Number" >:: (check_eval "413" "413");
    "App" >:: (check_eval "(fn x => x) 413" "413");
    "Higher-order function" >:: (check_eval "413"
        "(fn x => x 412) succ");
    "If" >:: (check_eval "413"
        "if iszero(pred (pred 2)) then succ (pred 413) else 2");
    "two params" >:: (check_eval "2"
        "(fn m => (fn n => if iszero n then n else n)) 0 2");
    "Recursive binary add" >:: (check_eval
        "(rec add => fn n => fn m =>
            if iszero n
            then m
            else succ(add (pred n) m)) 400 13"
        "413")
    ]
(*
"(fn mul =>
    (rec fact => (fn n =>
        if iszero n
        then 1
        else mul n (fact (pred n))))
)
(
(fn add =>
    (rec _mul => fn n => fn m =>
        if iszero n
        then 0
        else add m (_mul (pred n) m)))

(rec _add => fn n => fn m =>
    if iszero n
    then m
    else succ(_add (pred n) m))
)
5"
*)
