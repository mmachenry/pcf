let examples = [
    "x";
    "x x";
    "fn x => x";
    "rec fact => fn n => if iszero n then 1 else mul n (fact (pred n))";
    ]

let () = List.iter (fun s ->
    print_endline
        (Print.string_of_ast
            (Read.ast_of_string s)))
    examples

