let () =
    print_endline
        (Print.string_of_ast
            (Read.ast_of_string "x x"))
