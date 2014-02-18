open ExtLib

let read_file (filename : string) : string =
    let ic = open_in filename
    in String.concat "" (Std.input_list ic)

let () =
    let filename = "testfile.prg" in (* file name from cmd line args *)
    let str = read_file filename in
    let ast = Read.ast_of_string str in
    (*let outstr = Ast.string_of_ast ast in*)
    let outstr = Print.string_of_value (Eval.eval [] ast) in
    print_endline outstr

