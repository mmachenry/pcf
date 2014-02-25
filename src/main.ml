open ExtLib

let filename = ref ""

let usage_msg = "pcf <filename>"

let speclist = []

let read_file (filename : string) : string =
    let ic = open_in filename
    in String.concat "\n" (Std.input_list ic)

let () =
    Arg.parse (Arg.align speclist) (fun str -> filename := str) usage_msg;
    if !filename = ""
    then print_endline usage_msg
    else let str = read_file !filename in
         let ast = Read.ast_of_string str in
         let outstr = Print.string_of_value (Eval.eval ast) in
         print_endline outstr

