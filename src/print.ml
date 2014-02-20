(* Printer for values that programs can evaluate to *)

let rec string_of_value = function
    | Ast.Int i -> string_of_int i
    | Ast.Bool b -> string_of_bool b
    | Ast.Fun (param, expr) -> "<function>"
    | Ast.Primitive _ -> "<primitive>"
    | _ -> "<not a value"

