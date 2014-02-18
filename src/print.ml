(* Printer for values that programs can evaluate to *)

let rec string_of_value = function
    | Ast.Int i -> string_of_int i
    | Ast.Bool b -> string_of_bool b
    | Ast.Fun (param, expr) -> "<function>"
    | Ast.Succ -> "succ"
    | Ast.Pred -> "pred"
    | Ast.IsZero -> "iszero"
    | _ -> "<not a value"

