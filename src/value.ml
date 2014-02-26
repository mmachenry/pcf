type env = (string * value) list
and value =
    | Int of int
    | Bool of bool
    | Closure of env * string * Ast.expr
    | Primitive of (value -> value)

let string_of_value = function
    | Int i -> string_of_int i
    | Bool b -> string_of_bool b
    | Closure (env, param, body) -> "<fun>"
    | Primitive f -> "<primitive>"
