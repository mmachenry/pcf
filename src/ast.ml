type expr =
    | Id of string
    | Int of int
    | Bool of bool
    | Fun of string * expr
    | App of expr * expr
    | If of expr * expr * expr
    | Rec of string * expr
    | Primitive of (expr -> expr)

let rec string_of_ast = function
    | Id str -> "Id " ^ str
    | Int i -> "Int " ^ string_of_int i
    | Bool b -> "Bool " ^ string_of_bool b
    | Fun (param, expr) ->
        "Fun (\"" ^ param ^ "\", " ^ string_of_ast expr ^ ")"
    | App (e1, e2) ->
        "App (" ^ string_of_ast e1 ^ ", " ^ string_of_ast e2 ^ ")"
    | If (e1,e2,e3) ->
        "If (" ^ string_of_ast e1 ^
        ", " ^ string_of_ast e2 ^
        ", " ^ string_of_ast e3 ^ ")"
    | Rec (id, expr) ->
        "Rec (" ^ id ^ ", " ^ string_of_ast expr ^ ")"
    | Primitive _ -> "Primitive (fun x-> x)"

