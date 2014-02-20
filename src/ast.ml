type expr =
    | Id of string
    | Int of int
    | Bool of bool
    | Fun of string * expr
    | App of expr * expr
    | If of expr * expr * expr
    | Rec of string * expr
    | Primitive of (expr -> expr)

(* TODO: Pretty printer.
   Remove extraneous parens when possible and introduce newlines where
   it will look nice. *)

let rec string_of_ast = function
    | Id str -> str
    | Int i -> string_of_int i
    | Bool b -> string_of_bool b
    | Fun (param, expr) ->
        "(fn " ^ param ^ " => " ^ string_of_ast expr ^ ")"
    | App (e1, e2) ->
        "(" ^ string_of_ast e1 ^ ") (" ^ string_of_ast e2 ^ ")"
    | If (e1,e2,e3) ->
        "(if " ^ string_of_ast e1 ^
        " then " ^ string_of_ast e2 ^
        " else " ^ string_of_ast e3 ^ ")"
    | Rec (id, expr) ->
        "(rec " ^ id ^ " => " ^ string_of_ast expr ^ ")"
    | Primitive _ -> "<primitive>"

