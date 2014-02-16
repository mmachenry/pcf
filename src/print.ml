(* Printer for the AST so that the results can be seen *)
(* TODO: Pretty printer.
   Remove extraneous parens when possible and introduce newlines where
   it will look nice. *)

let rec string_of_ast = function
    | Ast.Id str -> str
    | Ast.Int i -> string_of_int i
    | Ast.Bool b -> string_of_bool b
    | Ast.Fun (param, expr) ->
        "fn " ^ param ^ " => (" ^ string_of_ast expr ^ ")"
    | Ast.App (e1, e2) ->
        "(" ^ string_of_ast e1 ^ ") (" ^ string_of_ast e2 ^ ")"
    | Ast.Succ -> "succ"
    | Ast.Pred -> "pred"
    | Ast.IsZero -> "iszero"
    | Ast.If (e1,e2,e3) ->
        "(if " ^ string_of_ast e1 ^
        " then " ^ string_of_ast e2 ^
        " else " ^ string_of_ast e3 ^ ")"
    | Ast.Rec (id, expr) ->
        "rec " ^ id ^ " => (" ^ string_of_ast expr ^ ")"

