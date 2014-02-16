type expr =
    | Id of string
    | Int of int
    | Bool of bool
    | Fun of string * expr
    | App of expr * expr
    | Succ
    | Pred
    | IsZero
    | If of expr * expr * expr
    | Rec of string * expr
