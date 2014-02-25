open Ast

let make_int_func (f : int -> int) : expr = Primitive (function
    | Int i -> Int (f i)
    | _ -> failwith "Not an integer.")

let primitives = [
    ("succ", make_int_func (fun x -> x + 1 ));
    ("pred", make_int_func (fun x -> x - 1));
    ("iszero", Primitive (function
        | Int i -> Bool (i = 0)
        | _ -> failwith "Not an integer."))
    ]

let rec subst expr id replacement = match expr with
    | Id str -> if id = str then replacement else Id str
    | Fun (param, body) ->
        if param = id
        then Fun (param,body)
        else Fun (param, subst body id replacement)
    | App (e1, e2) -> App (subst e1 id replacement, subst e2 id replacement)
    | If (e1, e2, e3) ->
        If (subst e1 id replacement,
            subst e2 id replacement,
            subst e3 id replacement)
    | Rec (i, e) ->
        if id = i
        then Rec (i,e)
        else Rec (i, subst e id replacement)
    | other -> other

let rec eval_in_env env = function
    | Id id -> (
        try List.assoc id env
        with Not_found -> failwith ("Invalid ID: " ^ id))
    | App (e1, e2) -> (
        let func = eval_in_env env e1 in
        let arg = eval_in_env env e2 in
        eval_app env func arg)
    | If (e1, e2, e3) -> (match eval_in_env env e1 with
        | Bool b -> eval_in_env env (if b then e2 else e3)
        | _ -> failwith "Not a boolean value in IF.")
    | Rec (id, expr) -> eval_in_env env (subst expr id (Rec (id,expr)))
    | other -> other

(* TODO need to handle indetifier capture. implement closures. *)
and eval_app env func arg = match func with
    | Fun (id, body) -> let e = ((id, arg)::env) in eval_in_env e body
    | Primitive f -> f arg
    | ast -> failwith ("Cannot apply a non-function: " ^ (string_of_ast ast))

let eval expr = eval_in_env primitives expr

