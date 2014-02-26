open Ast

let make_int_func (f : int -> int) : Value.value = Value.Primitive (function
    | Value.Int i -> Value.Int (f i)
    | _ -> failwith "Not an integer.")

let primitives : Value.env = [
    ("succ", make_int_func (fun x -> x + 1 ));
    ("pred", make_int_func (fun x -> x - 1));
    ("iszero", Value.Primitive (function
        | Value.Int i -> Value.Bool (i = 0)
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

let rec eval_in_env (env : Value.env) : (expr -> Value.value) = function
    | Id id -> (
        try List.assoc id env
        with Not_found -> failwith ("Invalid ID: " ^ id))
    | App (e1, e2) -> (
        let func = eval_in_env env e1 in
        let arg = eval_in_env env e2 in
        eval_app func arg)
    | If (e1, e2, e3) -> (match eval_in_env env e1 with
        | Value.Bool b -> eval_in_env env (if b then e2 else e3)
        | _ -> failwith "Not a boolean value in IF.")
    | Rec (id, expr) -> eval_in_env env (subst expr id (Rec (id,expr)))
    | Int i -> Value.Int i
    | Bool b -> Value.Bool b
    | Fun (param, body) -> Value.Closure (env, param, body)

and eval_app (func : Value.value) (arg : Value.value) : Value.value =
    match func with
    | Value.Closure (env, id, body) ->
        let e = ((id, arg)::env)
        in eval_in_env e body
    | Value.Primitive f -> f arg
    | ast -> failwith "Cannot apply a non-function."

let eval expr = eval_in_env primitives expr

