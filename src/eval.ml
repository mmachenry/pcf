(* TODO: the evaluator for the PCF program. Accepts an Ast and produces a
value*)

open Ast

let rec eval env = function
    | Id id -> List.assoc id env;
    | App (e1, e2) -> (
        let func = eval env e1 in
        let arg = eval env e2 in
        eval_app env func arg)
    | If (e1, e2, e3) -> (match eval env e1 with
        | Bool b -> eval env (if b then e2 else e1)
        | _ -> failwith "Not a boolean value in IF.");
    | Rec (id, expr) -> failwith "Not yet implemented.";
    | other -> other
and eval_app env func arg = match func with
    | Fun (id, body) -> eval ((id, arg)::env) body
    | Succ -> (match arg with Int i -> Ast.Int (i+1) | _ -> failwith "not int")
    | Pred -> (match arg with Int i -> Ast.Int (i-1) | _ -> failwith "not int")
    | IsZero -> (match arg with Int i -> Ast.Bool (i==0) | _ -> failwith "not int")
    | _ -> failwith "Cannot apply a non-function."

