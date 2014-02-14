{

open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
        { pos with pos_bol = lexbuf.lex_curr_pos;
                   pos_lnum = pos.pos_lnum + 1
        }
}

let digit = ['0'-'9']
let integer = '-'? digit+
(*
let frac = '.' digit+
let exponent = ['e' 'E']['-' '+']? digit+
let floating = digit+ frac? exponent?
*)
let whitespace = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = parse
    | whitespace { read lexbuf }
    | newline { next_line lexbuf; read lexbuf }
    | integer { INT (int_of_string (Lexing.lexeme lexbuf)) }
    (*| floating { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }*)
    | "true" { TRUE }
    | "false" { FALSE }
    | "succ" { SUCC }
    | "pred" { PRED }
    | "iszero" { ISZERO }
    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }
    | "fn" { FN }
    | "=>" { ARROW }
    | "rec" { REC }
    | '(' { LEFT_PAREN }
    | ')' { RIGHT_PAREN }
    | id { ID (Lexing.lexeme lexbuf) }
    | eof { EOF }

