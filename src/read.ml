(* TODO: This module will combine the lexer, parser, and ast into a convenient
interface in which we can read in PCF expressions *)
let ast_of_string s = Parser.main Lexer.read (Lexing.from_string s)
