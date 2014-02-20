%token <string> ID
%token <int> INT
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token FN
%token ARROW
%token REC
%token LEFT_PAREN
%token RIGHT_PAREN
%token EOF
%start <Ast.expr> main
%%
main:
    | e = expr; EOF { e }

expr:
    | a = app { a }
    | IF; e0 = expr; THEN; e1 = expr; ELSE; e2 = expr { Ast.If (e0, e1, e2) }
    | FN; id = ID; ARROW; e = expr { Ast.Fun (id,e) }
    | REC; id = ID; ARROW; e = expr { Ast.Rec (id,e) }

app:
    | e1 = app; e2 = atom { Ast.App(e1,e2) }
    | a = atom { a }

atom:
    | id = ID { Ast.Id id }
    | i = INT { Ast.Int i }
    | TRUE { Ast.Bool true }
    | FALSE { Ast.Bool false }
    | LEFT_PAREN; e = expr; RIGHT_PAREN { e }

