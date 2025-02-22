%{
open Lang.Syntax
%}

%type <program> program
%start program

%token EOF
%token <string> LID UID
%token <int> NUM

%token NEW LET IN CLASS EXTENDS METHOD NULL IF THEN ELSE
%token INT

%token DOT COMMA SEMICOLON EQ PLUS MINUS STAR ASSIGN EQEQ ARROW
%token LBRACE RBRACE LPAREN RPAREN

%nonassoc IN
%right SEMICOLON
%nonassoc ASSIGN
%left PLUS MINUS
%left STAR
%nonassoc DOT
%%

ty 
  : INT { TInt }
  | UID { TClass $1 }

lvalue 
  : LID          { LValueVar $1 }
  | expr DOT LID { LValueField($1, $3) }

expr
  : NUM                                                    { EInt $1 }
  | NULL                                                   { ENull }
  | LID                                                    { EVar $1 }
  | expr PLUS expr                                         { EBinop(BINOP_Add, $1, $3) }
  | expr MINUS expr                                        { EBinop(BINOP_Sub, $1, $3) }
  | expr STAR expr                                         { EBinop(BINOP_Mul, $1, $3) }
  | expr EQEQ expr                                         { EBinop(BINOP_Eq, $1, $3) }
  | NEW UID                                                { ENewObject $2 }
  | expr DOT LID LPAREN separated_list(COMMA, expr) RPAREN { EMethodCall($1, $3, $5) }
  | LET ty LID ASSIGN expr IN expr                         { ELet($3, $2, $5, $7) } 
  | lvalue ASSIGN expr                                     { EAssign($1, $3) }
  | expr DOT LID                                           { EFetch($1, $3) }
  | expr SEMICOLON expr                                    { ESeq($1, $3) }
  | LPAREN expr RPAREN                                     { $2 }
  | IF expr THEN expr ELSE expr                            { EIf($2, $4, $6) }

param
  : ty LID { ($2, $1) }

method_def
  : METHOD LID LPAREN separated_list(COMMA, param) RPAREN ARROW ty LBRACE expr RBRACE 
    { MethodDef {
        method_name = $2; 
        method_return_type = $7; 
        method_params = $4; 
        method_body = $9
      }
    }

field 
  : ty LID SEMICOLON { ($2, $1) }

class_def 
  : CLASS UID EXTENDS UID LBRACE list(field) list(method_def) RBRACE
    { ClassDef {
        class_name = $2; 
        class_parent_name = $4; 
        class_fields = $6;
        class_methods = $7;
      }
    }

program
  : list(class_def) expr EOF
    { Program {
        class_defs = $1;
        toplevel_expr = $2
      }
    }

%%
