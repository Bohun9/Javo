{
open Parser

let kw_map = [
  "new", NEW;
  "let", LET;
  "in", IN;
  "int", INT;
  "class", CLASS;
  "extends", EXTENDS;
  "method", METHOD;
  "null", NULL;
  "if", IF;
  "then", THEN;
  "else", ELSE
] |> List.to_seq |> Hashtbl.of_seq

let symbols_map = [
  "(", LPAREN;
  ")", RPAREN;
  "{", LBRACE;
  "}", RBRACE;
  ".", DOT;
  ",", COMMA;
  ";", SEMICOLON;
  "=", EQ;
  "+", PLUS;
  "-", MINUS;
  "*", STAR;
  "->", ARROW;
  ":=", ASSIGN;
  "==", EQEQ
] |> List.to_seq |> Hashtbl.of_seq

let make_lid s = 
  try
    Hashtbl.find kw_map s
  with
    Not_found -> LID s

let make_symbol s = 
  Hashtbl.find symbols_map s
}

let digit = ['0'-'9']
let int = digit+
let lower_identifier = ['a'-'z' '_'] ['a'-'z' '_' '0'-'9']*
let upper_identifier = ['A'-'Z'] ['a'-'z' '_' '0'-'9']*
let symbol = ['(' ')' '{' '}' '.' ',' ';' '=' '+' '-' '*'] | ":=" | "->" | "=="

rule token = parse
  | [' ' '\t']             { token lexbuf }
  | ['\n']                 { Lexing.new_line lexbuf; token lexbuf }
  | int as i               { NUM (int_of_string i) }
  | lower_identifier as id { make_lid id }
  | upper_identifier as id { UID id }
  | symbol as s            { make_symbol s }
  | eof                    { EOF }
