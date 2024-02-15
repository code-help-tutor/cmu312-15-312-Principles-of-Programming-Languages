structure Token =
struct

  datatype token =
    IDENT of string

  | NUMBER of int

  | LANGLE
  | RANGLE
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | LPAREN
  | RPAREN

  | ARROW
  | BAR
  | COLON
  | COMMA
  | DARROW
  | DOT
  | EQUAL
  | PLUS
  | STAR
  | SEMICOLON

  | INT
  | LEQ
  | REC
  | ERROR
  | IN
  | CASE
  | FN
  | FOLD
  | UNFOLD
end
