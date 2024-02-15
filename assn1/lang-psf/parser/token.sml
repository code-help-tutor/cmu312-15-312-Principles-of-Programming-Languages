structure Token =
struct

  datatype token =
    IDENT of string

  | STRING of string

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

  | UNIT
  | VOID
  | FN
  | CASE
  | IN
  | L
  | R

end
