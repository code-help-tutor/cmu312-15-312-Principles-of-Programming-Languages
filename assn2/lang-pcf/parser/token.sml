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

  | NAT
  | ZERO
  | SUCC
  | NUMBER of int
  | IFZ
  | FUN
  | IS

end
