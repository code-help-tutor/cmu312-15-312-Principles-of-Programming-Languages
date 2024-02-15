structure Token =
struct

  datatype token =
    IDENT of string

  | NUMBER of int
  | BOOLCONST of bool

  | LBRACKET
  | RBRACKET
  | LPAREN
  | RPAREN

  | COLON
  | COMMA
  | EQUAL
  | PLUS
  | LEQ
  | SEMICOLON

  | DEF
  | ISINSTANCE
  | LEN
  | IF
  | ELSE
  | LAMBDA
  | RETURN

  | IFNAME
  | PRINT

  | BOOL
  | INT
  | LIST
  | FUN
end
