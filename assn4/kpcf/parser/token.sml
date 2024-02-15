structure Token =
struct
  datatype token =
    IDENT of string
  (* | NUM of int *)

  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | LBRACE
  | RBRACE
  | LANGLE
  | RANGLE

  | COMMA
  | BAR
  | DOT
  | COLON
  | DARROW

  | NAT
  | ARROW
  | PLUS
  | TIMES
  | UNIT
  | VOID
  | TYPEA
  | TYPEB
  | TYPEC
  | TYPED

  | IFZ
  | CASE
  | SPLIT
  | LLABEL
  | RLABEL
  | CONT
  | LETCC
  | Z
  | S
  | RET
  | IS
  | IN
  | FUN
  | FN
  | LET
  | COMP
  | THROW
end
