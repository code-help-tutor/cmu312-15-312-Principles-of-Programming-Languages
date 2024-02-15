structure Token =
struct
  datatype token =
    IDENT of string
  | FILENAME of string
  | DOT
  | LPAREN
  | RPAREN
  | EQUAL
  | VAL
  | IFZ
  | COMMA
  | LOAD
  | PRINT
  | TYPE
  | STAR
  | PLUS
  | DASH
  | SLASH
  | PERCENT
  | ANDAND
  | OROR
  | BANG
  | LTE
  | GTE
  | NEQ
  | ARROW
  | REC
  | UNIT
  | CASE
  | COLON
  | TILDE
  | FALSE
  | FN
  | FUN
  | FOLD
  | UNFOLD
  | GT
  | LT
  | IF
  | INL
  | INR
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | LET
  | SPLIT
  | IS
  | IN
  | TRUE
  | NUM of int
  | ZERO
  | SUCC
  | THEN
  | ELSE
  | DARROW
  | END
  | OF
  | PIPE
  | ABORT
  | VOID
  | CMD
  | RET
  | NAT
  | BOOL
  | EMIT
  | NEWCHAN
  | CHAN
  | DO
  | SPAWN
  | SYNC
  | STRING of string
end
