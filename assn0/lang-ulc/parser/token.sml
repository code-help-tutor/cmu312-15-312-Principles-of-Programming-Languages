structure Token =
struct
  datatype token =
    IDENT of string

  | LPAREN
  | RPAREN

  | DOT
  | LAMBDA

  | NORM
  | DEF
  | DEF_FORCE
  | UNDEF
  | PRINT
  | PRINT_FORCE
  | ASSERT
  | ASSERT_FORCE
  | EQ
  | NEQ
end
