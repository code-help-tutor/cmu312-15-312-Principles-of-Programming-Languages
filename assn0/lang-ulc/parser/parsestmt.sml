functor ParseStmt (type term) :> PARSE_STMT where type term = term =
struct
  type term = term

  datatype t =
    Define of bool * string * term
  | Undefine of string
  | Print of bool * term
  | Assert of bool * term * bool * term
end
