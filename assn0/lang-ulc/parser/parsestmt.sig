signature PARSE_STMT =
sig
  type term

  datatype t =
    Define of bool * string * term
  | Undefine of string
  | Print of bool * term
  | Assert of bool * term * bool * term
end
