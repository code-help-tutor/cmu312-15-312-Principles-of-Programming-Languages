structure Syntax =
struct

  type variable = string
  type symbol = string

  datatype ty =
    Int
  | Prod of (string * ty) list
  | Sum of (string * ty) list
  | Arrow of ty * ty
  | Rec of variable * ty

  datatype exp =
    Var of variable
  | Number of int
  | Err of ty
  | Plus of exp * exp
  | Leq of exp * exp
  | Tuple of (string * exp) list
  | Proj of exp * string
  | Inj of string * ty * exp
  | Case of ty * exp * (string * variable * exp) list
  | Lam of (ty * string) * exp
  | App of exp * exp
  | Fold of (variable * ty) * exp
  | Unfold of exp
end
