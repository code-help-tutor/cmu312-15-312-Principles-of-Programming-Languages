functor Stmt (type term) :> STMT where type term = term =
struct
  type term = term

  datatype t = Print of term | Assert of term * bool * term
end
