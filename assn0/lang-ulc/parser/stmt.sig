signature STMT =
sig
  type term

  datatype t = Print of term | Assert of term * bool * term
end
