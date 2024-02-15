functor Inductive (T : POSITIVE_TYPE_OPERATOR) :> INDUCTIVE where T = T =
  struct
    structure T = T

    datatype t = FOLD of t T.view

    fun REC (f : 'rho T.view -> 'rho) : t -> 'rho =
      fn (FOLD e : t) => f (T.map (REC f) e)
  end
