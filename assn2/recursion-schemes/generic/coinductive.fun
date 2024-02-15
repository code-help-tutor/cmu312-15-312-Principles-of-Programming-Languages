functor Coinductive (T : POSITIVE_TYPE_OPERATOR) :> COINDUCTIVE where T = T =
  struct
    structure T = T

    datatype t = FOLD of unit -> t T.view

    fun GEN (f : 'sigma -> 'sigma T.view) : 'sigma -> t =
      fn (s : 'sigma) => FOLD (fn () => T.map (GEN f) (f s))

    val UNFOLD = fn FOLD e => e ()
  end
