signature EVALUATOR =
sig
  type term
  type final

  val evaluate: term -> final
end
