signature CONTEXT_PYCF =
sig
  include CONTEXT

  type var = PyCF.Object.Var.t

  val singleton: var -> context
  val insert: context -> var -> context

  val find: context -> var -> bool
end
