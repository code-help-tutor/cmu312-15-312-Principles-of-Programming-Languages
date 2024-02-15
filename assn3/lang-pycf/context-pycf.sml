structure ContextPyCF :> CONTEXT_PYCF =
let
  structure Set = PyCF.Object.Var.Set
  structure SetContext = SetContext (structure Set = Set)
in
  struct
    open SetContext

    type var = PyCF.Object.Var.t

    val singleton = Set.singleton
    val insert = Set.insert

    val find = Set.member
  end
end
