structure DynamicsPSF :>
  DYNAMICS
    where State = StatePSF
      and type term = PSF.Exp.t
  =
  let
    structure Exp = PSF.Exp
  in
    struct
      structure State = StatePSF

      type term = Exp.t

      structure Error = DynamicsErrorPSF
      exception Malformed of Error.t

      fun progress (exp : Exp.t) : Exp.t State.t =
        raise Fail "TODO"
    end
  end
