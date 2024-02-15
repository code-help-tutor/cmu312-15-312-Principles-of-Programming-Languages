structure DynamicsPCF :>
  DYNAMICS
    where State = StatePCF
      and type term = PCF.Exp.t
  =
  let
    structure Exp = PCF.Exp
  in
    struct
      structure State = StatePCF

      type term = Exp.t

      structure Error = DynamicsErrorPCF
      exception Malformed of Error.t

      fun progress (exp : Exp.t) : Exp.t State.t =
        raise Fail "TODO"
    end
  end
