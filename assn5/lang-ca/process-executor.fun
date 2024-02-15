functor Executor (structure EC : EXECUTION_CONTEXT
                  structure Dynamics : DYNAMICS where State = EC)
  :> EXECUTOR where type context = Dynamics.term EC.t =
  let
    structure D = Dynamics

    (* not exported from EC to reduce confusion in EC, and with Cmd.Bind *)
    val bind = EC.bind

    fun stepProcWithCount p n =
      case n of
        0 => p
      | _ => stepProcWithCount (bind (p, Dynamics.progress)) (n - 1)
  in
    struct
      type context = Dynamics.term EC.t

      exception Malformed
      fun run (ctx : context) : context =
        case EC.chooseProcess ctx of
          (SOME p, ctx') => run (EC.conc (stepProcWithCount (EC.initial p) 100, ctx'))
        | (NONE  , ctx') => ctx'
    end
  end
