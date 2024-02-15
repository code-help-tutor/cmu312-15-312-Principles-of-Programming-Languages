functor DynamicsCA
  (EC : EXECUTION_CONTEXT where type chan = CA.Chan.t
                            and type msg  = CA.Exp.t)

  :> DYNAMICS
    where State = EC
      and type term = CA.Chan.t * CA.Cmd.t
  =
  let
    structure Exp = CA.Exp
    structure Cmd = CA.Cmd
    structure Chan = CA.Chan
    structure StateExp = StateCAExp
    structure DynExp = DynamicsCAExp
  in
    struct
      structure State = EC

      type term = Chan.t * Cmd.t

      structure Error = StringError
      exception Malformed of Error.t
      exception Unimplemented

    (* This function singles steps an atomic process of form run[a](m) according
     * rules R1-R7. *)
      fun progress (a : Chan.t, m : Cmd.t) : term State.t =
        let
          fun pRun m = EC.initial (a, m)
        in
          case Cmd.out m of
            Cmd.NewChn (_, (_, m)) => pRun m  (* freshness guaranteed by Chan.new *)
          | Cmd.Print e => (
              case DynExp.progress e of
                StateExp.Step e' => pRun (Cmd.Print' e')
              | StateExp.Val v => (
                  print (
                    case Exp.out v of
                      Exp.String s => valOf (String.fromString s)
                    | Exp.Num n    => Int.toString n
                    | _            => raise Malformed "unable to print."
                  )
                ; pRun (Cmd.Ret' Exp.Triv')
                )
            )

          | _ => raise Unimplemented
        end
    end
  end
