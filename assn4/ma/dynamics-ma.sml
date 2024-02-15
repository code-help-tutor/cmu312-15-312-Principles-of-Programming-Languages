structure DynamicsMA :> DYNAMICS
  where State = StateMA
    and type term = MA.Cmd.t
=
let
  structure Typ = MA.Typ
  structure Exp = MA.Exp
  structure Cmd = MA.Cmd
  structure StateExp = StateMAExp
  structure DynExp = DynamicsMAExp
  structure MemDict = StateMA.MemDict
in
  struct
    structure State = StateMA

    type term = Cmd.t

    structure Error = StringError
    exception Malformed of Error.t

    fun progress (cmd: Cmd.t) : Cmd.t State.t =
      fn (mem: StateMA.memory) =>
        case Cmd.out cmd of
          Cmd.Print e =>
            (case DynExp.progress e of
               StateExp.Step e' => State.Step (Cmd.Print' e', mem)
             | StateExp.Val v =>
                 ( print
                     (case Exp.out v of
                        Exp.String s => valOf (String.fromString s)
                      | Exp.Num n => Int.toString n
                      | _ => raise Malformed "unable to print.")
                 ; State.Step (Cmd.Ret' Exp.Triv', mem)
                 ))
        | _ => raise Fail "unimplemented"

  end
end