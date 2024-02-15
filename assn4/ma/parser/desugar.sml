structure Desugar :> DESUGAR =
struct

  fun cmddo mExp =
    let val x = MA.Exp.Var.new "x"
    in MA.Cmd.Bind' (mExp, (x, MA.Cmd.Ret' (MA.Exp.Var' x)))
    end

  fun cmdcmdlet (m1, (x, m2)) =
    MA.Cmd.Bind' (MA.Exp.Cmd' m1, (x, m2))

  fun cmdgetvars (xs, e) =
    case xs of
      [] => MA.Cmd.Ret' e
    | (a, x) :: xs =>
        MA.Cmd.Bind' (MA.Exp.Cmd' (MA.Cmd.Get' a), (x, cmdgetvars (xs, e)))

  fun cmdgetvars' (xs, m) =
    case xs of
      [] => m
    | (a, x) :: xs =>
        MA.Cmd.Bind' (MA.Exp.Cmd' (MA.Cmd.Get' a), (x, cmdgetvars' (xs, m)))

  fun cmdignore m = MA.Cmd.Ret' (MA.Exp.Num' 1) (* To be implemented *)

  fun cmdseq (m1, m2) = MA.Cmd.Ret' (MA.Exp.Num' 1) (* To be implemented *)

  fun cmdif (m, m1, m2) = MA.Cmd.Ret' (MA.Exp.Num' 1) (* To be implemented *)

  fun cmdwhile (m1, m2) = MA.Cmd.Ret' (MA.Exp.Num' 1) (* To be implemented *)
end