signature DESUGAR =
sig

    (* do(e) =def= bnd(e; x.ret(x)) *)
    val cmddo : MA.Cmd.exp -> MA.Cmd.cmd

    (* cmdlet x = m1 in m2 =def= bnd(cmd(m1); x.m2) *)
    val cmdcmdlet : MA.Exp.cmd * (MA.Cmd.expVar * MA.Cmd.cmd) -> MA.Cmd.cmd

    (* [x, y] (x + y) =def= bnd(cmd(get(x)); x. bnd(cmd(get(y)); y. ret(x + y))) *)
    val cmdgetvars : (MA.Ref.t * MA.Cmd.expVar) list * MA.Cmd.exp -> MA.Cmd.cmd

    (* [x, y] { set x := x + y } =def= bnd(cmd(get(x)); x.bnd(cmd(get(y)); y. set<x>(x + y))) *)
    val cmdgetvars' : (MA.Ref.t * MA.Cmd.expVar) list * MA.Cmd.cmd -> MA.Cmd.cmd

    val cmdignore : MA.Exp.cmd -> MA.Cmd.cmd

    val cmdseq : MA.Exp.cmd * MA.Cmd.cmd -> MA.Cmd.cmd

    val cmdif : MA.Exp.cmd * MA.Exp.cmd * MA.Exp.cmd -> MA.Cmd.cmd

    val cmdwhile : MA.Exp.cmd * MA.Exp.cmd -> MA.Cmd.cmd
end