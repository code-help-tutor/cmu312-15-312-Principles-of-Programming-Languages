functor TopLevelCommands (type 'a t): TOP_LEVEL_COMMANDS =
struct
  type t = (CA.Chan.t * CA.Cmd.t) t

  datatype cmd =
    TypeDef of CA.Typ.Var.t * CA.Typ.t
  | TermDef of CA.Exp.Var.t * CA.Exp.t
  | Command of CA.Cmd.t
  | Load of string

  type assertion = CA.Exp.t

  datatype res =
    TypeDefRes of CA.Typ.Var.t * CA.Typ.t
  | TermDefRes of CA.Exp.Var.t * CA.Exp.t * CA.Typ.t
  | CommandRes of t
  | LoadRes
end
