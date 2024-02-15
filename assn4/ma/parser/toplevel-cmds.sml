structure TopLevelCommands : TOP_LEVEL_COMMANDS =
struct
  type t = MA.Exp.t

  datatype cmd =
    TypeDef of MA.Typ.Var.t * MA.Typ.t
  | TermDef of MA.Exp.Var.t * MA.Exp.t
  | Command of MA.Cmd.t
  | Load of string

  type assertion = MA.Exp.t

  datatype res =
    TypeDefRes of MA.Typ.Var.t * MA.Typ.t
  | TermDefRes of MA.Exp.Var.t * MA.Exp.t * MA.Typ.t
  | CommandRes of t
  | LoadRes
end
