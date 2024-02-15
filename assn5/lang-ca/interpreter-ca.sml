structure Executor =
  Executor (structure EC = ExecutionContext structure Dynamics = DynamicsCA)
structure TLC = TopLevelCommands(ExecutionContext)
structure Parser = Parser(TLC)
structure InterpreterCA =
  InterpreterCA
    (structure Dynamics = DynamicsCA
     structure Executor = Executor
     structure TopLevelCommands = TLC
     structure Parser = Parser)
