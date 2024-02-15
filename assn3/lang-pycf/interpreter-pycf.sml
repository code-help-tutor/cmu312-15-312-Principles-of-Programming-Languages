structure InterpreterPyCF =
  Interpreter
    (TypedInterpreterImpl
       (structure Parser = ParserPyCF
        structure Statics = StaticsPyCF
        structure Dynamics = DynamicsPyCF))
