structure InterpreterPSF =
  Interpreter
    (TypedInterpreterImpl
       (structure Parser = ParserPSF
        structure Statics = StaticsPSF
        structure Dynamics = DynamicsPSF))
