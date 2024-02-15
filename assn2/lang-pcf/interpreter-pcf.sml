structure InterpreterPCF =
  Interpreter
    (TypedInterpreterImpl
       (structure Parser = ParserPCF
        structure Statics = StaticsPCF
        structure Dynamics = DynamicsPCF))
