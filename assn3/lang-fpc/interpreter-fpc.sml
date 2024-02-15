structure InterpreterFPC =
  Interpreter
    (TypedInterpreterImpl
       (structure Parser = ParserFPC
        structure Statics = StaticsFPC
        structure Dynamics = DynamicsFPC))
