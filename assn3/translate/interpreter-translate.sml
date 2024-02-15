structure InterpreterTranslate =
  Interpreter
    (TypedCompileInterpreterImpl
       (structure Parser = ParserPyCF
        structure StaticsSource = StaticsPyCF
        structure StaticsTarget = StaticsFPC
        structure DynamicsTarget = DynamicsFPC
        val compile = Translate.translate))
