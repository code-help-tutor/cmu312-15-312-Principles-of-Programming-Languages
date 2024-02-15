structure ParserMA = Parser (
        structure TopLevelCommands = TopLevelCommands
        structure Desugar = Desugar
        )
structure InterpreterMA =
InterpreterMA (structure Dynamics = DynamicsMA
               structure Parser = ParserMA)
