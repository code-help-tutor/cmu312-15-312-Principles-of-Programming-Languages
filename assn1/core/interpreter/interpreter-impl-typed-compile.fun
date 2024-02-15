functor TypedCompileInterpreterImpl
  (structure Parser : PARSER
   structure StaticsSource : STATICS
   structure StaticsTarget : STATICS
   structure DynamicsTarget : DYNAMICS
   sharing type Parser.directive = StaticsSource.Term.t
   sharing type StaticsTarget.Term.t = DynamicsTarget.term
   val compile : StaticsSource.Term.t -> StaticsTarget.Term.t) :>
  INTERPRETER_IMPL
    where Parser = Parser
      and type state = unit
  =
  let
    structure Evaluator = ContEvaluator (DynamicsTarget)
  in
    struct
      structure Parser = Parser

      type state = unit
      val init = ()

      fun println s = TextIO.print (s ^ "\n")

      fun runDirective (term : StaticsSource.Term.t) () : unit =
        let
          val () =
            let
              val () = println (StaticsSource.Term.toString term)
              val typ = StaticsSource.inferType StaticsSource.Context.empty term
              val () = println ("Type (source): " ^ StaticsSource.Typ.toString typ)

              val () = println "Compiling... "
              val term' = compile term

              val () = println (StaticsTarget.Term.toString term')
              val typ' = StaticsTarget.inferType StaticsTarget.Context.empty term'
              val () = println ("Type (target): " ^ StaticsTarget.Typ.toString typ')

              val () = TextIO.print "Evaluating... "
              val final = Evaluator.evaluate (DynamicsTarget.State.initial term')
              val () = println (DynamicsTarget.State.toString Void.absurd final)
            in
              ()
            end
              handle StaticsSource.TypeError error => println ("Type Error: " ^ StaticsSource.Error.toString error)
                   | StaticsTarget.TypeError error => println ("Type Error: " ^ StaticsTarget.Error.toString error)
                   | DynamicsTarget.Malformed error => println ("Malformed: " ^ DynamicsTarget.Error.toString error)
                   | exn => println ("Unexpected error: " ^ General.exnName exn ^ " [" ^ General.exnMessage exn ^ "]")
        in
          println ""
        end
    end
  end
