functor TypedInterpreterImpl
  (structure Parser : PARSER
   structure Statics : STATICS
   structure Dynamics : DYNAMICS
   sharing type Parser.directive = Statics.Term.t = Dynamics.term) :>
  INTERPRETER_IMPL
    where Parser = Parser
      and type state = unit
  =
  let
    structure Evaluator = ContEvaluator (Dynamics)
  in
    struct
      structure Parser = Parser

      type state = unit
      val init = ()

      fun println s = TextIO.print (s ^ "\n")

      fun runDirective (term : Statics.Term.t) () : unit =
        let
          val () =
            let
              val () = println (Statics.Term.toString term)
              val typ = Statics.inferType Statics.Context.empty term
              val () = println ("Type: " ^ Statics.Typ.toString typ)
              val () = TextIO.print "Evaluating... "
              val final = Evaluator.evaluate (Dynamics.State.initial term)
              val () = println (Dynamics.State.toString Void.absurd final)
            in
              ()
            end
              handle Statics.TypeError error => println ("Type Error: " ^ Statics.Error.toString error)
                   | Dynamics.Malformed error => println ("Malformed: " ^ Dynamics.Error.toString error)
                   | exn => println ("Unexpected error: " ^ General.exnName exn ^ " [" ^ General.exnMessage exn ^ "]")
        in
          println ""
        end
    end
  end
