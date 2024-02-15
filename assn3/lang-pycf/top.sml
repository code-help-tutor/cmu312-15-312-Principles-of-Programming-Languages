structure TopLevelImpl = struct
  structure Object = PyCF.Object

  type term = Object.t

  datatype cmd
    = Load of Object.t
    | Step of Object.t option
    | Eval of Object.t option
    | Trans of Object.t option

  datatype mem
    = None
    | Next of Object.t
    | Val of Object.t
    | Err of Object.t

  fun init () = None

  fun toString mem =
    case mem of
      None => ""
    | Next e => " |--> " ^ Object.toString e
    | Val e => " " ^ Object.toString e ^ " VAL"
    | Err e => " " ^ Object.toString e ^ " ERR"

  val parse = Directive.map PyCFBind.bind o Parser.parse

  val bind =
   fn Directive.Load term => Load term
    | Directive.Step termopt => Step termopt
    | Directive.Eval termopt => Eval termopt
    | Directive.Trans termopt => Trans termopt
    | Directive.Use file => Load (PyCFBind.bind (Parser.parseFile Parser.parseProg file))

  fun step d =
    case PyCFDynamics.view (PyCFDynamics.trystep d) of
      PyCFDynamics.Step d' => Next d'
    | PyCFDynamics.Val => Val d
    | PyCFDynamics.Err => Err d

  val eval = Val o PyCFDynamics.eval

  fun trans d =
    let
      val e = Translate.translate d
      val () = TextIO.print ("Translation: " ^ FPC.Exp.toString e ^ "\n")
      val () = TextIO.print ("Translation type: " ^ FPC.Typ.toString (FPCChecker.synthtype FPCChecker.Context.empty e) ^ "\n")
      val v = FPCDynamics.eval e
      val () = TextIO.print ("Translation value: " ^ FPC.Exp.toString v ^ "\n")
      val () = TextIO.print ("Translation value type: " ^ FPC.Typ.toString (FPCChecker.synthtype FPCChecker.Context.empty v) ^ "\n")
    in
      TextIO.print "Leaving PyCF expression unaffected.\n"
    end

  fun get str mem eopt =
    case eopt of
      NONE => (
        case mem of
          Next e => e
        | _ => raise Fail ("Nothing to " ^ str ^ "!")
      )
    | SOME e => e

  fun runcmd cmd mem =
    case cmd of
      Load e => Next e
    | Step eopt => step (get "step" mem eopt)
    | Eval eopt => eval (get "eval" mem eopt)
    | Trans eopt => mem before trans (get "trans" mem eopt)

  fun hdl exn =
    TextIO.print (
      case exn of
        (Lexer.Error | Parser.Error) => "Parse error\n"
      | PyCFBind.Error str => str
      | PyCFDynamics.Malformed => "Malformed error in dynamics\n"
      | PyCFDynamics.RuntimeError => "Runtime error in dynamics\n"
      | Fail s => "Fail: " ^ s ^ "\n"
      | _ => "Unexpected error: " ^
              exnName exn ^ " [" ^
              exnMessage exn ^ "]\n"
    )
end

structure TopLevel = TopLevelFn (TopLevelImpl)
