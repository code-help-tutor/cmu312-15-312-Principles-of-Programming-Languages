structure TestHarness :> TESTS =
struct
  structure Var = Variable
  structure TL = InterpreterKPCF

  fun vprint verb s =
    if verb then print s else ()

  fun hdl verb n f comp exp =
    let
      val res = f ()
    in
      case res of
        SOME res =>
          if comp (res, exp) then
            (vprint verb (Int.toString n ^ ": Success!\n"); true)
          else
            ( vprint verb
                (Int.toString n ^ ": Failed: Incorrect result "
                 ^ KPCFv.Value.toString res ^ "\n")
            ; false
            )
      | NONE =>
          ( vprint verb (Int.toString n ^ ": Failed: No result returned.\n")
          ; false
          )
    end
    handle
      Parser.ParseError _ =>
        (vprint verb (Int.toString n ^ ": Failed: Parse Error\n"); false)
    | Statics.TypeError s =>
        ( vprint verb (Int.toString n ^ ": Failed: Type Error [" ^ s ^ "]\n")
        ; false
        )
    | _ => (vprint verb (Int.toString n ^ ": Failed: Runtime Error\n"); false)

  fun runtestfile v ((file, exp), (L, n)) =
    ((hdl v n (fn _ => TL.evalFile file) KPCFv.Value.aequiv exp) :: L, n + 1)

  fun summarize [] (pass, fail) =
        ( if fail > 0 then
            TextIO.print
              "-------------------------------------------------------\n"
          else
            ()
        ; TextIO.print
            ("\nTests completed: " ^ Int.toString (pass + fail) ^ "\n")
        ; TextIO.print ("Tests passed   : " ^ Int.toString pass ^ "\n")
        ; TextIO.print ("Tests failed   : " ^ Int.toString fail ^ "\n")
        ; if fail = 0 then TextIO.print "Congratulations!\n" else ()
        )
    | summarize (result :: results) (pass, fail) =
        let val stats' = if result then (pass + 1, fail) else (pass, fail + 1)
        in summarize results stats'
        end

  local
    open KPCFv

    fun num 0 = Value.Zero'
      | num n =
          Value.Succ' (num (n - 1))

    val f = Variable.new "f"
    val t = Variable.new "t"
  in
    val testfiles =
      [ ("tests/one.kpcf", (num 1))
      , ("tests/split.kpcf", (Value.Tuple' (num 3, Value.Unit')))
      , ("tests/plus.kpcf", (num 8))
      , ("tests/sum.kpcf", (Value.Tuple' (num 1, num 3)))
      , ("tests/ifz.kpcf", num 1)
      , ("tests/funap.kpcf", num 2)
      , ("tests/dne.kpcf", Value.Unit')
      ]
  end

  fun runfiletests verbose =
    ( print "\n\nRunning file tests...\n"
    ; summarize (#1 (foldl (runtestfile verbose) ([], 1) testfiles)) (0, 0)
    )

  fun runalltests v = runfiletests v
end
