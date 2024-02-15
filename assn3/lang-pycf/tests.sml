structure TestHarness :> TESTHARNESS =
struct
  structure UD = PyCFDynamics
  structure TLI = TopLevelImpl
  structure TL = TopLevel
  structure Term = PyCF.Object

  datatype dynamics = Step of PyCF.Object.t | Val | Err

  datatype 'a expected_result =
    Pass of 'a
  | Fail
  | TypeFail
  | ParseFail
  | EvalFail

  local open Term
  in
    fun Num n = Term.Int' n
    fun num_list nums =
      Term.List' (List.map Num nums)
  end

  type test = string * (Term.t expected_result)
  val tests: test list =
    [ ("tests/easy.pycf", Pass (Term.Int' 2))
    , ("tests/int.pycf", Pass (Term.Int' 6))
    , ( "tests/lambda.pycf"
      , Pass
          (let val x = Term.Var.new "x"
           in Term.Fun' (Term.Var.new "f", (x, Term.Var' x))
           end)
      )
    , ("tests/triangle.pycf", Pass (Term.Int' 55))
    , ("tests/sum.pycf", Pass (Term.Int' 12))
    , ("tests/sum_unsafe.pycf", EvalFail)
    , ("tests/sum_safe.pycf", Pass (Term.Int' 12))
    , ("tests/index.pycf", Pass (Term.Int' 1))
    , ("tests/err.pycf", EvalFail)
    , ("tests/map.pycf", Pass (num_list [2, 6, 4, 2, 3]))
    , ("tests/filter.pycf", Pass (num_list [5, 3]))
    , ("tests/just_ints.pycf", Pass (num_list [3, 1, 2]))
    , ("tests/plus_bool.pycf", Pass (num_list [5, 3, 2, 1]))
    , ("tests/mixtype.pycf", Pass (Term.Int' 1))
    , ( "tests/concat.pycf"
      , Pass (Term.List'
          [ Term.Int' 1
          , Term.Int' 5
          , Term.List' []
          , Term.Int' 3
          , Term.Bool' true
          , Term.Int' 2
          ])
      )
    , ( "tests/flatten.pycf"
      , Pass (Term.List'
          [Term.Int' 1, Term.Int' 5, Term.Int' 3, Term.Bool' true, Term.Int' 2])
      )
    , ("tests/y_combinator.pycf", Pass (Term.Int' 64))
    , ("tests/add_contract.pycf", EvalFail)
    , ("tests/quicksorteasy.pycf", Pass (num_list [122, 150]))
    , ("tests/quicksort.pycf", Pass (num_list [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))
    ]

  fun test_dir file = OS.Path.joinDirFile {dir = "tests", file = file}

  fun test_files () =
    let
      val dir = OS.FileSys.openDir "tests"
      fun all dir acc =
        case OS.FileSys.readDir dir of
          SOME file =>
            (case OS.Path.ext file of
               SOME "dyn" => all dir (file :: acc)
             | _ => all dir acc)
        | _ => acc
      val files = all dir []
      val _ = OS.FileSys.closeDir dir
    in
      map test_dir files
    end

  fun eval t =
    Pass (UD.eval t)
    handle UD.RuntimeError => EvalFail | UD.Malformed => EvalFail

  fun res_eq (t1, t2) =
    case (t1, t2) of
      (Pass n1, Pass n2) => Term.aequiv (n1, n2)
    | (Fail, Fail) => true
    | (TypeFail, TypeFail) => true
    | (ParseFail, ParseFail) => true
    | (EvalFail, EvalFail) => true
    | _ => false

  fun vprint verb s =
    if verb then print s else ()

  fun success verb file =
    (if verb then print (file ^ ": Success!\n") else (); true)

  fun hdl verb n f file pass =
    let
      val (exp, exp_res) = f file
    in
      let
        val passed = pass (exp, exp_res)
      in
        if passed then success verb file
        else (vprint verb (file ^ ": Failed: Result does not match\n"); false)
      end
    end
    handle
      Parser.Error => (vprint verb (file ^ ": Failed: Parse Error\n"); false)
    | UD.Malformed =>
        ( vprint verb
            (file ^ ": Failed: Malformed Error in InexhaustiveDynamics\n")
        ; false
        )
    | exn => raise exn

  exception TestError

  fun runtestFile v ((file, exp_res), (L, n)) =
    let
      fun f s =
        (case TL.evalFile s of
           TLI.Val exp => (Pass exp, exp_res)
         | _ => (Fail, exp_res))
        handle
          UD.Malformed => (EvalFail, exp_res)
        | UD.RuntimeError => (EvalFail, exp_res)
      val pass = fn (e, res) => res_eq (e, res)
    in
      ((hdl v n f file pass) :: L, n + 1)
    end

  fun summarize [] (pass, fail) =
        ( (if fail > 0 then
             TextIO.print
               "-------------------------------------------------------\n"
           else
             ())
        ; TextIO.print
            ("\nTests completed: " ^ (Int.toString (pass + fail)) ^ "\n")
        ; TextIO.print ("Tests passed   : " ^ (Int.toString pass) ^ "\n")
        ; TextIO.print ("Tests failed   : " ^ (Int.toString fail) ^ "\n")
        ; if fail = 0 then TextIO.print "Congratulations!\n" else ()
        )
    | summarize (result :: results) (pass, fail) =
        let val stats' = if result then (pass + 1, fail) else (pass, fail + 1)
        in summarize results stats'
        end

  fun runfiletests verbose =
    ( print "\n\nRunning file tests...\n"
    ; summarize (#1 (foldl (runtestFile verbose) ([], 1) (tests))) (0, 0)
    )
end
