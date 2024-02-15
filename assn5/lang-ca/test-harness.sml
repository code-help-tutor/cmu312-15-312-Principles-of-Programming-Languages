structure TestHarness :> TEST_HARNESS =
struct
  structure Chan = struct open CharOrdered val toString = Char.toString end

  structure Proc =
  struct
    type t = int * string
    val equal: t * t -> bool = (op=)
    val toString = fn (i, s) =>
      "(" ^ Int.toString i ^ ", " ^ String.toString s ^ ")"
  end

  structure Msg = struct type t = string val toString = String.toString end

  val listToString = fn f =>
    fn l => "[" ^ String.concatWith ", " (List.map f l) ^ "]"

  structure EC =
    ExecutionContext
      (structure Chan = Chan
       structure Msg = Msg
       structure Queue = Queue(val random = true))

  type context = Proc.t EC.t

  val runTests = fn verbose =>
    let
      fun proc (id: int) (msg: string) : Proc.t = (id, msg)

      val logprint = if verbose then print else ignore
      val log = fn s => logprint ("> " ^ s ^ "\n")

      infix 4 >>= >=>
      datatype 'a result = Ok of 'a | Error of string
      val return = Ok
      fun (x: context result) >>= (f: context -> context result) :
        context result =
        case x of
          Ok y => (logprint (EC.toString Proc.toString y); f y)
        | Error s => Error s
      fun (f >=> g) x =
        return x >>= f >>= g

      fun addProcess p ctx =
        ( log ("Adding process: " ^ Proc.toString p)
        ; return (EC.conc (EC.initial p, ctx))
        )
      fun send c m ctx =
        ( log ("Sending " ^ Msg.toString m ^ " on channel " ^ Chan.toString c)
        ; return (EC.conc (EC.send c m, ctx))
        )
      fun accept cs w ctx =
        ( log ("Adding waiter to channel " ^ Chan.toString cs)
        ; return (EC.conc (EC.recv cs (EC.initial o w), ctx))
        )

      fun mem (x: Proc.t) : Proc.t list -> bool =
        List.exists (Fn.curry Proc.equal x)
      fun remP (i: int) : Proc.t list -> Proc.t list =
        List.filter (not o Fn.curry (op=) i o #1)
      fun remM (m: string) : Proc.t list -> Proc.t list =
        List.filter (not o Fn.curry (op=) m o #2)

      fun expectNone ctx =
        case EC.chooseProcess ctx of
          (NONE, ctx) => return ctx
        | (SOME p, ctx) =>
            Error
              ("expected no ready processes, received process "
               ^ Proc.toString p)

      fun expect ps k ctx =
        case EC.chooseProcess ctx of
          (NONE, ctx) => Error ("expected a ready process, did not receive one")
        | (SOME p, ctx) =>
            if mem p ps then
              k p ctx
            else
              Error
                ("expected a ready process in " ^ listToString Proc.toString ps
                 ^ ", received process " ^ Proc.toString p)

      val tests =
        [ addProcess (proc 0 "aardvark")
          >=> expect [(0, "aardvark")] (fn _ => expectNone)
        , addProcess (proc 0 "bluebird") >=> addProcess (proc 1 "chameleon")
          >=>
          expect [proc 0 "bluebird", proc 1 "chameleon"] (fn (i, _) =>
            expect (remP i [proc 0 "bluebird", proc 1 "chameleon"]) (fn _ =>
              expectNone))
        , addProcess (proc 0 "dolphin") >=> send #"a" "elephant"
          >=> accept #"a" (proc 1)
          >=>
          expect [proc 0 "dolphin", proc 1 "elephant"] (fn (i, _) =>
            expect (remP i [proc 0 "dolphin", proc 1 "elephant"]) (fn _ =>
              expectNone))
        , addProcess (proc 0 "falcon") >=> accept #"a" (proc 1)
          >=> expect [proc 0 "falcon"] (fn _ => expectNone) >=> send #"a" "goat"
          >=> expect [proc 1 "goat"] (fn _ => expectNone)
          >=> send #"a" "hummingbird" >=> expectNone
        , accept #"a" (proc 0) >=> expectNone >=> send #"a" "iguana"
          >=> send #"a" "jaguar"
          >=>
          expect [proc 0 "iguana", proc 0 "jaguar"] (fn (_, s) =>
            accept #"a" (proc 1)
            >=>
            expect (remM s [proc 1 "iguana", proc 1 "jaguar"]) (fn _ =>
              expectNone)) >=> accept #"a" (proc 2) >=> expectNone
        , accept #"a" (proc 0) >=> accept #"a" (proc 1) >=> send #"a" "kangaroo"
          >=>
          expect [proc 0 "kangaroo", proc 1 "kangaroo"] (fn (i, _) =>
            expectNone >=> send #"a" "llama"
            >=>
            expect (remP i [proc 0 "llama", proc 1 "llama"]) (fn _ =>
              expectNone))
        , accept #"a" (proc 0) >=> accept #"a" (proc 1) >=> send #"a" "macaw"
          >=> send #"a" "newt"
          >=>
          expect [proc 0 "macaw", proc 0 "newt", proc 1 "macaw", proc 1 "newt"]
            (fn (i, s) =>
               expect
                 (remM s (remP i
                    [ proc 0 "macaw"
                    , proc 0 "newt"
                    , proc 1 "macaw"
                    , proc 1 "newt"
                    ])) (fn _ => expectNone))
        , send #"a" "ocelot" >=> send #"a" "platypus" >=> accept #"a" (proc 0)
          >=> accept #"a" (proc 1)
          >=>
          expect
            [ proc 0 "ocelot"
            , proc 0 "platypus"
            , proc 1 "ocelot"
            , proc 1 "platypus"
            ]
            (fn (i, s) =>
               expect
                 (remM s (remP i
                    [ proc 0 "ocelot"
                    , proc 0 "platypus"
                    , proc 1 "ocelot"
                    , proc 1 "platypus"
                    ])) (fn _ => expectNone))
        , accept #"a" (proc 0) >=> accept #"b" (proc 1) >=> send #"b" "quokka"
          >=> send #"b" "raccoon"
          >=>
          expect [proc 1 "quokka", proc 1 "raccoon"] (fn (_, s) =>
            send #"a" s >=> expect [proc 0 s] (fn _ => expectNone))
        , accept #"a" (proc 0) >=> send #"a" "sloth" >=> send #"a" "turtle"
          >=>
          expect [proc 0 "sloth", proc 0 "turtle"] (fn (_, s) =>
            expectNone >=> accept #"a" (proc 1) >=> accept #"a" (proc 2)
            >=>
            expect
              [ proc 1 (if s = "sloth" then "turtle" else "sloth")
              , proc 2 (if s = "sloth" then "turtle" else "sloth")
              ]
              (fn (i, _) =>
                 expectNone >=> send #"a" "urchin"
                 >=>
                 expect [proc (if i = 1 then 2 else 1) "urchin"] (fn _ =>
                   expectNone)))
        , accept #"a" (proc 0) >=> accept #"b" (proc 1) >=> send #"a" "viper"
          >=> send #"b" "walrus"
          >=>
          expect [proc 0 "viper", proc 1 "walrus"] (fn (i, s) =>
            expect (remP i [proc 0 "viper", proc 1 "walrus"]) (fn _ =>
              expectNone))
        , accept #"a" (proc 0) >=> send #"a" "xerus"
          >=> expect [proc 0 "xerus"] (fn _ => expectNone) >=> send #"a" "yak"
          >=> expectNone >=> accept #"a" (proc 1)
          >=> expect [proc 1 "yak"] (fn _ => expectNone)
        ]
    in
      List.map
        (fn test =>
           fn () =>
             (case test EC.empty of
                Error s => SOME ("Test failed! " ^ s ^ "\n")
              | Ok _ => NONE)
             handle e =>
               SOME ("Test failed with exception: " ^ exnMessage e ^ "\n"))
        tests
    end

  val tests = runTests false

  val testEC = fn verbose =>
    List.app
      (fn test =>
         print
           (case test () of
              NONE => "Test passed.\n"
            | SOME s => s)) (runTests verbose)
end
