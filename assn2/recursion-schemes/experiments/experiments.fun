functor Experiments (
  structure Nat : INDUCTIVE where T = NatOp
  val <= : Nat.t * Nat.t -> bool

  structure ListOp : LIST_OP where type element = Nat.t
  structure List : INDUCTIVE where T = ListOp
  structure Stream : COINDUCTIVE where T = ListOp

  structure StringOp : LIST_OP where type element = char
  structure String : INDUCTIVE where T = StringOp
  structure Automaton : COINDUCTIVE where T = AutomatonOp
) :> EXPERIMENTS where Nat = Nat
               and ListOp = ListOp
               and List = List
               and Stream = Stream
               and StringOp = StringOp
               and String = String
               and Automaton = Automaton =
  struct
    structure N = NatOp
    structure Nat = Nat
    type nat = Nat.t

    structure L = ListOp
    structure ListOp = ListOp
    type element = L.element
    structure List = List
    type list = List.t

    structure Stream = Stream
    type stream = Stream.t

    structure StringOp = StringOp
    structure String = String
    type string = String.t

    structure Automaton = Automaton
    type automaton = Automaton.t

    val ZERO = Nat.FOLD N.Zero
    val SUCC = Nat.FOLD o N.Succ

    val NIL = List.FOLD L.Nil
    val CONS = List.FOLD o L.Cons

    structure NatUtil =
      struct
        val double : nat -> nat =
          Nat.REC
            (fn N.Zero   => ZERO
              | N.Succ n => SUCC (SUCC n))

        val exp2 : nat -> nat =
          fn _ => raise Fail "Unimplemented"

        val halve : nat -> nat =
          fn _ => raise Fail "Unimplemented"

        val add : nat * nat -> nat =
          fn (m, n) =>
            Nat.REC
              (fn N.Zero   => n
                | N.Succ r => SUCC r)
              m

        val fib : nat -> nat =
          fn _ => raise Fail "Unimplemented"
      end

    structure ListUtil =
      struct
        val length : list -> nat =
          List.REC
            (fn L.Nil => ZERO
              | L.Cons (_, n) => SUCC n)

        val sum : list -> nat =
          fn _ => raise Fail "Unimplemented"

        val map =
          fn f =>
            List.REC
              (fn L.Nil          => NIL
                | L.Cons (x, xs) => CONS (f x, xs))

        val filter : (element -> bool) -> list -> list =
          fn p => raise Fail "Unimplemented"

        val reverse : list -> list =
          fn _ => raise Fail "Unimplemented"

        val UNFOLD : list -> list L.view =
          fn l => raise Fail "Unimplemented"

        val REC' : (('rho * list) L.view -> 'rho) -> list -> 'rho =
          fn (f : ('rho * list) L.view -> 'rho) =>
            raise Fail "Unimplemented"
      end

    structure InsertionSort =
      struct
        val insert : nat * list -> list =
          fn (x, l) => raise Fail "Unimplemented"

        val sort : list -> list =
          fn l => raise Fail "Unimplemented"
      end

    structure MergeSort =
      struct
        val split =
          List.REC
            (fn L.Nil => (NIL, NIL)
              | L.Cons (x, (l1, l2)) => (l2, CONS (x, l1)))

        val merge =
          fn (l1, l2) => raise Fail "Unimplemented"

        val sort =
          fn l =>
            Nat.REC
              (fn N.Zero => (fn l => l)
                | N.Succ f =>
                    fn l =>
                      let
                        val (l1, l2) = split l
                      in
                        merge (f l1, f l2)
                      end)
              (ListUtil.length l)
              l
      end

    structure StreamUtil =
      struct
        val fromList : list -> stream =
          fn l => raise Fail "Unimplemented"

        val map : (element -> element) -> stream -> stream =
          fn f =>
            Stream.GEN
              (fn s =>
                case Stream.UNFOLD s of
                  L.Nil          => L.Nil
                | L.Cons (x, xs) => L.Cons (f x, xs))

        val zipWith : (element * element -> element) -> stream * stream -> stream =
          fn f => raise Fail "Unimplemented"
      end

    structure AutomatonUtil =
      struct
        (* M¬AA example from the writeup *)
        val notConsecutiveA =
          let
            datatype state = S1 | S2 | S3
          in
            Automaton.GEN
              (fn S1 => (true, fn #"A" => S2 | _ => S1)
                | S2 => (true, fn #"A" => S3 | _ => S1)
                | S3 => (false, fn _ => S3))
              S1
          end

        val abc =
          let
            datatype state = A | B | C | Succeed | Fail
          in
            Automaton.GEN
              (fn A => (false, fn #"A" => B | _ => Fail)
                | B => (false, fn #"B" => C | _ => Fail)
                | C => (false, fn #"C" => Succeed | _ => Fail)
                | Succeed => (true, fn _ => Fail)
                | Fail => (false, fn _ => Fail))
              A
          end

        val run : automaton -> string -> bool =
          fn automaton => raise Fail "Unimplemented"

        val endsWithA : automaton =
          Automaton.GEN (fn () => (false, fn _ => ())) ()

        val abStar : automaton =
          Automaton.GEN (fn () => (false, fn _ => ())) ()

        val either =
          fn (a1, a2) => raise Fail "Unimplemented"
      end
  end
