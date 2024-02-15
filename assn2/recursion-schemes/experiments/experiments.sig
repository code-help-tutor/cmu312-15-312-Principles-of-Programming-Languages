signature EXPERIMENTS =
  sig
    structure Nat : INDUCTIVE where T = NatOp
    type nat = Nat.t

    structure ListOp : LIST_OP
    type element = ListOp.element
    structure List : INDUCTIVE where T = ListOp
    type list = List.t
    structure Stream : COINDUCTIVE where T = ListOp
    type stream = Stream.t

    structure StringOp : LIST_OP where type element = char
    structure String : INDUCTIVE where T = StringOp
    type string = String.t
    structure Automaton : COINDUCTIVE where T = AutomatonOp
    type automaton = Automaton.t

    structure NatUtil :
      sig
        val double : nat -> nat
        val exp2 : nat -> nat

        val halve : nat -> nat

        val add : nat * nat -> nat
        val fib : nat -> nat
      end

    structure ListUtil :
      sig
        val length : list -> nat
        val sum : list -> nat

        val map : (element -> element) -> list -> list
        val filter : (element -> bool) -> list -> list

        val reverse : list -> list

        val UNFOLD : list -> list ListOp.view
        val REC' : (('rho * list) ListOp.view -> 'rho) -> list -> 'rho
      end

    structure InsertionSort :
      sig
        val insert : element * list -> list
        val sort : list -> list
      end

    structure MergeSort :
      sig
        val split : list -> list * list
        val merge : list * list -> list
        val sort : list -> list
      end

    structure StreamUtil :
      sig
        val fromList : list -> stream
        val map : (element -> element) -> stream -> stream
        val zipWith : (element * element -> element) -> stream * stream -> stream
      end

    structure AutomatonUtil :
      sig
        val notConsecutiveA : automaton
        val abc : automaton

        val run : automaton -> string -> bool

        val endsWithA : automaton
        val abStar : automaton
        val either : automaton * automaton -> automaton
      end
  end
