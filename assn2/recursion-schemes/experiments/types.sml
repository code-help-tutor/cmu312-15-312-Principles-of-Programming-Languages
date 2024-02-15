structure PrimNat : INDUCTIVE where T = NatOp =
  struct
    structure T = NatOp

    type t = int

    val FOLD =
      fn NatOp.Zero   => 0
       | NatOp.Succ n => n + 1

    val REC = fn f => fn n => Fn.repeat n (f o NatOp.Succ) (f NatOp.Zero)
  end

functor PrimList (ListOp : LIST_OP) : INDUCTIVE where T = ListOp =
  struct
    structure T = ListOp

    type t = ListOp.element list

    val FOLD =
      fn ListOp.Nil          => nil
       | ListOp.Cons (x, xs) => x :: xs

    val REC =
      fn (f : 'rho ListOp.view -> 'rho) =>
        List.foldr (f o ListOp.Cons) (f ListOp.Nil)
  end

functor PrimString (StringOp : LIST_OP where type element = char) : INDUCTIVE where T = StringOp =
  struct
    structure T = StringOp

    type t = string

    val FOLD =
      fn StringOp.Nil         => ""
       | StringOp.Cons (c, s) => s ^ str c

    val REC =
      fn f =>
        List.foldl (f o StringOp.Cons) (f StringOp.Nil) o String.explode
  end

functor Stream (ListOp : LIST_OP) = Coinductive (ListOp)

(* transparent buffered LIST_OP stream, so user can see upcoming elements *)
functor BufferedStream (ListOp : LIST_OP) : COINDUCTIVE where T = ListOp =
  struct
    structure T = ListOp

    datatype t = HIDE of ListOp.element list * (unit -> t) option

    val TO_SHOW = 17  (* default NJ printDepth = 16; will trigger "..." *)

    fun GEN (f : 'sigma -> 'sigma T.view) (s : 'sigma) : t =
      let
        fun aux 0 s acc = HIDE (List.rev acc, SOME (fn () => GEN f s))
          | aux n s acc = (
              case f s of
                ListOp.Nil         => HIDE (List.rev acc, NONE)
              | ListOp.Cons (x, s) => aux (n - 1) s (x :: acc)
            ) handle _ => HIDE (List.rev acc, NONE)
      in
        aux TO_SHOW s nil
      end

    fun UNFOLD (HIDE (l, susp)) =
      case l of
        nil     => (
          case susp of
            NONE => ListOp.Nil
          | SOME susp => UNFOLD (susp ())
        )
      | x :: xs => ListOp.Cons (x, HIDE (xs, susp))
  end
