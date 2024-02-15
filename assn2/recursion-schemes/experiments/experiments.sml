structure Experiments =
  Experiments
    (structure Nat = PrimNat
     val op<= = Int.<=

     structure ListOp = ListOp(type element = int)
     structure List = PrimList(ListOp)
     structure Stream = BufferedStream(ListOp)

     structure StringOp = ListOp(type element = char)
     structure String = PrimString(StringOp)
     structure Automaton = Coinductive(AutomatonOp))
