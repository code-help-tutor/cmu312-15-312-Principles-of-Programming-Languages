structure Context =
  SplayDict
    (structure Key =
     struct
       type t = Variable.t
       val eq = Variable.equal
       val compare = Variable.compare
     end)
