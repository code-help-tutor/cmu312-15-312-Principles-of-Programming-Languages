structure ExecutionContext =
let open CA
in
  ExecutionContext
    (structure Chan = struct open Chan val eq = equal end

     structure Msg = Exp

     structure Queue = Queue(val random = true))
end
