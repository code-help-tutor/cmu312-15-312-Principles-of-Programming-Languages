functor Queue (val random: bool) :> QUEUE =
let
  val frontSimple = fn nil => NONE | x :: xs => SOME (x, xs)

  local
    fun /> (x, f) = f x
    infix 0 />

    fun removeNth (l, 0) = List.tl l
      | removeNth ([], n) = raise Fail "invalid"
      | removeNth (x :: xs, n) =
          x :: removeNth (xs, n - 1)
  in
    val frontRandom =
      fn nil => NONE
       | q =>
        let
          val i = MTRand.randInt (List.length q)
          val nth = List.nth (q, i)
          val q' = removeNth (q, i)
        in
          SOME (nth, q')
        end
  end
in
  struct
    type 'a t = 'a list

    val empty = nil
    val singleton = fn x => x :: nil
    val insert = fn q => fn x => q @ [x]
    val front = fn q => if random then frontRandom q else frontSimple q

    val append = op@

    val size = List.length

    val map = List.map
    val foldl = List.foldl
    val filter = List.filter

    val remove = fn eq => fn q => fn x => List.filter (not o Fn.curry eq x) q

    val toList = Fn.id
    val toString = fn f =>
      fn q => "[" ^ String.concatWith ", " (List.map f q) ^ "]"
  end
end
