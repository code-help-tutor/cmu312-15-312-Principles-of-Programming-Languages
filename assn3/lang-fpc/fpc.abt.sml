structure Int =
  struct
    type t = int
    val equal : t * t -> bool = op =
    val toString = Int.toString
  end

structure Label =
  struct
    type t = string
    val equal : t * t -> bool = op =
    val toString = fn s => "\"" ^ String.toString s ^ "\""

    val eq = equal
    val compare = String.compare
  end

structure Labeled =
  let
    structure Dict = RedBlackDict (structure Key = Label)
  in
    struct
      open Dict

      type 'a t = 'a dict

      val equal = fn equal => fn (d1, d2) =>
        Dict.size d1 = Dict.size d2 andalso
        Dict.foldl
          (fn (k, v1, b) => b andalso
            case Dict.find d2 k of
              NONE => false
            | SOME v2 => equal (v1, v2))
          true
          d1

      val toString = fn toString => fn d =>
        let
          val show = fn (k, v) => Label.toString k ^ " ↪ " ^ toString v
          fun aux [] = ""
            | aux [x] = show x
            | aux (x :: xs) = show x ^ ", " ^ aux xs
        in
          "[" ^ aux (Dict.toList d) ^ "]"
        end

      val iter = fn f => fn (d1, s) =>
        Dict.foldl
          (fn (k, v1, (d2, s)) =>
            let
              val (v2, s) = f (v1, s)
            in
              (Dict.insert d2 k v2, s)
            end
          )
          (Dict.empty, s)
          d1
    end
  end

structure FPC =
  FPC (
    structure Int = Int
    structure Label = Label
    structure Labeled = Labeled
  )
