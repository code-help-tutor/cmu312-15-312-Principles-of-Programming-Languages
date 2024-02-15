functor TopLevelFun
  (structure ULC: ULC
   structure Normalize: NORMALIZE_ULC where type term = ULC.Term.t) :> TOPLEVEL =
struct

  (***** Convert a charstream into a string stream, breaking at ; boundaries ****)
  local
    fun revtostring L =
      String.implode (foldl (op::) [] L)
  in
    fun stringStream L s =
      case Stream.front s of
        Stream.Cons ((#";", _), s) =>
          Stream.lazy (fn () =>
            Stream.Cons (revtostring L, Stream.lazy (fn () =>
              Stream.front (stringStream [] s))))
      | Stream.Cons ((c, _), s) => stringStream (c :: L) s
      | Stream.Nil =>
          if Input.isBlank (String.implode L) then (* avoid parser errors at EOF *)
            Stream.lazy (fn () => Stream.Nil)
          else
            Stream.lazy (fn () =>
              Stream.Cons (revtostring L, Stream.lazy (fn () => Stream.Nil)))
  end

  fun foldl f x s =
    case Stream.front s of
      Stream.Nil => x
    | Stream.Cons (h, t) => foldl f (f (h, x)) t

  structure Term = ULC.Term

  structure Parser = Parser(ULC)

  fun parse text =
    Parser.parse Normalize.norm (Stream.fromString text)

  fun norm e = Parser.ParseTerm.to_term e Normalize.norm

  local
    structure Variables = SplayDict (structure Key = StringOrdered)

    (* map from user string -> string -> new name *)
    type names = string Variables.dict Variables.dict

    fun update (names: names) x =
      let
        val base = Term.Var.toUserString x
        val name = Term.Var.toString x
      in
        case Variables.find names base of
          SOME occurrences =>
            (case Variables.find occurrences name of
               SOME name => (name, names, true)
             | _ =>
                 let
                   val n = Variables.size occurrences
                   (* the new name is renumbered *)
                   val name' = Term.Var.toUserString x ^ Int.toString (n + 1)
                 in
                   ( name'
                   , Variables.insert (Variables.remove names base) base
                       (Variables.insert occurrences name name')
                   , false
                   )
                 end)
        | _ =>
            ( base
            , Variables.insert names base (Variables.singleton name base)
            , false
            )
      end

    fun toDoc t =
      let
        open PrettyDoc
        infix 2 ++ $$ //

        val update = fn names =>
          fn x => let val (x1, x2, x3) = update names x in (text x1, x2, x3) end

        (* the variant of the top node of the tree *)
        datatype top = V | A | L

        fun toDoc' t (names: names) =
          case Term.out t of
            Term.Var x =>
              let
                val (name, names, found) = update names x
              in
                (* a free variable has "_" prepended *)
                ( if found then name else (text ("_" ^ Term.Var.toUserString x))
                , names
                , V
                )
              end
          | Term.Ap (t1, t2) =>
              let
                val (s1, names, top1) = toDoc' t1 names
                val (s2, names, top2) = toDoc' t2 names
                val first = if top1 = L then parensAround s1 else s1
                val second = if top2 = V then s2 else parensAround s2
              in
                (group (first $$ spaces 2 ++ second), names, A)
              end
          | Term.Lam (x, t) =>
              let
                val (name, names, _) = update names x
                val (s, names, _) = toDoc' t names
              in
                ( group
                    (text Unicode.lambda ++ name ++ text "." $$ spaces 2 ++ s)
                , names
                , L
                )
              end

        val (s, _, _) = toDoc' t Variables.empty
      in
        s
      end
  in
    fun toString t =
      PrettyDoc.toString (toDoc t)
  end

  val eval = List.app
    (fn Parser.Stmt.Print e => TextIO.print (toString (norm e) ^ "\n")
      | Parser.Stmt.Assert (e1, compare, e2) =>
       let
         val e1 = norm e1
         val e2 = norm e2
       in
         if Term.aequiv (e1, e2) = compare then
           ()
         else
           TextIO.print
             ("Assertion failed: " ^ Term.toString e1
              ^ (if compare then " = " else " != ") ^ Term.toString e2 ^ "\n")
       end)

  exception ExecutionError of string

  fun hdl f x syms =
    (f x syms)
    handle
      Parser.ParseError Parser.EOF =>
        (TextIO.print "Syntax error at end of file\n"; syms)
    | Parser.ParseError (Parser.Pos pos) =>
        (TextIO.print ("Syntax error at " ^ Int.toString pos ^ "\n"); syms)
    | e => (TextIO.print ("Exception: " ^ General.exnMessage e ^ "\n"); syms)

  fun process text syms =
    let val (stmts, syms) = parse text syms
    in (eval stmts; syms)
    end

  fun run (text, syms) =
    hdl process text syms

  fun evalFile filename =
    (foldl run Symbols.empty (stringStream [] (Input.readFile filename)); ())

  fun repl () =
    ( foldl run Symbols.empty (Input.promptKeybd "->" "=>" (stringStream []))
    ; ()
    )
end
