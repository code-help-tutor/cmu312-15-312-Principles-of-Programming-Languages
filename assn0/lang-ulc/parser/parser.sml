functor Parser (ULC : ULC) :> PARSER where ULC = ULC =
struct
  structure ULC = ULC
  structure ParseTerm = ParseTerm (ULC)
  structure ParseStmt = ParseStmt (type term = ParseTerm.t)
  structure Stmt = Stmt (type term = ParseTerm.t)

  datatype position = EOF | Pos of int

  exception ParseError of position (*= Lexer.Error *)

  type pos = int

  structure Arg =
  struct
    type string = string
    type stmts = ParseStmt.t list
    type stmt = ParseStmt.t
    type term = ParseTerm.t
    type bool = bool

    val termvar = ParseTerm.Var
    val termapp = ParseTerm.App
    val termabs = ParseTerm.Lam
    val termnorm = ParseTerm.Norm
    fun termid x = x
    val stmtdef = ParseStmt.Define
    val stmtundef = ParseStmt.Undefine
    val stmtprint = ParseStmt.Print
    val stmtassert = ParseStmt.Assert
    val stmtscons = op::
    fun stmtsnil () = []
    fun bfalse () = false
    fun btrue () = true

    datatype terminal = datatype Token.token

    fun error s =
      case Stream.front s of
        Stream.Nil => ParseError EOF
      | Stream.Cons ((_, pos), _) => ParseError (Pos pos)
  end

  structure StreamWithPos =
     CoercedStreamable (structure Streamable = StreamStreamable
                        type 'a item = 'a * pos
                        fun coerce (x, _) = x)

  structure ParseMain =
     ParseMainFun
     (structure Streamable = StreamWithPos
      structure Arg = Arg)

  fun parse norm_fn s syms =
    let
      open ParseTerm
      val stmts = #1 (ParseMain.parse (Lexer.lex s))
      fun norm e syms = ParseTerm.norm (add_defs (ParseTerm.Norm e) syms) norm_fn
    in
      List.foldl (fn (stmt, (stmts, syms)) =>
        case stmt of
          ParseStmt.Print (false, e) => (Stmt.Print (add_defs e syms)::stmts, syms)
        | ParseStmt.Print (true, e) => (Stmt.Print (norm e syms)::stmts, syms)
        | ParseStmt.Assert (false, e1, compare, e2) =>
            (Stmt.Assert (add_defs e1 syms, compare, add_defs e2 syms)::stmts, syms)
        | ParseStmt.Assert (true, e1, compare, e2) =>
            (Stmt.Assert (norm e1 syms, compare, norm e2 syms)::stmts, syms)
        | ParseStmt.Define (false, s, e) =>
            (stmts, Symbols.insert (Symbols.remove syms s) s (add_defs e syms))
        | ParseStmt.Define (true, s, e) =>
            (stmts, Symbols.insert (Symbols.remove syms s) s (norm e syms))
        | ParseStmt.Undefine s =>
            (stmts, Symbols.remove syms s)
      ) ([], syms) stmts
    end
end
