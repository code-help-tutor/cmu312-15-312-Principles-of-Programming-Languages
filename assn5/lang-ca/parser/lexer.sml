structure Lexer :> LEXER where type token = Token.token =
struct
  open Token

  (* reserved words *)
  val keywords: token option Symbols.dict =
    List.foldr (fn ((str, token), m) => Symbols.insert m str (SOME token))
      Symbols.empty
      [ ("val", VAL)
      , ("if", IF)
      , ("ifz", IFZ)
      , ("print", PRINT)
      , ("load", LOAD)
      , ("type", TYPE)
      , ("rec", REC)
      , ("unit", UNIT)
      , ("zero", ZERO)
      , ("succ", SUCC)
      , ("then", THEN)
      , ("else", ELSE)
      , ("true", TRUE)
      , ("false", FALSE)
      , ("end", END)
      , ("in", IN)
      , ("let", LET)
      , ("fn", FN)
      , ("fun", FUN)
      , ("case", CASE)
      , ("fold", FOLD)
      , ("unfold", UNFOLD)
      , ("inl", INL)
      , ("inr", INR)
      , ("split", SPLIT)
      , ("is", IS)
      , ("of", OF)
      , ("abort", ABORT)
      , ("void", VOID)
      , ("cmd", CMD)
      , ("do", DO)
      , ("ret", RET)
      , ("nat", NAT)
      , ("bool", BOOL)
      , ("emit", EMIT)
      , ("newchan", NEWCHAN)
      , ("chan", CHAN)
      , ("spawn", SPAWN)
      , ("sync", SYNC)
      ]

  open Stream

  type pos = int

  type t = int -> (token * pos) front
  type u = int -> char stream * int

  type self =
    { lexmain: char stream -> int -> (token * pos) front
    , skipcomment: char stream -> int -> char stream * int
    }

  type info =
    { match: char list
    , len: int
    , start: char stream
    , follow: char stream
    , self: self
    }

  exception Error

  fun action f ({match, len, follow, self, ...}: info) pos =
    Cons (f (match, len, pos), lazy (fn () => #lexmain self follow (pos + len)))

  fun simple token ({len, follow, self, ...}: info) pos =
    Cons ((token, pos), lazy (fn () => #lexmain self follow (pos + len)))

  structure Arg =
  struct
    type symbol = char
    val ord = Char.ord

    type t = t
    type u = u
    type self = self
    type info = info

    fun eof _ _ = Nil

    val ident = action (fn (chars, _, pos) =>
      let
        val str = implode chars
      in
        case Symbols.find keywords str of
          NONE => (IDENT str, pos)
        | SOME NONE =>
            ( print "Illegal identifier at "
            ; print (Int.toString pos)
            ; print ".\n"
            ; raise Error
            )
        | SOME (SOME token) => (token, pos)
      end)

    val str = action (fn (chars, _, pos) =>
      let
        val chart = List.tl chars
        val str = implode (List.take (chart, (List.length chart - 1)))
      in
        (STRING str, pos)
      end)

    fun skip ({len, follow, self, ...}: info) pos =
      #lexmain self follow (pos + len)

    fun lcomment ({len, follow, self, ...}: info) pos =
      let val (follow', pos') = #skipcomment self follow (pos + len)
      in #lexmain self follow' pos'
      end

    fun comment_open ({len, follow, self, ...}: info) pos =
      let val (follow', pos') = #skipcomment self follow (pos + len)
      in #skipcomment self follow' pos'
      end

    fun comment_close ({len, follow, ...}: info) pos = (follow, pos + len)

    fun comment_skip ({len, follow, self, ...}: info) pos =
      #skipcomment self follow (pos + len)

    fun comment_error _ pos =
      ( print "Unclosed comment at "
      ; print (Int.toString pos)
      ; print ".\n"
      ; raise Error
      )

    fun error _ pos =
      ( print "Lexical error at "
      ; print (Int.toString pos)
      ; print ".\n"
      ; raise Error
      )

    val num = action (fn (chars, _, pos) =>
      case Int.fromString (implode chars) of
        NONE =>
          ( print "Couldn't parse number at "
          ; print (Int.toString pos)
          ; print ".\n"
          ; raise Error
          )
      | SOME n => (NUM n, pos))

    val filename = action (fn (chars, _, pos) =>
      (FILENAME (implode (List.take (List.tl chars, length chars - 2))), pos))

    val dot = simple DOT
    val comma = simple COMMA
    val plus = simple PLUS
    val equal = simple EQUAL
    val lparen = simple LPAREN
    val rparen = simple RPAREN
    val star = simple STAR
    val gt = simple GT
    val lt = simple LT
    val lbrace = simple LBRACE
    val rbrace = simple RBRACE
    val lbrack = simple LBRACK
    val rbrack = simple RBRACK
    val colon = simple COLON
    val tilde = simple TILDE
    val darrow = simple DARROW
    val arrow = simple ARROW
    val pipe = simple PIPE
    val lte = simple LTE
    val gte = simple GTE
    val dash = simple DASH
    val slash = simple SLASH
    val neq = simple NEQ
    val andand = simple ANDAND
    val oror = simple OROR
    val bang = simple BANG
    val percent = simple PERCENT
  end

  structure LexMain =
    LexMainFun (structure Streamable = StreamStreamable structure Arg = Arg)

  fun lex s =
    lazy (fn () => LexMain.lexmain s 0)
end
