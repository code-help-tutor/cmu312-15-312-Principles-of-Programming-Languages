structure Lexer :> LEXER where type token = Token.token =
struct
  open Token

  (* reserved words *)
  val keywords: token option Symbols.dict =
    List.foldr (fn ((str, token), m) => Symbols.insert m str (SOME token))
      Symbols.empty
      [ ("nat", NAT)
      , ("ifz", IFZ)
      , ("z", Z)
      , ("s", S)
      , ("ret", RET)
      , ("is", IS)
      , ("let", LET)
      , ("in", IN)
      , ("fun", FUN)
      , ("unit", UNIT)
      , ("void", VOID)
      , ("fn", FN)
      , ("A", TYPEA)
      , ("B", TYPEB)
      , ("C", TYPEC)
      , ("D", TYPED)
      , ("case", CASE)
      , ("cont", CONT)
      , ("throw", THROW)
      , ("letcc", LETCC)
      , ("comp", COMP)
      , ("split", SPLIT)
      , ("L", LLABEL)
      , ("R", RLABEL)
      ]

  open Stream

  type pos = int * int
  fun pos_to_string (l, c) =
    "line " ^ Int.toString l ^ ", column " ^ Int.toString c

  type t = pos -> (token * pos) front
  type u = pos -> char stream * pos

  type self = {lexmain: char stream -> t, skipcomment: char stream -> u}

  type info =
    { match: char list
    , len: int
    , start: char stream
    , follow: char stream
    , self: self
    }

  exception Error

  fun action f ({match, len, follow, self, ...}: info) (l, c) =
    Cons (f (match, len, (l, c)), lazy (fn () =>
      #lexmain self follow (l, c + len)))

  fun simple token ({len, follow, self, ...}: info) (l, c) =
    Cons ((token, (l, c)), lazy (fn () => #lexmain self follow (l, c + len)))

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
            ; print (pos_to_string pos)
            ; print ".\n"
            ; raise Error
            )
        | SOME (SOME token) => (token, pos)
      end)

    fun skip ({len, follow, self, ...}: info) (l, c) =
      #lexmain self follow (l, c + len)

    fun newline ({len, follow, self, ...}: info) (l, c) =
      #lexmain self follow (l + 1, 0)

    fun lcomment ({len, follow, self, ...}: info) (l, c) =
      let val (follow', pos') = #skipcomment self follow (l, c + len)
      in #lexmain self follow' pos'
      end

    fun comment_newline ({len, follow, self, ...}: info) (l, c) =
      #skipcomment self follow (l + 1, 0)

    fun comment_open ({len, follow, self, ...}: info) (l, c) =
      let val (follow', pos') = #skipcomment self follow (l, c + len)
      in #skipcomment self follow' pos'
      end

    fun comment_close ({len, follow, ...}: info) (l, c) =
      (follow, (l, c + len))

    fun comment_skip ({len, follow, self, ...}: info) (l, c) =
      #skipcomment self follow (l, c + len)

    fun comment_error _ pos =
      ( print "Unclosed comment at "
      ; print (pos_to_string pos)
      ; print ".\n"
      ; raise Error
      )

    fun error _ pos =
      ( print "Lexical error at "
      ; print (pos_to_string pos)
      ; print ".\n"
      ; raise Error
      )

    val lparen = simple LPAREN
    val rparen = simple RPAREN
    val lbrack = simple LBRACK
    val rbrack = simple RBRACK
    val lbrace = simple LBRACE
    val rbrace = simple RBRACE
    val langle = simple LANGLE
    val rangle = simple RANGLE

    val comma = simple COMMA
    val dot = simple DOT
    val bar = simple BAR
    val colon = simple COLON
    val darrow = simple DARROW
    val leftarrow = simple IS
    val equals = simple IS

    val llabel = simple LLABEL
    val rlabel = simple RLABEL

    val nat = simple NAT
    val arrow = simple ARROW

    val plusop = simple PLUS
    val timesop = simple TIMES
    val lambda = simple FN
  end

  structure LexMain =
    LexMainFun (structure Streamable = StreamStreamable structure Arg = Arg)

  fun lex s =
    lazy (fn () => LexMain.lexmain s (0, 0))
end
