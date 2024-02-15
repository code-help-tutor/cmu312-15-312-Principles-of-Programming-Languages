structure Lexer: LEXER =
struct
  open Token

  structure Symbols = StringSplayDict

  (* reserved words *)
  val keywords: token Symbols.dict =
    List.foldr (fn ((str, token), m) => Symbols.insert m str token)
      Symbols.empty
      [ ("rec", REC)
      , ("error", ERROR)
      , ("in", IN)
      , ("case", CASE)
      , ("fn", FN)
      , ("fold", FOLD)
      , ("unfold", UNFOLD)
      ]

  open Stream

  type pos = int * int
  fun posToString (l, c) =
    (* + 1, to match editor line/column numbers *)
    "line " ^ Int.toString (l + 1) ^ ", column " ^ Int.toString (c + 1)

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
        | SOME token => (token, pos)
      end)

    val number = action (fn (chars, _, pos) =>
      let
        val str = implode chars
      in
        case Int.fromString str of
          NONE =>
            ( print "Illegal identifier at "
            ; print (posToString pos)
            ; print ".\n"
            ; raise Error
            )
        | SOME n => (NUMBER n, pos)
      end)

    fun skip ({len, follow, self, ...}: info) (l, c) =
      #lexmain self follow (l, c + len)

    fun newline ({len, follow, self, ...}: info) (l, c) =
      #lexmain self follow (l + 1, 0)

    fun begin_comment ({len, follow, self, ...}: info) (l, c) =
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
      ; print (posToString pos)
      ; print ".\n"
      ; raise Error
      )

    fun error _ pos =
      ( print "Lexical error at "
      ; print (posToString pos)
      ; print ".\n"
      ; raise Error
      )

    val langle = simple LANGLE
    val rangle = simple RANGLE
    val lbrace = simple LBRACE
    val rbrace = simple RBRACE
    val lbracket = simple LBRACKET
    val rbracket = simple RBRACKET
    val lparen = simple LPAREN
    val rparen = simple RPAREN

    val arrow = simple ARROW
    val bar = simple BAR
    val colon = simple COLON
    val comma = simple COMMA
    val darrow = simple DARROW
    val dot = simple DOT
    val equal = simple EQUAL
    val plus = simple PLUS
    val star = simple STAR
    val semicolon = simple SEMICOLON

    val leq = simple LEQ
  end

  structure LexMain =
    LexMain (structure Streamable = StreamStreamable structure Arg = Arg)

  fun lex s =
    lazy (fn () => LexMain.lexmain s (0, 0))
end
