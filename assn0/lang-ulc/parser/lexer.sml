structure Lexer :> LEXER where type token = Token.token =
struct
  open Token

  open Stream

  type pos = int

  type t = int -> (token * pos) front
  type u = int -> char stream * int

  type self = {lexmain: char stream -> int -> (token * pos) front}

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

    val ident = action (fn (chars, _, pos) => let val str = implode chars
                                              in (IDENT str, pos)
                                              end)

    fun skip ({len, follow, self, ...}: info) pos =
      #lexmain self follow (pos + len)

    fun error _ pos =
      ( print "Lexical error at "
      ; print (Int.toString pos)
      ; print ".\n"
      ; raise Error
      )

    val lparen = simple LPAREN
    val rparen = simple RPAREN

    val dot = simple DOT
    val lambda = simple LAMBDA

    val norm = simple NORM

    val def = simple DEF
    val def_force = simple DEF_FORCE
    val undef = simple UNDEF
    val print = simple PRINT
    val print_force = simple PRINT_FORCE
    val assert = simple ASSERT
    val assert_force = simple ASSERT_FORCE
    val eq = simple EQ
    val neq = simple NEQ
  end

  structure LexMain =
    LexMainFun (structure Streamable = StreamStreamable structure Arg = Arg)

  fun lex s =
    lazy (fn () => LexMain.lexmain s 0)
end
