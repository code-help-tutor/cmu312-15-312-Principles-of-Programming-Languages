(* Chris Okasaki / Robert Harper
   School of Computer Science
   Carnegie Mellon University
   Pittsburgh, PA 15213
   cokasaki@cs.cmu.edu *)
(* Modified by Kevin Watkins *)

signature INPUT =
sig
  (* also useful in toplevel *)
  val isBlank: string -> bool

  (* Each element of an input stream provides a character
     together with a list of all characters from that point
     to the end of the line (for error reporting) *)
  val readFile: string -> (char * char list) Stream.stream
  val readKeybd: unit -> (char * char list) Stream.stream

  (* The type of this one is slightly subtle in order to handle
     different prompts for the start and the continuation of a term. *)
  val promptKeybd: string
                   -> string
                   -> ((char * char list) Stream.stream -> 'a Stream.stream)
                   -> 'a Stream.stream

end; (* signature INPUT *)

structure Input :> INPUT =
struct
  structure T = TextIO
  structure S = Stream

  (* Reading a stream of lines from some input source *)

  fun isBlank ln =
    String.tokens (fn c => Char.isSpace c orelse Char.isCntrl c) ln = nil

  fun keybdLines file prompt1 prompt2 ducer =
    let
      val pr = ref prompt1
      fun get () =
        ( (!pr) ()
        ; case T.inputLine file of
            NONE => S.Nil
          | SOME ln =>
              ( if not (isBlank ln) then pr := prompt2 else ()
              ; S.Cons (ln, S.lazy get)
              )
        )
      fun put s =
        case (pr := prompt1; S.front s) of
          S.Cons (x, s) => S.Cons (x, S.lazy (fn () => put s))
        | S.Nil => S.Nil
    in
      S.lazy (fn () => put (ducer (S.lazy get)))
    end

  fun fileLines filename =
    let
      fun get file =
        case T.inputLine file of
          NONE => (T.closeIn file; S.Nil)
        | SOME ln =>
            if isBlank ln then get file
            else S.Cons (ln, S.lazy (fn () => get file))
    in
      S.lazy (fn () => get (T.openIn filename))
    end

  (* Splitting a stream of lines into a stream of chars
     (with rest-of-lines for error reporting) *)

  fun line (nil, s) = S.front s
    | line (l as (h :: t), s) =
        S.Cons ((h, l), S.lazy (fn () => line (t, s)))

  fun linesToChars s =
    let
      fun get s =
        case S.front s of
          S.Cons (x, s) => line (explode x, S.lazy (fn () => get s))
        | S.Nil => S.Nil
    in
      S.lazy (fn () => get s)
    end

  (* Putting it together *)

  fun readFile filename =
    linesToChars (fileLines filename)

  fun readKeybd () =
    keybdLines T.stdIn (fn () => ()) (fn () => ()) linesToChars

  fun doprompt s =
    (T.output (T.stdOut, s); T.flushOut T.stdOut)

  fun promptKeybd prompt1 prompt2 ducer =
    keybdLines T.stdIn (fn () => doprompt prompt1) (fn () => doprompt prompt2)
      (fn s => ducer (linesToChars s))

end; (* structure Input *)
