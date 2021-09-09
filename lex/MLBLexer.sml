(** Copyright (c) 2021 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure MLBLexer:
sig
  exception Error of LineError.t

  (** Get the next token in the given source. If there isn't one, returns NONE.
    * raises Error if there's a problem.
    *)
  val next: Source.t -> MLBToken.t option

  (** Get all the tokens in the given source.
    * raises Error if there's a problem.
    *)
  val tokens: Source.t -> MLBToken.t Seq.t
end =
struct

  exception Error of LineError.t
  fun error {what, pos, explain} =
    raise Error
      { header = "SYNTAX ERROR"
      , pos = pos
      , what = what
      , explain = explain
      }

  fun success x = SOME x


  fun expectSMLToken check src =
    case Lexer.next src of
      NONE => raise Fail "Lexer bug!"
    | SOME tok =>
        if check tok then
          success (MLBToken.fromSMLToken tok)
        else
          raise Fail "Lexer bug!"


  val acceptedExtensions =
    [ ".mlb"
    , ".sml"
    , ".sig"
    , ".fun"
    ]


  fun extensionOfPathInSource src =
    let
      fun findDot i =
        if i = 0 then
          NONE
        else
          case Source.nth src (i-1) of
            #"." => SOME (i-1)
          | #"/" => NONE
          | _ => findDot (i-1)
    in
      case findDot (Source.length src) of
        SOME i => Source.toString (Source.slice src (i, Source.length src - i))
      | NONE =>
          error
            { pos = src
            , what = "Path is missing extension."
            , explain = NONE
            }
    end


  fun makePath src =
    case extensionOfPathInSource src of
      ".mlb" => MLBToken.make src MLBToken.MLBPath
    | ".sml" => MLBToken.make src MLBToken.SMLPath
    | ".sig" => MLBToken.make src MLBToken.SMLPath
    | ".fun" => MLBToken.make src MLBToken.SMLPath
    | _ =>
        error
          { pos = src
          , what = "Unsupported file extension."
          , explain = SOME "Valid extensions are: .mlb .sml .sig .fun"
          }


  fun next src =
    let
      val startOffset = Source.absoluteStartOffset src
      val src = Source.wholeFile src

      (** Some helpers for making source slices and tokens. *)
      fun slice (i, j) = Source.slice src (i, j-i)
      fun sliceFrom i = slice (i, Source.length src)
      fun mk x (i, j) = MLBToken.make (slice (i, j)) x
      fun mkr x (i, j) = MLBToken.reserved (slice (i, j)) x

      fun get i = Source.nth src i

      fun isEndOfFileAt i =
        i >= Source.length src

      fun check f i = i < Source.length src andalso f (get i)
      fun is c = check (fn c' => c = c')

      fun isString str i =
        Source.length src - i >= String.size str
        andalso
        Util.all (0, String.size str) (fn j => get (i+j) = String.sub (str, j))


      fun loop_toplevel i =
        if isEndOfFileAt i then
          NONE

        else if
          isString "(*" i
        then
          expectSMLToken Token.isComment (sliceFrom i)

        else if
          is #"\"" i
        then
          expectSMLToken Token.isStringConstant (sliceFrom i)

        else if
          isString "bas" i
        then
          loop_after_bas (i+3)

        else if
          isString "ann" i andalso
          not (is #"/" (i+3))    (** annoying edge case for paths *)
        then
          success (mkr MLBToken.Ann (i, i+3))

        else if
          check LexUtils.isValidUnquotedPathChar i
        then
          loop_maybePath {start=i} (i+1)

        else if
          check Char.isSpace i
        then
          loop_toplevel (i+1)

        else
          expectSMLToken (fn _ => true) (sliceFrom i)


      (** bas
        *    ^
        *)
      and loop_after_bas i =
        if
          isString "is" i andalso
          not (is #"/" (i+2))    (** annoying edge case for paths *)
        then
          success (mkr MLBToken.Basis (i-3, i+2))

        else if
          not (is #"/" i)
        then
          success (mkr MLBToken.Bas (i-3, i))

        else
          loop_maybePath {start = i-3} i


      (** Paths are strange. You really could see anything... the only
        * saving grace here is that MLBs seem to require that paths end
        * in a valid extension, which for now I'm going to restrict to:
        *   .mlb .sml .sig .fun
        *)
      and loop_maybePath {start} i =
        if check LexUtils.isValidUnquotedPathChar i then
          loop_maybePath {start=start} (i+1)

        else if
          Util.exists (start, i) (fn j => is #"." j orelse is #"/" j)
        then
          success (makePath (slice (start, i)))

        else
          expectSMLToken (fn _ => true) (sliceFrom start)

    in
      loop_toplevel startOffset
    end


  fun tokens src =
    let
      val startOffset = Source.absoluteStartOffset src
      val endOffset = Source.absoluteEndOffset src
      val src = Source.wholeFile src

      fun tokEndOffset tok =
        Source.absoluteEndOffset (Token.getSource tok)

      fun finish acc =
        Seq.rev (Seq.fromList acc)

      fun loop acc offset =
        if offset >= endOffset then
          finish acc
        else
          case next (Source.drop src offset) of
            NONE =>
              finish acc
          | SOME tok =>
              loop (tok :: acc) (tokEndOffset tok)
    in
      loop [] startOffset
    end

end
