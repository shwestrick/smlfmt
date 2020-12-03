structure SeqLex: LEX =
struct

  (** ======================================================================
    * Some helpers
    *)

  val isValidFormatEscapeChar = Char.contains " \t\n\f\r"
  val isValidSingleEscapeChar = Char.contains "abtnvfr\\\""
  val isSymbolic = Char.contains "!%&$#+-/:<=>?@\\~`^|*"
  fun isDecDigit c = Char.isDigit c
  fun isHexDigit c = Char.isHexDigit c
  fun isLetter c = Char.isAlpha c
  fun isAlphaNumPrimeOrUnderscore c =
    Char.isAlphaNum c orelse c = #"_" orelse c = #"'"


  (** ======================================================================
    * A stream is a convenient representation of the input, as we scan
    * from left to right.
    *)

  structure Stream:
  sig
    type stream
    type t = stream
    val make: char Seq.t -> stream
    val next1: stream -> char option
    val next2: stream -> (char * char) option
    val step1: stream -> stream
    val step2: stream -> stream
    val pos: stream -> int
  end =
  struct
    type stream = char Seq.t * int
    type t = stream

    fun make c = (c, 0)

    fun next1 (c, i) =
      if i >= Seq.length c then
        NONE
      else
        SOME (Seq.nth c i)

    fun next2 (c, i) =
      if i+1 >= Seq.length c then
        NONE
      else
        SOME (Seq.nth c i, Seq.nth c (i+1))

    fun step1 (c, i) =
      (c, Int.min (Seq.length c, i+1))

    fun step2 (c, i) =
      (c, Int.min (Seq.length c, i+2))

    fun pos (c, i) = i
  end



  (** =====================================================================
    * STATE MACHINE
    *
    * This bunch of mutually-recursive functions implements an efficient
    * state machine. Each is named `loop_<STATE_NAME>`. The arguments are
    * always
    *   `loop_XXX acc stream [args]`
    * where
    *   `acc` is a token accumulator,
    *   `stream` is the rest of the input, and
    *   `args` is a state-dependent state (haha)
    *)

  structure S = Stream

  fun loop_topLevel acc s =
    case S.next1 s of
      SOME #"(" =>
        loop_afterOpenParen acc (S.step1 s)
    | SOME #")" =>
        loop_topLevel (Token.closeParen (S.pos s) :: acc) (S.step1 s)
    | SOME #"[" =>
        loop_topLevel (Token.openSquareBracket (S.pos s) :: acc) (S.step1 s)
    | SOME #"]" =>
        loop_topLevel (Token.closeSquareBracket (S.pos s) :: acc) (S.step1 s)
    | SOME #"{" =>
        loop_topLevel (Token.openCurlyBracket (S.pos s) :: acc) (S.step1 s)
    | SOME #"}" =>
        loop_topLevel (Token.closeCurlyBracket (S.pos s) :: acc) (S.step1 s)
    | SOME #"," =>
        loop_topLevel (Token.comma (S.pos s) :: acc) (S.step1 s)
    | SOME #";" =>
        loop_topLevel (Token.semicolon (S.pos s) :: acc) (S.step1 s)
    | SOME #"_" =>
        loop_topLevel (Token.underscore (S.pos s) :: acc) (S.step1 s)
    | SOME #"\"" =>
        loop_inString acc (S.step1 s) {stringStart = S.pos s}
    | SOME #"~" =>
        loop_afterTwiddle acc (S.step1 s)
    | SOME #"'" =>
        loop_alphanumId acc (S.step1 s) {idStart = S.pos s, startsPrime = true}
    | SOME #"0" =>
        loop_afterZero acc (S.step1 s)
    | SOME #"." =>
        loop_afterDot acc (S.step1 s)
    | SOME c =>
        if isDecDigit c then
          loop_decIntegerConstant acc (S.step1 s) {constStart = S.pos s}
        else if isSymbolic c then
          loop_symbolicId acc (S.step1 s) {idStart = S.pos s}
        else if isLetter c then
          loop_alphanumId acc (S.step1 s) {idStart = S.pos s, startsPrime = false}
        else
          loop_topLevel acc (S.step1 s)
    | NONE =>
        (** DONE *)
        acc


  and loop_afterDot acc s =
    case S.next2 s of
      SOME (#".", #".") =>
        loop_topLevel (Token.dotdotdot (S.pos s - 1) :: acc) (S.step2 s)
    | SOME (c1, c2) =>
        raise Fail ("expected '...' but found '." ^ String.implode [c1,c2] ^ "'")
    | NONE =>
        raise Fail ("expected '...' but found end of file")



  and loop_symbolicId acc s (args as {idStart}) =
    case S.next1 s of
      SOME c =>
        if isSymbolic c then
          loop_symbolicId acc (S.step1 s) args
        else
          loop_topLevel
            (Token.symbolicId (idStart, S.pos s) :: acc)
            s
    | NONE =>
        (** DONE *)
        Token.symbolicId (idStart, S.pos s) :: acc



  and loop_alphanumId acc s (args as {idStart, startsPrime}) =
    case S.next1 s of
      SOME #"." =>
        if startsPrime then
          raise Fail "structure identifiers cannot start with prime"
        else
          loop_continueLongIdentifier acc (S.step1 s) {idStart=idStart}
    | SOME c =>
        (** SML's notion of alphanum is a little weird. *)
        if isAlphaNumPrimeOrUnderscore c then
          loop_alphanumId acc (S.step1 s) args
        else
          loop_topLevel
            (Token.alphanumId (idStart, S.pos s) :: acc)
            s
    | NONE =>
        (** DONE *)
        Token.alphanumId (idStart, S.pos s) :: acc



  and loop_continueLongIdentifier acc s (args as {idStart}) =
    case S.next1 s of
      SOME c =>
        if isSymbolic c then
          loop_symbolicId acc (S.step1 s) args
        else if isLetter c then
          loop_alphanumId acc (S.step1 s) {idStart = idStart, startsPrime = false}
        else
          raise Fail "after qualifier, expected letter or symbol"
    | NONE =>
        raise Fail "unexpected end of qualified identifier"



  (** After seeing a twiddle, we might be at the beginning of an integer
    * constant, or we might be at the beginning of a symbolic-id.
    *
    * Note that seeing "0" next is special, because of e.g. "0x" used to
    * indicate the beginning of hex format.
    *)
  and loop_afterTwiddle acc s =
    case S.next1 s of
      SOME #"0" =>
        loop_afterTwiddleThenZero acc (S.step1 s)
    | SOME c =>
        if isDecDigit c then
          loop_decIntegerConstant acc (S.step1 s) {constStart = S.pos s - 1}
        else if isSymbolic c then
          loop_symbolicId acc (S.step1 s) {idStart = S.pos s - 1}
        else
          loop_topLevel
            (Token.symbolicId (S.pos s - 1, S.pos s) :: acc)
            s
    | NONE =>
        (** DONE *)
        Token.symbolicId (S.pos s - 1, S.pos s) :: acc



  (** Comes after "~0"
    * This might be the middle or end of an integer constant. We have
    * to first figure out if the integer constant is hex format.
    *)
  and loop_afterTwiddleThenZero acc s =
    case S.next2 s of
      SOME (#"x", c) =>
        if isHexDigit c then
          loop_hexIntegerConstant acc (S.step2 s) {constStart = S.pos s - 2}
        else
          loop_topLevel
            (Token.integerConstant (S.pos s - 2, S.pos s) :: acc)
            s
    | _ =>
    case S.next1 s of
      SOME #"." =>
        loop_realConstantAfterDot acc (S.step1 s) {constStart = S.pos s - 2}
    | SOME c =>
        if isDecDigit c then
          loop_decIntegerConstant acc (S.step1 s) {constStart = S.pos s - 2}
        else
          loop_topLevel
            (Token.integerConstant (S.pos s - 2, S.pos s) :: acc)
            s
    | NONE =>
        (** DONE *)
        Token.integerConstant (S.pos s - 2, S.pos s) :: acc



  (** After seeing "0", we're certainly at the beginning of some sort
    * of numeric constant. We need to figure out if this is an integer or
    * a word, and if it is hex or decimal format.
    *)
  and loop_afterZero acc s =
    case S.next2 s of
      SOME (#"x", c) =>
        if isHexDigit c then
          loop_hexIntegerConstant acc (S.step2 s) {constStart = S.pos s - 1}
        else
          loop_topLevel (Token.integerConstant (S.pos s - 1, S.pos s) :: acc) s
    | _ =>
    case S.next1 s of
      SOME #"." =>
        loop_realConstantAfterDot acc (S.step1 s) {constStart = S.pos s - 1}
    | SOME #"w" =>
        loop_afterZeroDubya acc (S.step1 s)
    | SOME c =>
        if isDecDigit c then
          loop_decIntegerConstant acc (S.step1 s) {constStart = S.pos s - 1}
        else
          loop_topLevel (Token.integerConstant (S.pos s - 1, S.pos s) :: acc) s
    | NONE =>
        (** DONE *)
        Token.integerConstant (S.pos s - 1, S.pos s) :: acc



  and loop_decIntegerConstant acc s (args as {constStart}) =
    case S.next1 s of
      SOME #"." =>
        loop_realConstantAfterDot acc (S.step1 s) args
    | SOME c =>
        if isDecDigit c then
          loop_decIntegerConstant acc (S.step1 s) args
        else
          loop_topLevel (Token.integerConstant (constStart, S.pos s) :: acc) s
    | NONE =>
        (** DONE *)
        Token.integerConstant (constStart, S.pos s) :: acc



  (** Immediately after the dot, we need to see at least one decimal digit *)
  and loop_realConstantAfterDot acc s (args as {constStart}) =
    case S.next1 s of
      SOME c =>
        if isDecDigit c then
          loop_realConstant acc (S.step1 s) args
        else
          raise Fail ("while parsing real constant, expected decimal digit \
                      \but found '." ^ String.implode [c] ^ "'")
    | NONE =>
        raise Fail "real constant ends unexpectedly at end of file"


  (** Parsing the remainder of a real constant. This is already after the
    * dot, because the front of the real constant was already parsed as
    * an integer constant.
    *)
  and loop_realConstant acc s (args as {constStart}) =
    case S.next1 s of
      SOME #"E" =>
        raise Fail "real constants with exponents 'E' not supported yet"
    | SOME c =>
        if isDecDigit c then
          loop_realConstant acc (S.step1 s) args
        else
          loop_topLevel
            (Token.realConstant (constStart, S.pos s) :: acc)
            s
    | NONE =>
        Token.realConstant (constStart, S.pos s) :: acc



  and loop_hexIntegerConstant acc s (args as {constStart}) =
    case S.next1 s of
      SOME c =>
        if isHexDigit c then
          loop_hexIntegerConstant acc (S.step1 s) args
        else
          loop_topLevel (Token.integerConstant (constStart, S.pos s) :: acc) s
    | NONE =>
        (** DONE *)
        Token.integerConstant (constStart, S.pos s) :: acc



  and loop_decWordConstant acc s (args as {constStart}) =
    case S.next1 s of
      SOME c =>
        if isDecDigit c then
          loop_decWordConstant acc (S.step1 s) args
        else
          loop_topLevel (Token.wordConstant (constStart, S.pos s) :: acc) s
    | NONE =>
        (** DONE *)
        Token.wordConstant (constStart, S.pos s) :: acc



  and loop_hexWordConstant acc s (args as {constStart}) =
    case S.next1 s of
      SOME c =>
        if isHexDigit c then
          loop_hexWordConstant acc (S.step1 s) args
        else
          loop_topLevel (Token.wordConstant (constStart, S.pos s) :: acc) s
    | NONE =>
        (** DONE *)
        Token.wordConstant (constStart, S.pos s) :: acc



  (** Comes after "0w"
    * It might be tempting to think that this is certainly a word constant,
    * but that's not necessarily true. Here's some possibilities:
    *   0w5       -- word constant 5
    *   0wx5      -- word constant 5, in hex format
    *   0w        -- integer constant 0 followed by alphanum-id "w"
    *   0wx       -- integer constant 0 followed by alphanum-id "wx"
    *)
  and loop_afterZeroDubya acc s =
    case S.next2 s of
      SOME (#"x", c) =>
        if isHexDigit c then
          loop_hexWordConstant acc (S.step2 s) {constStart = S.pos s - 2}
        else
          (** TODO: FIX: this is not quite right. Should
            * jump straight to identifier that starts with "wx"
            *)
          loop_topLevel
            (Token.integerConstant (S.pos s - 2, S.pos s - 1) :: acc)
            s
    | _ =>
    case S.next1 s of
      SOME c =>
        if isDecDigit c then
          loop_decWordConstant acc (S.step1 s) {constStart = S.pos s - 2}
        else
          (** TODO: FIX: this is not quite right. Should
            * jump straight to identifier that starts with "w"
            *)
          loop_topLevel
            (Token.integerConstant (S.pos s - 2, S.pos s - 1) :: acc)
            s
    | NONE =>
        (** DONE *)
        Token.integerConstant (S.pos s - 2, S.pos s - 1) ::
        Token.alphanumId (S.pos s - 1, S.pos s) ::
        acc


  (** An open-paren could just be a normal paren, or it could be the
    * start of a comment.
    *)
  and loop_afterOpenParen acc s =
    case S.next1 s of
      SOME #"*" =>
        loop_inComment acc (S.step1 s) {commentStart = S.pos s - 1, nesting = 1}
    | SOME _ =>
        loop_topLevel (Token.openParen (S.pos s - 1) :: acc) s
    | NONE =>
        (** DONE *)
        Token.openParen (S.pos s - 1) :: acc



  and loop_inString acc s (args as {stringStart}) =
    case S.next1 s of
      SOME #"\\" (* " *) =>
        loop_inStringEscapeSequence acc (S.step1 s) args
    | SOME #"\"" =>
        loop_topLevel
          (Token.string (stringStart, 1 + S.pos s) :: acc)
          (S.step1 s)
    | SOME c =>
        if Char.isPrint c then
          loop_inString acc (S.step1 s) args
        else
          raise Fail ("non-printable character at " ^ Int.toString (S.pos s))
    | NONE =>
        raise Fail ("unclosed string starting at " ^ Int.toString stringStart)



  (** Inside a string, and furthermore immediately inside a backslash *)
  and loop_inStringEscapeSequence acc s (args as {stringStart}) =
    case S.next1 s of
      SOME c =>
        if isValidSingleEscapeChar c then
          loop_inString acc (S.step1 s) args
        else if isValidFormatEscapeChar c then
          loop_inStringFormatEscapeSequence acc (S.step1 s) args
        else if c = #"^" then
          loop_inStringControlEscapeSequence acc (S.step1 s) args
        else if c = #"u" then
          raise Fail "escape sequences \\uxxxx not supported yet"
        else if isDecDigit c then
          raise Fail "escape sequences \\ddd not supported yet"
        else
          loop_inString acc s args
    | NONE =>
        raise Fail ("unclosed string starting at " ^ Int.toString stringStart)



  (** Inside a string, and furthermore inside an escape sequence of
    * the form \^c
    *)
  and loop_inStringControlEscapeSequence acc s (args as {stringStart}) =
    case S.next1 s of
      SOME c =>
        if 64 <= Char.ord c andalso Char.ord c <= 95 then
          loop_inStringEscapeSequence acc (S.step1 s) args
        else
          raise Fail ("invalid control escape sequence at " ^ Int.toString (S.pos s))
    | NONE =>
        raise Fail ("incomplete control escape sequence at " ^ Int.toString (S.pos s))



  (** Inside a string, and furthermore inside an escape sequence of the
    * form \f...f\ where each f is a format character (space, newline, tab,
    * etc.)
    *)
  and loop_inStringFormatEscapeSequence acc s (args as {stringStart}) =
    case S.next1 s of
      SOME #"\\" (*"*) =>
        loop_inString acc (S.step1 s) args
    | SOME c =>
        if isValidFormatEscapeChar c then
          loop_inStringFormatEscapeSequence acc (S.step1 s) args
        else
          raise Fail ("invalid format escape sequence at " ^ Int.toString (S.pos s))
    | NONE =>
        raise Fail ("incomplete format escape sequence at " ^ Int.toString (S.pos s))


  (** Inside a comment that started at `commentStart`
    * `nesting` is always >= 0 and indicates how many open-comments we've seen.
    *)
  and loop_inComment acc s {commentStart, nesting} =
    if nesting = 0 then
      loop_topLevel (Token.comment (commentStart, S.pos s) :: acc) s
    else

    case S.next2 s of
      SOME (#"(", #"*") =>
        loop_inComment acc (S.step2 s) {commentStart=commentStart, nesting=nesting+1}
    | SOME (#"*", #")") =>
        loop_inComment acc (S.step2 s) {commentStart=commentStart, nesting=nesting-1}
    | _ =>

    case S.next1 s of
      SOME _ =>
        loop_inComment acc (S.step1 s) {commentStart=commentStart, nesting=nesting}
    | NONE =>
        raise Fail ("unclosed comment starting at " ^ Int.toString commentStart)



  (** =====================================================================
    * The lexer above is a bit sloppy, in that it doesn't always produce
    * all reserved words. So we clean up here.
    *
    * The function `clarifyClassOfSymbolic` catches reserved words that
    * were sloppily identified as `Token.SymbolicId`.
    *
    * Similarly, `clarifyClassOfAlphanum` handles sloppy `Token.AlphanumId`s
    *)

  (** look closer at the token [f(k) : i <= k < j] *)
  fun clarifyClassOfSymbolic (i, j, f: int -> char) =
    case f i of
      #":" =>
        if j = i+1 then
          Token.Reserved Token.Colon
        else if j = i+2 andalso f (i+1) = #">" then
          Token.Reserved Token.ColonArrow
        else
          Token.SymbolicId
    | #"|" =>
        if j = i+1 then
          Token.Reserved Token.Bar
        else
          Token.SymbolicId
    | #"=" =>
        if j = i+1 then
          Token.Reserved Token.Equal
        else if j = i+2 andalso f (i+1) = #">" then
          Token.Reserved Token.FatArrow
        else
          Token.SymbolicId
    | #"-" =>
        if j = i+2 andalso f (i+1) = #">" then
          Token.Reserved Token.Arrow
        else
          Token.SymbolicId
    | #"#" =>
        if j = i+1 then
          Token.Reserved Token.Hash
        else
          Token.SymbolicId
    | _ =>
        Token.SymbolicId


  fun clarifyClassOfAlphanum (i, j, f) =
    let
      val str = CharVector.tabulate (j-i, fn k => f (i+k))
      fun r rclass = Token.Reserved rclass
    in
      case str of
      (** Core *)
        "abstype" => r Token.Abstype
      | "and" => r Token.And
      | "andalso" => r Token.Andalso
      | "as" => r Token.As
      | "case" => r Token.Case
      | "datatype" => r Token.Datatype
      | "do" => r Token.Do
      | "else" => r Token.Else
      | "end" => r Token.End
      | "exception" => r Token.Exception
      | "fn" => r Token.Fn
      | "fun" => r Token.Fun
      | "handle" => r Token.Handle
      | "if" => r Token.If
      | "in" => r Token.In
      | "infix" => r Token.Infix
      | "infixr" => r Token.Infixr
      | "let" => r Token.Let
      | "local" => r Token.Local
      | "nonfix" => r Token.Nonfix
      | "of" => r Token.Of
      | "op" => r Token.Op
      | "open" => r Token.Open
      | "orelse" => r Token.Orelse
      | "raise" => r Token.Raise
      | "rec" => r Token.Rec
      | "then" => r Token.Then
      | "type" => r Token.Type
      | "val" => r Token.Val
      | "with" => r Token.With
      | "withtype" => r Token.Withtype
      | "while" => r Token.While
      (** Modules *)
      | "eqtype" => r Token.Eqtype
      | "functor" => r Token.Functor
      | "include" => r Token.Include
      | "sharing" => r Token.Sharing
      | "sig" => r Token.Sig
      | "signature" => r Token.Signature
      | "struct" => r Token.Struct
      | "structure" => r Token.Structure
      | "where" => r Token.Where

      (** Other *)
      | _ => Token.AlphanumId
    end


  (** ======================================================================
    * The final algorithm just invokes the state-machine loop and then
    * cleans up reserved words.
    *)

  fun tokens chars =
    let
      val toks = Seq.fromList (List.rev (loop_topLevel [] (Stream.make chars)))

      fun replaceWithReserved (tok as {start, stop, class}: Token.t) =
        { start = start
        , stop = stop
        , class =
            case class of
              Token.SymbolicId =>
                clarifyClassOfSymbolic (start, stop, Seq.nth chars)
            | Token.AlphanumId =>
                clarifyClassOfAlphanum (start, stop, Seq.nth chars)
            | _ => class
        }
    in
      Seq.map replaceWithReserved toks
    end

end
