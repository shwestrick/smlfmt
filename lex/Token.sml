structure Token =
struct

  datatype reserved =
  (** ============ core ============ *)
    Abstype
  | And
  | Andalso
  | As
  | Case
  | Datatype
  | Do
  | Else
  | End
  | Exception
  | Fn
  | Fun
  | Handle
  | If
  | In
  | Infix
  | Infixr
  | Let
  | Local
  | Nonfix
  | Of
  | Op
  | Open
  | Orelse
  | Raise
  | Rec
  | Then
  | Type
  | Val
  | With
  | Withtype
  | While
  | OpenParen
  | CloseParen
  | OpenSquareBracket
  | CloseSquareBracket
  | OpenCurlyBracket
  | CloseCurlyBracket
  | Comma
  | Colon
  | Semicolon
  | DotDotDot
  | Underscore
  | Bar
  | Equal
  | FatArrow
  | Arrow
  | Hash
  (** ============ modules ============ *)
  | Eqtype
  | Functor
  | Include
  | Sharing
  | Sig
  | Signature
  | Struct
  | Structure
  | Where
  | ColonArrow

  datatype class =
    Comment
  | Reserved of reserved
  | IntegerConstant
  | WordConstant
  | RealConstant
  | StringConstant
  | AlphanumId
  | SymbolicId
  | Other

  type token = {start: int, stop: int, class: class}
  type t = token

  fun comma pos =
    {start = pos, stop = pos+1, class = Reserved Comma}

  fun underscore pos =
    {start = pos, stop = pos+1, class = Reserved Underscore}

  fun semicolon pos =
    {start = pos, stop = pos+1, class = Reserved Semicolon}

  fun colon pos =
    {start = pos, stop = pos+1, class = Reserved Colon}

  fun colonArrow pos =
    {start = pos, stop = pos+1, class = Reserved ColonArrow}

  fun openParen pos =
    {start = pos, stop = pos+1, class = Reserved OpenParen}

  fun closeParen pos =
    {start = pos, stop = pos+1, class = Reserved CloseParen}

  fun openSquareBracket pos =
    {start = pos, stop = pos+1, class = Reserved OpenSquareBracket}

  fun closeSquareBracket pos =
    {start = pos, stop = pos+1, class = Reserved CloseSquareBracket}

  fun openCurlyBracket pos =
    {start = pos, stop = pos+1, class = Reserved OpenCurlyBracket}

  fun closeCurlyBracket pos =
    {start = pos, stop = pos+1, class = Reserved CloseCurlyBracket}

  fun comment (start, stop) =
    {start = start, stop = stop, class = Comment}

  fun string (start, stop) =
    {start = start, stop = stop, class = StringConstant}

  fun symbolicId (start, stop) =
    {start = start, stop = stop, class = SymbolicId}

  fun alphanumId (start, stop) =
    {start = start, stop = stop, class = AlphanumId}

  fun integerConstant (start, stop) =
    {start = start, stop = stop, class = IntegerConstant}

  fun realConstant (start, stop) =
    {start = start, stop = stop, class = RealConstant}

  fun wordConstant (start, stop) =
    {start = start, stop = stop, class = WordConstant}

  fun reserved (start, stop, rclass) =
    {start = start, stop = stop, class = Reserved rclass}

  fun dotdotdot start =
    {start = start, stop = start+3, class = Reserved DotDotDot}

  fun classToString class =
    case class of
      Comment => "comment"
    | Reserved _ => "reserved"
    | IntegerConstant => "integer"
    | WordConstant => "word"
    | RealConstant => "real"
    | StringConstant => "string"
    | AlphanumId => "alphanum-id"
    | SymbolicId => "symbolic-id"
    | Other => "other"

end
