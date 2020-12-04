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

  type token = {source: Source.t, class: class}
  type t = token

  fun make src class =
    {source = src, class = class}

  fun reserved src rclass =
    {source = src, class = Reserved rclass}

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
