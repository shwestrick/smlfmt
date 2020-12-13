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
  | Identifier
  | Qualifier

  type token = {source: Source.t, class: class}
  type t = token

  fun make src class =
    {source = src, class = class}

  fun reserved src rclass =
    {source = src, class = Reserved rclass}

  fun qualifier src =
    {source = src, class = Qualifier}

  fun identifier src =
    {source = src, class = Identifier}

  fun tryReserved src =
    let
      val str = CharVector.tabulate (Source.length src, Source.nth src)
      fun r rclass = SOME rclass
    in
      case str of
      (** Symbolic *)
        ":" => r Colon
      | ":>" => r ColonArrow
      | "|" => r Bar
      | "=" => r Equal
      | "=>" => r FatArrow
      | "->" => r Arrow
      | "#" => r Hash
      (** Core *)
      | "abstype" => r Abstype
      | "and" => r And
      | "andalso" => r Andalso
      | "as" => r As
      | "case" => r Case
      | "datatype" => r Datatype
      | "do" => r Do
      | "else" => r Else
      | "end" => r End
      | "exception" => r Exception
      | "fn" => r Fn
      | "fun" => r Fun
      | "handle" => r Handle
      | "if" => r If
      | "in" => r In
      | "infix" => r Infix
      | "infixr" => r Infixr
      | "let" => r Let
      | "local" => r Local
      | "nonfix" => r Nonfix
      | "of" => r Of
      | "op" => r Op
      | "open" => r Open
      | "orelse" => r Orelse
      | "raise" => r Raise
      | "rec" => r Rec
      | "then" => r Then
      | "type" => r Type
      | "val" => r Val
      | "with" => r With
      | "withtype" => r Withtype
      | "while" => r While
      (** Modules *)
      | "eqtype" => r Eqtype
      | "functor" => r Functor
      | "include" => r Include
      | "sharing" => r Sharing
      | "sig" => r Sig
      | "signature" => r Signature
      | "struct" => r Struct
      | "structure" => r Structure
      | "where" => r Where

      | other => NONE
          (* (print ("not reserved: " ^ other ^ "\n"); NONE) *)
    end

  fun reservedOrIdentifier src =
    case tryReserved src of
      SOME r => reserved src r
    | NONE => identifier src

  fun isReserved (tok: token) =
    case #class tok of
      Reserved _ => true
    | _ => false

  fun classToString class =
    case class of
      Comment => "comment"
    | Reserved _ => "reserved"
    | IntegerConstant => "integer"
    | WordConstant => "word"
    | RealConstant => "real"
    | StringConstant => "string"
    | Identifier => "identifier"
    | Qualifier => "qualifier"


  fun isValidQualifier src =
    Source.length src > 0 andalso
    Source.nth src 0 <> #"'" andalso
    List.all LexUtils.isAlphaNumPrimeOrUnderscore
      (List.tabulate (Source.length src, Source.nth src))


  fun switchIdentifierToQualifier (tok: token) =
    case #class tok of
      Identifier =>
        if isValidQualifier (#source tok) then
          {source = #source tok, class = Qualifier}
        else
          raise Fail ("Token.switchIdentifierToQualifier on invalid qualifier: "
                      ^ Source.toString (#source tok))
    | cls =>
        raise Fail ("Token.switchIdentifierToQualifier " ^ classToString cls)

end
