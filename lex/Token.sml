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
  (** The separators are the offsets of the "."s in a long identifier. For
    * short identifiers, separators is empty.
    *)
  | Identifier of {separators: int Seq.t, isSymbolic: bool}

  type token = {source: Source.t, class: class}
  type t = token

  fun make src class =
    {source = src, class = class}

  fun reserved src rclass =
    {source = src, class = Reserved rclass}

  fun checkReserved src =
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


  (** This function attempts to make an identifier token, which could be
    * a long identifier, in which case we need to find the separators and
    * make sure none of the fields are reserved.
    *)
  fun identifierOrReserved src =
    let
      (** Indices of '.' separators within this source *)
      val seps =
        Seq.filter (fn i => Source.nth src i = #".")
        (Seq.tabulate (fn i => i) (Source.length src))
      val numSeps = Seq.length seps
      val numFields = numSeps+1

(*
      val _ = print ("CHECKING: " ^ Source.toString src ^ "\n")
      val _ = print ("seps: " ^ Seq.toString Int.toString seps ^ "\n")
*)

      fun fieldStart k =
        if k = 0 then 0 else 1 + Seq.nth seps (k-1)
      fun fieldEnd k =
        if k = numSeps then Source.length src else Seq.nth seps k
      fun field k =
        Source.subseq src (fieldStart k, fieldEnd k - fieldStart k)
      fun checkNoReserved () =
        Util.for (0, numSeps+1) (fn k =>
          case checkReserved (field k) of
            NONE => ()
          | SOME rclass =>
              raise Fail ("reserved word '" ^ Source.toString (field k)
                          ^ "' in '" ^ Source.toString src ^ "'"))
      val isSymb =
        LexUtils.isSymbolic (Source.nth (field (numFields-1)) 0)
    in
      if numSeps = 0 then
        case checkReserved (field 0) of
          SOME rclass => reserved src rclass
        | NONE => make src (Identifier {separators=seps, isSymbolic=isSymb})
      else
        ( checkNoReserved ()
        ; make src (Identifier {separators=seps, isSymbolic=isSymb})
        )
    end


  fun classToString class =
    case class of
      Comment => "comment"
    | Reserved _ => "reserved"
    | IntegerConstant => "integer"
    | WordConstant => "word"
    | RealConstant => "real"
    | StringConstant => "string"
    | Identifier {isSymbolic=false, ...} => "alphanum-id"
    | Identifier {isSymbolic=true, ...} => "symbolic-id"

end
