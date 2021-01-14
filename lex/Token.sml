(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

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
  | CharConstant
  | StringConstant
  | Identifier
  | LongIdentifier

  type token = class WithSource.t
  type t = token

  fun make src class =
    WithSource.make {value = class, source = src}

  fun reserved src rclass =
    WithSource.make {value = Reserved rclass, source = src}

  fun longIdentifier src =
    WithSource.make {value = LongIdentifier, source = src}

  fun identifier src =
    WithSource.make {value = Identifier, source = src}

  fun getClass tok =
    WithSource.valOf tok

  fun getSource tok =
    WithSource.srcOf tok

  fun toString tok =
    let
      val src = getSource tok
    in
      CharVector.tabulate (Source.length src, Source.nth src)
    end

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

  fun reservedToString rc =
    case rc of
    (** Symbolic *)
      Colon => ":"
    | ColonArrow => ":>"
    | Bar => "|"
    | Equal => "="
    | FatArrow => "=>"
    | Arrow => "->"
    | Hash => "#"
    (** Core *)
    | Abstype => "abstype"
    | And => "and"
    | Andalso => "andalso"
    | As => "as"
    | Case => "case"
    | Datatype => "datatype"
    | Do => "do"
    | Else => "else"
    | End => "end"
    | Exception => "exception"
    | Fn => "fn"
    | Fun => "fun"
    | Handle => "handle"
    | If => "if"
    | In => "in"
    | Infix => "infix"
    | Infixr => "infixr"
    | Let => "let"
    | Local => "local"
    | Nonfix => "nonfix"
    | Of => "of"
    | Op => "op"
    | Open => "open"
    | Orelse => "orelse"
    | Raise => "raise"
    | Rec => "rec"
    | Then => "then"
    | Type => "type"
    | Val => "val"
    | With => "with"
    | Withtype => "withtype"
    | While => "while"
    (** Modules *)
    | Eqtype => "eqtype"
    | Functor => "functor"
    | Include => "include"
    | Sharing => "sharing"
    | Sig => "sig"
    | Signature => "signature"
    | Struct => "struct"
    | Structure => "structure"
    | Where => "where"

    | _ => raise Fail "Bug: Token.reservedToString: missing something..."

  fun reservedOrIdentifier src =
    case tryReserved src of
      SOME r => reserved src r
    | NONE => identifier src

  fun isReserved (tok: token) =
    case getClass tok of
      Reserved _ => true
    | _ => false

  fun isComment tok =
    case getClass tok of
      Comment => true
    | _ => false

  fun isComma tok =
    case getClass tok of
      Reserved Comma => true
    | _ => false

  fun isAndalso tok =
    case getClass tok of
      Reserved Andalso => true
    | _ => false

  fun isOrelse tok =
    case getClass tok of
      Reserved Orelse => true
    | _ => false

  fun isStar tok =
    let
      val src = getSource tok
    in
      Source.length src = 1 andalso Source.nth src 0 = #"*"
    end

  fun isSemicolon tok =
    case getClass tok of
      Reserved Semicolon => true
    | _ => false

  fun isValueIdentifier tok =
    case getClass tok of
      Identifier => Source.nth (getSource tok) 0 <> #"'"
    | _ => false

  fun isLongIdentifier tok =
    case getClass tok of
      LongIdentifier => true
    | _ => false

  fun isMaybeLongIdentifier tok =
    case getClass tok of
      Identifier => true
    | LongIdentifier => true
    | _ => false

  (** tyvars are small identifiers that begin with a prime *)
  fun isTyVar tok =
    case getClass tok of
      Identifier => Source.nth (getSource tok) 0 = #"'"
    | _ => false

  (** tycons are maybe long identifiers that do not begin with a prime,
    * and are not "*"
    *)
  fun isTyCon tok =
    case getClass tok of
      Identifier =>
        not (isStar tok) andalso (Source.nth (getSource tok) 0 <> #"'")
    | _ => false

  fun isMaybeLongTyCon tok =
    isTyCon tok orelse isLongIdentifier tok

  (** SML permits ints, strings, words, and chars as constants in patterns,
    * but NOT reals.
    *)
  fun isPatternConstant tok =
    case getClass tok of
      IntegerConstant => true
    | WordConstant => true
    | StringConstant => true
    | CharConstant => true
    | _ => false

  fun isConstant tok =
    case getClass tok of
      IntegerConstant => true
    | WordConstant => true
    | StringConstant => true
    | CharConstant => true
    | RealConstant => true
    | _ => false

  fun isHexIntegerConstant tok =
    let
      val src = getSource tok
    in
      case getClass tok of
        IntegerConstant =>
          Source.length src > 2 andalso
          Source.nth src 0 = #"0" andalso
          Source.nth src 1 = #"x"
      | _ => false
    end

  fun isDecimalIntegerConstant tok =
    case getClass tok of
      IntegerConstant => not (isHexIntegerConstant tok)
    | _ => false

  val decStartTokens =
    [ Abstype
    , Datatype
    , Exception
    , Infix
    , Infixr
    , Nonfix
    , Type
    , Val
    , Fun
    , Open
    , Local
    ]

  fun isDecStartToken tok =
    case getClass tok of
      Reserved rc =>
        List.exists (fn rc' => rc = rc') decStartTokens
    | _ => false

  fun classToString class =
    case class of
      Comment => "comment"
    | Reserved _ => "reserved"
    | IntegerConstant => "integer"
    | WordConstant => "word"
    | RealConstant => "real"
    | StringConstant => "string"
    | CharConstant => "char"
    | Identifier => "identifier"
    | LongIdentifier => "long identifier"


  (** Check that the text of t1 exactly matches the text of t2. Useful for
    * comparing identifier names, e.g. for infix lookup.
    *)
  fun same (t1, t2) =
    let
      val s1 = getSource t1
      val s2 = getSource t2
      val n = Source.length s1
    in
      n = Source.length s2
      andalso
      Util.loop (0, n) true (fn (b, i) =>
        b andalso Source.nth s1 i = Source.nth s2 i)
    end


  (** This is used in Parser.consume_afterExp, to see if we should stop parsing
    * the current expression and pop up to the previous context.
    *)
  fun endsCurrentExp tok =
    isDecStartToken tok
    orelse
    case getClass tok of
      Reserved rc =>
        List.exists (fn rc' => rc = rc')
          [ CloseParen
          , CloseSquareBracket
          , CloseCurlyBracket
          , Comma
          , Semicolon
          , Bar
          , Then
          , Else
          , Do
          , Of
          , And
          , In
          , End
          ]
    | _ => false

  (** This is used in Parser, to see if we should stop parsing
    * the current type and pop up to the previous context.
    *)
  (* fun endsCurrentTy tok =
    endsCurrentExp
    orelse
    case getClass tok of
      Reserved rc =>
        List.exists (fn rc' => rc = rc')
          [ Arrow
          , As
          , Colon
          , Equal
          ] *)


  (* fun isValidQualifier src =
    Source.length src > 0 andalso
    Source.nth src 0 <> #"'" andalso
    List.all LexUtils.isAlphaNumPrimeOrUnderscore
      (List.tabulate (Source.length src, Source.nth src))


  fun switchIdentifierToQualifier (tok: token) =
    case WithSource.valOf tok of
      Identifier =>
        if isValidQualifier (WithSource.srcOf tok) then
          WithSource.map (fn _ => Qualifier) tok
        else
          raise Fail ("Token.switchIdentifierToQualifier on invalid qualifier: "
                      ^ Source.toString (WithSource.srcOf tok))
    | cls =>
        raise Fail ("Token.switchIdentifierToQualifier " ^ classToString cls) *)

end
