(** Copyright (c) 2021 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure MLBToken =
struct

  datatype reserved =
    Bas
  | Basis
  | Ann

  datatype class =
    SMLPath
  | MLBPath
  | Reserved of reserved
  | SML of Token.class

  type token = class WithSource.t
  type t = token

  fun make src class =
    WithSource.make {value = class, source = src}

  fun reserved src rclass =
    WithSource.make {value = Reserved rclass, source = src}

  fun fromSMLToken tok =
    make (Token.getSource tok) (SML (Token.getClass tok))

  fun getClass tok =
    WithSource.valOf tok

  fun getSource tok =
    WithSource.srcOf tok

  fun isComment tok =
    case WithSource.valOf tok of
      SML Token.Comment => true
    | _ => false

  fun isSMLPath tok =
    case WithSource.valOf tok of
      SMLPath => true
    | _ => false

  fun isMLBPath tok =
    case WithSource.valOf tok of
      MLBPath => true
    | _ => false

  fun isStringConstant tok =
    case WithSource.valOf tok of
      SML Token.StringConstant => true
    | _ => false

  val basDecStartTokens =
    [ SMLPath
    , MLBPath
    , Reserved Basis
    , Reserved Ann
    , SML Token.StringConstant
    , SML (Token.Reserved Token.Open)
    , SML (Token.Reserved Token.Local)
    , SML (Token.Reserved Token.Structure)
    , SML (Token.Reserved Token.Signature)
    , SML (Token.Reserved Token.Functor)
    ]

  fun isBasDecStartToken tok =
    let
      val c = getClass tok
    in
      List.exists (fn c' => c' = c) basDecStartTokens
    end


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
        NONE => NONE
      | SOME i =>
          SOME (Source.toString (Source.slice src (i, Source.length src - i)))
    end


  fun makePath src =
    case extensionOfPathInSource src of
      SOME ".mlb" => SOME (make src MLBPath)
    | SOME ".sml" => SOME (make src SMLPath)
    | SOME ".sig" => SOME (make src SMLPath)
    | SOME ".fun" => SOME (make src SMLPath)
    | _ => NONE

end
