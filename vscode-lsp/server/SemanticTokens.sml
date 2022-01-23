structure SemanticTokens:
sig

  val encode: Token.t Seq.t -> int Seq.t

  val makeResponse: ServerState.t -> {id: Message.Id.t, uri: URI.t} -> Json.t

end =
struct

  (*  0   namespace
   *  1   type
   *  2   class
   *  3   enum
   *  4   interface
   *  5   struct
   *  6   typeParameter
   *  7   parameter
   *  8   variable
   *  9   property
   *  10  enumMember
   *  11  event
   *  12  function
   *  13  method
   *  14  macro
   *  15  keyword
   *  16  modifier
   *  17  comment
   *  18  string
   *  19  number
   *  20  regexp
   *  21  operator
   *)

  fun tokenType tok =
    case Token.getClass tok of
      Token.Comment => 17
    | Token.IntegerConstant => 19
    | Token.WordConstant => 19
    | Token.CharConstant => 18
    | Token.StringConstant => 18
    | Token.Identifier => 8
    | Token.LongIdentifier => 0
    | Token.Reserved _ => 15
    | Token.MLtonReserved => 15
    | _ => raise Fail "SemanticTokens.tokenType: unsupported token class"


  fun encode toks =
    let
      val toks = Seq.filter (not o Token.isWhitespace) toks

      fun info tok =
        let
          val src = Token.getSource tok
          val {line, col} = Source.absoluteStart src
        in
          (line-1, col-1, Source.length src)
        end

      fun encodeAt i =
        let
          val (lPrev, cPrev, _) =
            if i = 0 then
              (0, 0, 0)
            else
              info (Seq.nth toks (i-1))

          val tok = Seq.nth toks i
          val (l, c, n) = info tok

          val dl = l - lPrev
          val dc = if l = lPrev then c-cPrev else c
        in
          Seq.fromList
            [ dl             (* deltaLine *)
            , dc             (* deltaStartChar *)
            , n              (* length *)
            , tokenType tok  (* tokenType *)
            , 0              (* tokenModifier *)
            ]
        end
    in
      Seq.flatten (Seq.tabulate encodeAt (Seq.length toks))
    end


  fun makeResponse serverState {id, uri} =
    let
      val toks = ServerState.get serverState uri
      val data = encode toks

      val result =
        Json.OBJECT (Json.objFromList
          [ ( "data"
            , Json.ARRAY
                (Seq.toList (Seq.map (Json.NUMBER o Int.toString) data))
            )
          ])
    in
      Json.OBJECT (Json.objFromList
        [ ("id", Message.Id.toJson id)
        , ("result", result)
        ])
    end

end
