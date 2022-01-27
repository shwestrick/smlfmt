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
    | Token.RealConstant => 19
    | Token.WordConstant => 19
    | Token.CharConstant => 18
    | Token.StringConstant => 18
    | Token.Identifier =>
        if Token.isTyVar tok then
          6
        else
          8
    | Token.LongIdentifier => 0
    | Token.Reserved _ => 15
    | Token.MLtonReserved => 15
    | _ => raise Fail "SemanticTokens.tokenType: unsupported token class"

  val qualifiertt = 0
  val vartt = 8

  fun encode toks =
    let
      val toks = Seq.filter (not o Token.isWhitespace) toks

      fun sourceInfo tt src =
        let
          val {line, col} = Source.absoluteStart src
        in
          (line-1, col-1, Source.length src, tt)
        end

      fun tokInfo tok =
        if Token.isComment tok orelse Token.isStringConstant tok then
          (** Comments or strings might be multiline tokens. Not all clients
            * support multiline semantic tokens. Rather than check for whether
            * or not they do... screw it. Just split into multiple lines.
            *)
          let
            val whole = Token.getSource tok
            val linePieces =
              Seq.map (fn (i, j) => Source.slice whole (i, j-i))
                (Source.lineRanges whole)
          in
            Seq.map (sourceInfo (tokenType tok)) linePieces
          end
        else
          case Token.splitLongIdentifier tok of
            NONE =>
              (* tok is not a long identifier *)
              Seq.singleton (sourceInfo (tokenType tok) (Token.getSource tok))

          | SOME srcs =>
              let
                val lastIdx = Seq.length srcs - 1
              in
                Seq.mapIdx (fn (i, src) =>
                    sourceInfo (if i = lastIdx then vartt else qualifiertt) src)
                  srcs
              end

      val infos = Seq.flatten (Seq.map tokInfo toks)

      fun encodeAt i =
        let
          val (lPrev, cPrev) =
            if i = 0 then
              (0, 0)
            else
              let val (l, c, _, _) = Seq.nth infos (i-1) in (l, c) end

          val (l, c, n, tt) = Seq.nth infos i

          val dl = l - lPrev
          val dc = if l = lPrev then c-cPrev else c
        in
          Seq.fromList
            [ dl             (* deltaLine *)
            , dc             (* deltaStartChar *)
            , n              (* length *)
            , tt             (* tokenType *)
            , 0              (* tokenModifier *)
            ]
        end
    in
      Seq.flatten (Seq.tabulate encodeAt (Seq.length infos))
    end


  fun makeResponse serverState {id, uri} =
    let
      val toks = Lexer.tokens (ServerState.get serverState uri)
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
    handle exn =>
      let
        (*
        val response =
          Json.OBJECT (Json.objFromList
            [ ("id", Message.Id.toJson id)
            , ( "error"
              , Json.OBJECT (Json.objFromList
                  [ ("code", Json.NUMBER "-32700")
                  , ("message", Json.STRING (exnMessage exn))
                  ])
              )
            ])
        *)

        val response =
          Json.OBJECT (Json.objFromList
            [ ("id", Message.Id.toJson id)
            , ("result", Json.NULL)
            ])
      in
        case exn of
          Error.Error e =>
            TerminalColorString.printErr
              (Error.show {highlighter = SOME SyntaxHighlighter.fuzzyHighlight} e)
        | _ => ();

        response
      end



end
