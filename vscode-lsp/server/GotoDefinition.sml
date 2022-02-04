structure GotoDefinition:
sig

  val makeResponse: ServerState.t
                 -> { id: Message.Id.t
                    , uri: URI.t
                    , position: {line: int, col: int}
                    }
                 -> Json.t

end =
struct

  fun log str =
    TextIO.output (TextIO.stdErr, str ^ "\n")

  (** if you're searching for an element X: `test Y = cmp (X, Y)` *)
  fun binsearch (s: 'a Seq.t) (whereIsTargetInComparisonTo: 'a -> order) =
    let
      fun loop lo hi =
        case hi - lo of
          0 => NONE
        | n =>
            let
              val mid = lo + n div 2
              val pivot = Seq.nth s mid
            in
              case whereIsTargetInComparisonTo pivot of
                LESS    => loop lo mid
              | EQUAL   => SOME pivot
              | GREATER => loop (mid+1) hi
            end
    in
      loop 0 (Seq.length s)
    end


  fun tokenAtPosition (tokens: Token.t Seq.t) lineColPosition =
    if Seq.length tokens = 0 then NONE else
    let
      val wholefile = Source.wholeFile (Token.getSource (Seq.nth tokens 0))
      val targetOffset = Source.lineColToOffset wholefile lineColPosition

      fun whereIsTarget token =
        let
          val src = Token.getSource token
          val startOff = Source.absoluteStartOffset src
          val stopOff = Source.absoluteEndOffset src
        in
          if targetOffset < startOff then
            LESS
          else if targetOffset >= startOff andalso targetOffset < stopOff then
            EQUAL
          else if targetOffset = stopOff then
            case Token.nextToken token of
              NONE => EQUAL
            | SOME nextTok =>
                if Source.absoluteStartOffset (Token.getSource nextTok) = stopOff
                then
                  GREATER
                else
                  EQUAL
          else
            GREATER
        end
    in
      binsearch tokens whereIsTarget
    end


  fun nullResponse id =
    Json.OBJECT (Json.objFromList
      [ ("id", Message.Id.toJson id)
      , ("result", Json.NULL)
      ])


  fun makeResponse serverState {id, uri, position} =
    let
      val contents = ServerState.get serverState uri
      val tokens = Token.regroup
        (Seq.filter (not o Token.isWhitespace) (Lexer.tokens contents))
      val ast = Parser.parse contents
      val bs = BindingSites.fromAst ast

      val tok = tokenAtPosition tokens position
      val site = Option.mapPartial (BindingSites.bindingSite bs) tok

      fun lcToJson {line, col} =
        (** have to convert back to 0-indexing... *)
        Json.OBJECT (Json.objFromList
          [ ("line", Json.NUMBER (Int.toString (line-1)))
          , ("character", Json.NUMBER (Int.toString (col-1)))
          ])

      val result =
        case site of
          NONE => Json.NULL
        | SOME tok =>
            let
              val start = Source.absoluteStart (Token.getSource tok)
              val stop = Source.absoluteEnd (Token.getSource tok)
            in
              Json.OBJECT (Json.objFromList
                [ ( "uri"
                  , Json.STRING (URI.toString uri)
                  )
                , ( "range"
                  , Json.OBJECT (Json.objFromList
                      [ ("start", lcToJson start)
                      , ("end", lcToJson stop)
                      ])
                  )
                ])
            end
    in
      Json.OBJECT (Json.objFromList
        [ ("id", Message.Id.toJson id)
        , ("result", result)
        ])
    end
    handle exn =>
      let
        val response = nullResponse id
      in
        case exn of
          Error.Error e =>
            let
              val errorInfo =
                Error.show {highlighter = SOME SyntaxHighlighter.fuzzyHighlight} e
                handle ee =>
                  TerminalColorString.fromString
                    ( "Got an error, but then another error occurred while \
                      \reporting the first: "
                    ^ exnMessage ee ^ "\n"
                    ^ String.concat (List.map (fn ln => "  " ^ ln ^ "\n")
                        (MLton.Exn.history ee))
                    )
            in
              TerminalColorString.printErr errorInfo
            end

        | _ => ();

        response
      end

end
