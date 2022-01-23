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

      type tok_info = {line: int, startChar: int, length: int, tokenType: int, tokenModifiers: int}

      fun encodeOne (tok: Token.t) : tok_info =
        let
          val src = Token.getSource tok
          val {line, col} = Source.absoluteStart src
          val line = line-1
          val col = col-1
        in
          { line = line
          , startChar = col
          , length = Source.length src
          , tokenType = tokenType tok
          , tokenModifiers = 0 (* nothing for now *)
          }
        end

      val tokInfos = Seq.map encodeOne toks

      fun delta (prev: tok_info) (curr: tok_info) =
        { deltaLine = #line curr - #line prev
        , deltaStartChar =
            if #line prev = #line curr then
              #startChar curr - #startChar prev
            else
              #startChar curr
        , length = #length curr
        , tokenType = #tokenType curr
        , tokenModifiers = #tokenModifiers curr
        }

      fun deltaAt i =
        if i > 0 then
          delta (Seq.nth tokInfos (i-1)) (Seq.nth tokInfos i)
        else
          let val {line, startChar, length, tokenType, tokenModifiers} =
                Seq.nth tokInfos i
          in { deltaLine = line
             , deltaStartChar = startChar
             , length = length
             , tokenType = tokenType
             , tokenModifiers = tokenModifiers
             }
          end

      val output = ForkJoin.alloc (5 * Seq.length toks)
    in
      ForkJoin.parfor 1000 (0, Seq.length toks) (fn i =>
        let
          val {deltaLine, deltaStartChar, length, tokenType, tokenModifiers} =
            deltaAt i
          fun put j x = Array.update (output, 5*i + j, x)
        in
          put 0 deltaLine;
          put 1 deltaStartChar;
          put 2 length;
          put 3 tokenType;
          put 4 tokenModifiers
        end);

      ArraySlice.full output
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
