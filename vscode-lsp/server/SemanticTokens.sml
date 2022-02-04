structure SemanticTokens:
sig

  val encode: (Token.t * InterestingTokensFromAst.info) Seq.t -> int Seq.t

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
    let
      open Token
    in
      case getClass tok of
        Comment => 17
      | IntegerConstant => 19
      | RealConstant => 19
      | WordConstant => 19
      | CharConstant => 18
      | StringConstant => 18
      | Identifier =>
          if isTyVar tok then
            6
          else
            8
      | LongIdentifier => 0
      | Reserved r =>
          (case r of
            OpenParen => 20
          | CloseParen => 20
          | OpenCurlyBracket => 20
          | CloseCurlyBracket => 20
          | OpenSquareBracket => 20
          | CloseSquareBracket => 20
          | Comma => 20
          | Arrow => 20
          (* | FatArrow => 20 *)
          | Underscore => 20
          | DotDotDot => 20
          | Semicolon => 20
          | Colon => 20
          | ColonArrow => 20

          | Fun => 14
          | Val => 14
          | Datatype => 14
          | Open => 14
          | Infix => 14
          | Infixr => 14
          | Nonfix => 14
          | Structure => 14
          | Type => 14
          | Exception => 14
          | And => 14
          | If => 14
          | Then => 14
          | Else => 14

          | _ => 15
          )
      | MLtonReserved => 15
      | _ => raise Fail "SemanticTokens.tokenType: unsupported token class"
    end

  val qualifiertt = (*0*) 5
  val vartt = 8

  fun refine tt interestingNess =
    let
      open InterestingTokensFromAst
    in
      case interestingNess of
        Function => 12
      | InfixOp => 20
      | Constructor => 10
      | StructureId => 5
      | Label => 14
      | SpecialKeyword => 14
      | StarInTupleType => 20
      | Type => 1
      | _ => tt
    end

  fun encode toks =
    let
      fun sourceInfo tt src =
        let
          val {line, col} = Source.absoluteStart src
        in
          (line-1, col-1, Source.length src, tt)
        end

      fun tokInfo (tok, interestingNess) =
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
            Seq.map (sourceInfo (refine (tokenType tok) interestingNess)) linePieces
          end
        else
          case Token.splitLongIdentifier tok of
            NONE =>
              (* tok is not a long identifier *)
              Seq.singleton
                (sourceInfo (refine (tokenType tok) interestingNess)
                  (Token.getSource tok))

          | SOME srcs =>
              let
                val lastIdx = Seq.length srcs - 1
              in
                Seq.mapIdx (fn (i, src) =>
                    sourceInfo
                      (if i = lastIdx then
                         refine vartt interestingNess
                       else qualifiertt)
                      src)
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



  fun mergeTokensWithInfo
    (lexToks: Token.t Seq.t)
    (interestingToks: (Token.t * InterestingTokensFromAst.info) Seq.t)
    =
    let
      open InterestingTokensFromAst
      val () = ()

      (** This is a bit of a mess, but...
        * The gist is to sort in order to remove duplicates. We know that
        * each token will appear at most twice: once from lexing, and again
        * if it is interesting. To group duplicate tokens, we can sort by their
        * start offset. But we also would like to ensure that if a token is
        * interesting, it appears earlier (to make it easy to deduplicate by
        * just taking the earlier in any set of duplicates). So, we sort
        * lexicographically first by start offset, and then next by
        * interesting-ness.
        *)
      fun cmp ((t1, c1), (t2, c2)) =
        case
          Int.compare
            ( Source.absoluteStartOffset (Token.getSource t1)
            , Source.absoluteStartOffset (Token.getSource t2)
            )
        of
          EQUAL =>
            (case (c1, c2) of
              (NotInteresting, NotInteresting) => EQUAL
            | (NotInteresting, _) => GREATER
            | _ => LESS
            )
        | other => other

      val lexToks =
        Seq.map (fn t => (t, NotInteresting)) lexToks
      val together =
        Mergesort.sort cmp (Seq.append (lexToks, interestingToks))

      fun isUnique (i, (tCurr, _)) =
        if i = 0 then true else
        let
          val (tPrev, _) = Seq.nth together (i-1)
        in
          (* keep it if it is different from the previous *)
          Source.absoluteStartOffset (Token.getSource tPrev)
          <>
          Source.absoluteStartOffset (Token.getSource tCurr)
        end
    in
      Seq.filterIdx isUnique together
    end


  (** These tokens are kept directly from the lexing info.
    *
    * For tokens that we want to extract from parsing info, see
    * InterestingTokensFromAst
    *)
  fun keepToken tok =
    let
      open Token
    in
      case getClass tok of
        CharConstant => true
      | StringConstant => true
      | _ => false
    end


  fun makeResponse serverState {id, uri} =
    let
      val contents = ServerState.get serverState uri
      val toks = Seq.filter keepToken (Lexer.tokens contents)

      val ast = Parser.parse contents
      val interestingToks =
        InterestingTokensFromAst.extract ast
        handle _ => Seq.empty ()

      val allToks = mergeTokensWithInfo toks interestingToks

      val data = encode allToks

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
