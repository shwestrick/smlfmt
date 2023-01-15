(** Copyright (c) 2023 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

(* The goal here is to check that the formatter hasn't messed anything up.
 *
 * The output of the formatter should:
 *   - lex to the exact same sequence of tokens
 *   - parse to the exact same AST
 *   - pretty-print to the same output
 *     (i.e., the formatter should be idempotent)
 *)
structure CheckOutput:
sig
  val check:
    { origLexerOutput: Token.t Seq.t
    , origParserOutput: Parser.parser_output
    , origFormattedOutput: string

    , formatter: Parser.parser_output -> string
    , allows: AstAllows.t
    , infdict: InfixDict.t option
    }
    -> bool
end =
struct

  val removeWhitespaceTokens = Seq.filter (not o Token.isWhitespace)


  fun checkTokenSeqs (ts1, ts2) =
    let
      fun checkTokens (t1, t2) =
        raise Fail
          "CheckOutput.checkTokenSeqs.checkTokens: not yet implemented..."
    in
      Seq.equal checkTokens
        (removeWhitespaceTokens ts1, removeWhitespaceTokens ts2)
    end


  fun checkParserOutputs (po1, po2) =
    case (po1, po2) of
      (Parser.JustComments cs1, Parser.JustComments cs2) =>
        checkTokenSeqs (cs1, cs2)
    | (Parser.Ast ast1, Parser.Ast ast2) => CompareAst.equal (ast1, ast2)
    | _ => false


  fun check
    { origLexerOutput
    , origParserOutput
    , origFormattedOutput
    , formatter
    , allows
    , infdict
    } =
    let
      val mockedSource = Source.make
        { fileName = FilePath.fromFields ["<output>"]
        , contents =
            Seq.tabulate (fn i => String.sub (origFormattedOutput, i))
              (String.size origFormattedOutput)
        }

      val newLexerOutput = Lexer.tokens allows mockedSource

      val newParserOutput =
        case infdict of
          NONE => Parser.parse allows newLexerOutput
        | SOME d =>
            let val (_, po) = Parser.parseWithInfdict allows d newLexerOutput
            in po
            end

      val newFormattedOutput = formatter newParserOutput
    in
      checkTokenSeqs (origLexerOutput, newLexerOutput)
      andalso checkParserOutputs (origParserOutput, newParserOutput)
      andalso origFormattedOutput = newFormattedOutput
    end

end
