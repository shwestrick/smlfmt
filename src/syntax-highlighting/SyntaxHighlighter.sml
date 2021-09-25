(** Copyright (c) 2021 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure SyntaxHighlighter:
sig
  (** Use just lexing info to color a sequence of tokens from a single source.
    * Tokens must be in order as they appear in the source.
    *)
  val highlight: Source.t -> TerminalColorString.t
end =
struct

  structure TCS = TerminalColorString
  structure TC = TerminalColors
  open Palette

  fun tokColor class =
    case class of
      Token.StringConstant =>
        TCS.foreground red
    | Token.CharConstant =>
        TCS.foreground purple
    | Token.WordConstant =>
        TCS.foreground yellow
    | Token.Comment =>
        TCS.italic o TCS.foreground gray
    | Token.IntegerConstant =>
        TCS.foreground lightblue
    | Token.RealConstant =>
        TCS.foreground green
    | Token.Reserved _ =>
        TCS.bold o TCS.foreground blue
    | Token.LongIdentifier =>
        TCS.foreground pink
    | Token.Identifier =>
        TCS.foreground darkgreen
    | Token.MLtonReserved =>
        TCS.foreground darkgreen

  (* fun tokColorMLB class =
    case class of
      MLBToken.MLBPath =>
        lightblue
    | MLBToken.SMLPath =>
        red
    | MLBToken.Reserved _ =>
        TC.bold ^ blue
    | MLBToken.SML c =>
        tokColor c *)


  fun loop tokColor acc (wholeSrc, i) (toks, j) =
    if i >= Source.length wholeSrc then
      acc
    else if
      j >= Seq.length toks orelse
      Source.absoluteStartOffset (WithSource.srcOf (Seq.nth toks j)) > i
    then
      let
        val c = Source.nth wholeSrc i
        val acc = TCS.append (acc, TCS.fromChar c)
      in
        loop tokColor acc (wholeSrc, i+1) (toks, j)
      end
    else
      let
        val {source=thisSrc, value=class} = WithSource.unpack (Seq.nth toks j)
        val thisOne = tokColor class (TCS.fromString (Source.toString thisSrc))
        val acc = TCS.append (acc, thisOne)
      in
        loop tokColor acc (wholeSrc, Source.absoluteEndOffset thisSrc) (toks, j+1)
      end


  fun highlight source =
    let
      val toks = Lexer.tokens source
    in
      loop tokColor TCS.empty (source, 0) (toks, 0)
    end

end
