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

  val green = TCS.foreground (TC.hsv {h=120.0, s=1.0, v=0.75})
  val darkgreen = TCS.foreground (TC.hsv {h=120.0, s=0.8, v=0.45})
  val red = TCS.foreground (TC.hsv {h=0.0, s=1.0, v=0.65})
  val yellow = TCS.foreground (TC.hsv {h=60.0, s=0.75, v=0.65})
  val blue = TCS.foreground (TC.hsv {h=240.0, s=0.65, v=0.85})
  val lightblue = TCS.foreground (TC.hsv {h=180.0, s=1.0, v=0.75})
  val pink = TCS.foreground (TC.hsv {h=300.0, s=1.0, v=0.75})
  val purple = TCS.foreground (TC.hsv {h=269.0, s=0.94, v=1.0})
  val gray = TCS.foreground (TC.hsv {h=0.0, s=0.0, v=0.55})

  fun tokColor class =
    case class of
      Token.StringConstant =>
        red
    | Token.CharConstant =>
        purple
    | Token.WordConstant =>
        yellow
    | Token.Comment =>
        TCS.italic o gray
    | Token.IntegerConstant =>
        lightblue
    | Token.RealConstant =>
        green
    | Token.Reserved _ =>
        TCS.bold o blue
    | Token.LongIdentifier =>
        pink
    | Token.Identifier =>
        darkgreen
    | Token.MLtonReserved =>
        darkgreen

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
