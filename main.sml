(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

(** ========================================================================
  * Some coloring utilities
  *)

structure TC = TerminalColors

val green = TC.foreground (TC.hsv {h=120.0, s=1.0, v=0.75})
val darkgreen = TC.foreground (TC.hsv {h=120.0, s=0.8, v=0.45})
val red = TC.foreground (TC.hsv {h=0.0, s=1.0, v=0.65})
val yellow = TC.foreground (TC.hsv {h=60.0, s=0.75, v=0.65})
val blue = TC.foreground (TC.hsv {h=240.0, s=0.65, v=0.85})
val lightblue = TC.foreground (TC.hsv {h=180.0, s=1.0, v=0.75})
val pink = TC.foreground (TC.hsv {h=300.0, s=1.0, v=0.75})
val gray = TC.foreground (TC.hsv {h=0.0, s=0.0, v=0.55})
val black = TC.foreground TC.black

fun tokColor class =
  case class of
    Token.StringConstant =>
      red
  | Token.WordConstant =>
      yellow
  | Token.Comment =>
      TC.italic ^ gray
  | Token.IntegerConstant =>
      lightblue
  | Token.RealConstant =>
      green
  | Token.Reserved _ =>
      TC.bold ^ blue
  | Token.LongIdentifier =>
      pink
  | Token.Identifier =>
      darkgreen

fun printLegend () =
  let
    val classes =
      [ Token.Comment
      , Token.StringConstant
      , Token.WordConstant
      , Token.IntegerConstant
      , Token.RealConstant
      , Token.Reserved Token.And  (* arbitrary... just need something reserved *)
      , Token.Identifier
      , Token.LongIdentifier
      ]
    val boxWidth =
      List.foldl Int.max 0
      (List.map (fn c => String.size (Token.classToString c)) classes)
    val boxTop =
      "+" ^ (String.implode (List.tabulate (boxWidth+2, fn _ => #"-"))) ^ "+\n"
    fun makeLine pre text post =
      let
        val width = String.size text
        val padding = String.implode
          (List.tabulate (Int.max (0, boxWidth-width), fn _ => #" "))
      in
        "| " ^ pre ^ text ^ post ^ padding ^ " |\n"
      end
    fun makeClassLine c =
      let
        val text = Token.classToString c
        val pre = tokColor c
        val post = TC.reset
      in
        makeLine pre text post
      end
  in
    print boxTop;
    print (makeLine "" "LEGEND" "");
    print boxTop;
    List.app (print o makeClassLine) classes;
    print boxTop
  end


(** ==========================================================================
  * Parse input file and color it.
  *)

fun loop (wholeSrc, i) (toks, j) =
  if i >= Source.length wholeSrc then
    ()
  else if
    j >= Seq.length toks orelse
    Source.absoluteStartOffset (WithSource.srcOf (Seq.nth toks j)) > i
  then
    ( TextIO.output1 (TextIO.stdOut, Source.nth wholeSrc i)
    ; loop (wholeSrc, i+1) (toks, j)
    )
  else
    let
      val {source=thisSrc, value=class} = WithSource.unpack (Seq.nth toks j)
    in
      print (tokColor class);
      print (Source.toString thisSrc);
      print TC.reset;
      loop (wholeSrc, Source.absoluteEndOffset thisSrc) (toks, j+1)
    end

val _ =
  let
    val infile = List.hd (CommandLine.arguments ())
    val source = Source.loadFromFile (FilePath.fromUnixPath infile)
  in
    case Lexer.tokens source of
      MaybeError.Error e =>
        print (LineError.show e)
    | MaybeError.Success toks =>
        ( loop (source, 0) (toks, 0)
        ; print "\n"
        ; printLegend ()
        )
  end

