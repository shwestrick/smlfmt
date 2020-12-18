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
  | Token.Qualifier =>
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
      , Token.Qualifier
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

fun loop acc (toks, i) {line=currLine, col=currCol} =
  if i >= Seq.length toks then
    print (String.concat (List.rev acc) ^ "\n")
  else
    let
      val {source, class} = Seq.nth toks i
      val {line, col} = Source.absoluteStart source

      val spaces =
        if line = currLine then
          CharVector.tabulate (col-currCol, fn _ => #" ")
        else
          CharVector.tabulate (line-currLine, fn _ => #"\n")
          ^ CharVector.tabulate (col-1, fn _ => #" ")

      val item = spaces ^ tokColor class ^ Source.toString source ^ TC.reset
    in
      loop (item :: acc) (toks, i+1) (Source.absoluteEnd source)
    end

val _ =
  let
    val infile = List.hd (CommandLine.arguments ())
    val source = Source.loadFromFile (FilePath.fromUnixPath infile)
    val (toks, err) =
      case Lexer.tokens source of
        LexResult.Success toks => (toks, NONE)
      | LexResult.Failure {partial=toks, error=err} => (toks, SOME err)
  in
    case err of
      SOME e => print (LexResult.report e ^ "\n")
    | _ =>
        ( loop [] (toks, 0) {line=1, col=1}
        ; print "\n"
        ; printLegend ()
        )
  end

