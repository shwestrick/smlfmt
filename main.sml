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
  | Token.SymbolicId =>
      pink
  | Token.AlphanumId =>
      darkgreen
  | _ =>
      ""

fun printLegend () =
  let
    val classes =
      [ Token.Comment
      , Token.StringConstant
      , Token.WordConstant
      , Token.IntegerConstant
      , Token.RealConstant
      , Token.Reserved Token.And  (* arbitrary... just need something reserved *)
      , Token.SymbolicId
      , Token.AlphanumId
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

val infile = List.hd (CommandLine.arguments ())
val fileContents = ReadFile.contentsSeq infile

fun loop (toks, i) j =
  if j >= Seq.length fileContents then
    ( print TC.reset
    ; TextIO.output1 (TextIO.stdOut, #"\n")
    )
  else if i >= Seq.length toks then
    ( TextIO.output1 (TextIO.stdOut, Seq.nth fileContents j)
    ; loop (toks, i) (j+1)
    )
  else
    let
      val {start, stop, class} = Seq.nth toks i
    in
      if j = start then print (tokColor class) else ();
      TextIO.output1 (TextIO.stdOut, Seq.nth fileContents j);
      if j = stop-1 then print TC.reset else ();
      loop (toks, if j=stop-1 then i+1 else i) (j+1)
    end

val _ =
  let
    val toks = SeqLex.tokens fileContents
  in
    loop (toks, 0) 0
  end
  handle e => print ("ERROR: " ^ exnMessage e ^ "\n")

val _ = printLegend ()
