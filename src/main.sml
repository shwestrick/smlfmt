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
val purple = TC.foreground (TC.hsv {h=269.0, s=0.94, v=1.0})
val gray = TC.foreground (TC.hsv {h=0.0, s=0.0, v=0.55})
val black = TC.foreground TC.black

fun tokColor class =
  case class of
    Token.StringConstant =>
      red
  | Token.CharConstant =>
      purple
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
  | Token.MLtonReserved =>
      darkgreen

fun tokColorMLB class =
  case class of
    MLBToken.MLBPath =>
      lightblue
  | MLBToken.SMLPath =>
      red
  | MLBToken.Reserved _ =>
      TC.bold ^ blue
  | MLBToken.SML c =>
      tokColor c

fun printLegend () =
  let
    val classes =
      [ Token.Comment
      , Token.StringConstant
      , Token.WordConstant
      , Token.IntegerConstant
      , Token.CharConstant
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

fun loop tokColor (wholeSrc, i) (toks, j) =
  if i >= Source.length wholeSrc then
    ()
  else if
    j >= Seq.length toks orelse
    Source.absoluteStartOffset (WithSource.srcOf (Seq.nth toks j)) > i
  then
    ( TextIO.output1 (TextIO.stdOut, Source.nth wholeSrc i)
    ; loop tokColor (wholeSrc, i+1) (toks, j)
    )
  else
    let
      val {source=thisSrc, value=class} = WithSource.unpack (Seq.nth toks j)
    in
      print (tokColor class);
      print (Source.toString thisSrc);
      print TC.reset;
      loop tokColor (wholeSrc, Source.absoluteEndOffset thisSrc) (toks, j+1)
    end

val mlbPathVars = CommandLineArgs.parseStrings "mlb-path-var"
val errorsOnly = CommandLineArgs.parseFlag "errors-only"
val infile = List.hd (CommandLineArgs.positional ())
(* val source = Source.loadFromFile (FilePath.fromUnixPath infile) *)

val pathmap = MLtonPathMap.getPathMap ()
val pathmap =
  List.concat (List.map MLtonPathMap.fromString mlbPathVars) @ pathmap

fun vprint msg =
  if errorsOnly then () else print msg

fun handleLexOrParseError exn =
  let
    val e =
      case exn of
        Error.Error e => e
      | other => raise other
    val hist = MLton.Exn.history exn
  in
    print (Error.show e);
    if List.null hist then () else
      print ("\n" ^ String.concat (List.map (fn ln => ln ^ "\n") hist));
    OS.Process.exit OS.Process.failure
  end


fun doMLB () =
  let
    (* val tokens =
      MLBLexer.tokens source *)
      (* handle exn => handleLexOrParseError exn *)

    (* val _ =
      if errorsOnly then () else
        ( loop tokColorMLB (source, 0) (tokens, 0)
        ; print "\n"
        ) *)

    val _ =
      ( print "==== PATH MAP ====\n"
      ; List.app
          (fn (key, value) => print (key ^ " " ^ value ^ "\n"))
          pathmap
      ; print "==================\n"
      )

    val ast =
      ParseAllSMLFromMLB.parse pathmap (FilePath.fromUnixPath infile)

    (* fun printloop i =
      if i >= Seq.length allSMLPaths then () else
      let
        val p = Seq.nth allSMLPaths i
      in
        print ("  " ^ FilePath.toUnixPath p ^ "\n");
        printloop (i+1)
      end *)
  in
    (* Util.for (0, Seq.length allSMLPaths) (fn i =>
      ( print ("parsing " ^ FilePath.toUnixPath (Seq.nth allSMLPaths i) ^ "\n")
      ; Parser.parse (Source.loadFromFile (Seq.nth allSMLPaths i))
      ; ()
      )
    ) *)
    (* print "Specifies these SML files:\n"; *)
    (* printloop 0 *)
    print "\nParsing succeeded.\n"
  end
  handle exn => handleLexOrParseError exn


fun doSML () =
  let
    val source = Source.loadFromFile (FilePath.fromUnixPath infile)
    val _ =
      if errorsOnly then () else
      let
        val toks =
          Lexer.tokens source
          handle exn => handleLexOrParseError exn

      in
        loop tokColor (source, 0) (toks, 0);
        print "\nLexing succeeded.\n"
      end


    val _ = vprint "Parsing...\n\n"

    val ast =
      Parser.parse source
      handle exn => handleLexOrParseError exn

    val _ =
      vprint (PrettyPrintAst.pretty ast ^ "\n")

    val _ = vprint "\nParsing succeeded.\n"
  in
    ()
  end


fun extError eo =
  ( case eo of
      SOME e => print ("Unsupported file extension: " ^ e ^ "\n")
    | NONE => print ("Missing file extension\n")
  ; OS.Process.exit OS.Process.failure
  )

val _ =
  case OS.Path.ext infile of
    SOME "mlb" => doMLB()
  | SOME e =>
      if List.exists (fn e' => e = e') ["sml","fun","sig"] then
        doSML ()
      else
        extError (SOME e)
  | _ => extError NONE
