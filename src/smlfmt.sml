(** Copyright (c) 2021 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure TCS = TerminalColorString
structure TC = TerminalColors
fun boldc c x = TCS.bold (TCS.foreground c (TCS.fromString x))
fun printErr m = TextIO.output (TextIO.stdErr, m)

val optionalArgDesc =
"  [--force]              overwrite files without interactive confirmation\n\
\  [--preview]            show formatted before writing to file\n\
\  [--preview-only]       show formatted output and skip file overwrite\n\
\                         (incompatible with --force)\n\
\  [-max-width W]         try to use at most <W> columns in each line\n\
\                         (default 80)\n\
\  [-ribbon-frac R]       controls how dense each line should be\n\
\                         (default 1.0; requires 0 < R <= 1)\n\
\  [-tab-width T]         parse input tab-stops as having width <T>\n\
\                         (default 4)\n\
\  [-indent-width I]      use <I> spaces for indentation in output\n\
\                         (default 2)\n\
\  [-mlb-path-var 'K V']  MLton-style path variable\n\
\  [-engine E]            Select a pretty printing engine.\n\
\                         Valid options are: prettier, pretty\n\
\                         (default 'prettier')\n\
\  [--help]               print this message\n"

fun usage () =
  "usage: smlfmt [ARGS] FILE ... FILE\n" ^
  "Optional arguments:\n" ^
  optionalArgDesc


val mlbPathVars = CommandLineArgs.parseStrings "mlb-path-var"
val ribbonFrac = CommandLineArgs.parseReal "ribbon-frac" 1.0
val maxWidth = CommandLineArgs.parseInt "max-width" 80
val tabWidth = CommandLineArgs.parseInt "tab-width" 4
val indentWidth = CommandLineArgs.parseInt "indent-width" 2
val engine = CommandLineArgs.parseString "engine" "prettier"
val inputfiles = CommandLineArgs.positional ()

val doForce = CommandLineArgs.parseFlag "force"
val doHelp = CommandLineArgs.parseFlag "help"
val preview = CommandLineArgs.parseFlag "preview"
val previewOnly = CommandLineArgs.parseFlag "preview-only"
val showPreview = preview orelse previewOnly

val _ =
  if doHelp orelse List.null inputfiles then
    ( print (usage ())
    ; OS.Process.exit OS.Process.success
    )
  else ()

val _ =
  if previewOnly andalso doForce then
    ( TCS.printErr (boldc Palette.red
        "ERROR: --force incompatible with --preview-only\n")
    ; OS.Process.exit OS.Process.failure
    )
  else ()


val prettyPrinter =
  case engine of
    "prettier" => PrettierPrintAst.pretty
  | "pretty" => PrettyPrintAst.pretty
  | other =>
      ( TCS.printErr (boldc Palette.red
          ("ERROR: unknown engine '"
           ^ other
           ^ "'; valid options are: prettier, pretty\n"))
      ; OS.Process.exit OS.Process.failure
      )


val pathmap = MLtonPathMap.getPathMap ()
val pathmap =
  List.concat (List.map MLtonPathMap.fromString mlbPathVars) @ pathmap

fun handleLexOrParseError exn =
  let
    val e =
      case exn of
        Error.Error e => e
      | other => raise other
    val hist = MLton.Exn.history exn
  in
    TCS.print
      (Error.show {highlighter = SOME SyntaxHighlighter.fuzzyHighlight} e);
    if List.null hist then () else
      print ("\n" ^ String.concat (List.map (fn ln => ln ^ "\n") hist));
    OS.Process.exit OS.Process.failure
  end


fun exnToString exn =
  let
    val header = "UNHANDLED EXCEPTION: " ^ exnMessage exn
    val stackTrace =
      if List.null (MLton.Exn.history exn) then
        ""
      else
        "\nSTACK TRACE:\n" ^
        List.foldl op^ ""
          (List.map (fn s => "  " ^ s ^ "\n") (MLton.Exn.history exn))
  in
    header ^ stackTrace
  end


fun doSMLAst (fp, ast) =
  let
    val hfp = FilePath.toHostPath fp

    val prettied =
      (prettyPrinter
        { ribbonFrac = ribbonFrac
        , maxWidth = maxWidth
        , tabWidth = tabWidth
        , indent = indentWidth
        }
        ast)

    val result = TCS.toString {colors=false} prettied

    fun writeOut () =
      let
        val outstream = TextIO.openOut hfp
      in
        printErr ("formatting " ^ hfp ^ "\n");
        TextIO.output (outstream, result);
        TextIO.output (outstream, "\n");
        TextIO.closeOut outstream
      end

    fun confirm () =
      ( print ("overwrite " ^ hfp ^ " [y/N]? ")
      ; case TextIO.inputLine TextIO.stdIn of
          NONE => printErr ("skipping " ^ hfp ^ "\n")
        | SOME line =>
            if line = "y\n" orelse line = "Y\n" then
              writeOut ()
            else
              printErr ("skipping " ^ hfp ^ "\n")
      )
  in
    if not showPreview then ()
    else
      ( TCS.print (boldc Palette.lightblue ("---- " ^ hfp ^ " ----"))
      ; print "\n"
      ; TCS.print prettied
      ; print "\n"
      ; TCS.print (boldc Palette.lightblue ("--------"))
      ; print "\n"
      );

    if previewOnly then ()
    else if doForce then
      writeOut ()
    else
      confirm ()
  end
  handle exn =>
    TCS.printErr (boldc Palette.red (exnToString exn ^ "\n"))


fun doSML filepath =
  let
    val fp = FilePath.fromUnixPath filepath
    val source = Source.loadFromFile fp
    val ast =
      Parser.parse source
      handle exn => handleLexOrParseError exn
  in
    doSMLAst (fp, ast)
  end


fun doMLB filepath =
  let
    val fp = FilePath.fromUnixPath filepath
    val asts =
      ParseAllSMLFromMLB.parse {skipBasis = true, pathmap = pathmap} fp
      handle exn => handleLexOrParseError exn
  in
    Util.for (0, Seq.length asts) (fn i =>
      doSMLAst (Seq.nth asts i)
    )
  end


datatype fileinfo =
  FileError of exn
| Unsupported of string
| MissingExtension
| MLBFile
| SMLFile

fun fileinfo filepath =
  let
    val eo = OS.Path.ext filepath
  in
    case eo of
      NONE => MissingExtension
    | SOME "mlb" => MLBFile
    | SOME e =>
        if List.exists (fn e' => e = e') ["sml","fun","sig"] then
          SMLFile
        else
          Unsupported e
  end
  handle exn => FileError exn

fun okayFile (filepath, info) =
  case info of SMLFile => true | MLBFile => true | _ => false

fun skipFile (filepath, info) =
  printErr ("skipping file " ^ filepath ^ ": "
  ^ (case info of
      MissingExtension => "missing extension"
    | Unsupported e => "unsupported file extension: " ^ e
    | FileError exn => exnMessage exn
    | _ => raise Fail "Error! Bug! Please submit an error report...")
  ^ "\n")

val (filesToDo, filesToSkip) =
  List.partition okayFile (List.map (fn x => (x, fileinfo x)) inputfiles)

fun doFile (fp, info) =
  case info of
    SMLFile => doSML fp
  | MLBFile => doMLB fp
  | _ => raise Fail "Error! Bug! Please submit an error report..."

val _ =
  List.app skipFile filesToSkip

val _ =
  List.app doFile filesToDo
