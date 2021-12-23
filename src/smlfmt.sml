(** Copyright (c) 2021 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

val mlbPathVars = CommandLineArgs.parseStrings "mlb-path-var"
val ribbonFrac = CommandLineArgs.parseReal "ribbon-frac" 1.0
val maxWidth = CommandLineArgs.parseInt "max-width" 80
val tabWidth = CommandLineArgs.parseInt "tab-width" 4
val indentWidth = CommandLineArgs.parseInt "indent-width" 2
val inputfiles = CommandLineArgs.positional ()

val doHelp =
  CommandLineArgs.findKey "h" orelse
  CommandLineArgs.findKey "help" orelse
  CommandLineArgs.parseFlag "help"

val optionalArgDesc =
"  [-max-width W]         try to use at most <W> columns in each line\n\
\                         (default 80)\n\
\  [-ribbon-frac R]       controls how dense each line should be\n\
\                         (default 1.0; requires 0 < R <= 1)\n\
\  [-tab-width T]         parse input tab-stops as having width <T>\n\
\                         (default 4)\n\
\  [-indent-width I]      use <I> spaces for indentation in output\n\
\                         (default 2)\n\
\  [-mlb-path-var 'K V']  MLton-style path variable\n\
\  [-h/-help/--help]      print this message\n"

fun usage () =
  String.concatWith " "
    [ "usage:"
    , "smlfmt"
    , "[ARGS]"
    , "FILE.sml"
    , "..."
    , "FILE.sml"
    ]
  ^ "\nOptional arguments:\n" ^ optionalArgDesc


val _ =
  if not doHelp then () else
  ( print (usage ())
  ; OS.Process.exit OS.Process.success
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
    TerminalColorString.print
      (Error.show {highlighter = SOME SyntaxHighlighter.fuzzyHighlight} e);
    if List.null hist then () else
      print ("\n" ^ String.concat (List.map (fn ln => ln ^ "\n") hist));
    OS.Process.exit OS.Process.failure
  end


(* fun doMLB () =
  let
    val _ =
      ( print "==== PATH MAP ====\n"
      ; List.app
          (fn (key, value) => print (key ^ " " ^ value ^ "\n"))
          pathmap
      ; print "==================\n"
      )

    val ast =
      ParseAllSMLFromMLB.parse
        {skipBasis = true, pathmap = pathmap}
        (FilePath.fromUnixPath (infile ()))
  in
    print "\nParsing succeeded.\n"
  end
  handle exn => handleLexOrParseError exn *)


fun doSML filepath =
  let
    val fp = FilePath.fromUnixPath filepath
    val source = Source.loadFromFile fp
    val ast = Parser.parse source
    val result =
      TerminalColorString.toString {colors=false}
      (PrettyPrintAst.pretty
        { ribbonFrac = ribbonFrac
        , maxWidth = maxWidth
        , tabWidth = tabWidth
        , indent = indentWidth
        }
        ast)

    fun writeOut () =
      let
        val outstream = TextIO.openOut (FilePath.toHostPath fp)
      in
        TextIO.output (outstream, result);
        TextIO.output (outstream, "\n");
        TextIO.closeOut outstream
      end
  in
    print ("overwrite " ^ filepath ^ " [y/N]? ");

    case TextIO.inputLine TextIO.stdIn of
      NONE => ()
    | SOME line =>
        if line = "y\n" orelse line = "Y\n" then
          writeOut ()
        else ()
  end
  handle exn => handleLexOrParseError exn

fun printErr m =
  TextIO.output (TextIO.stdErr, m)

fun extError eo =
  ( case eo of
      SOME e => printErr ("Unsupported file extension: " ^ e ^ "\n")
    | NONE => printErr ("Missing file extension\n")
  ; OS.Process.exit OS.Process.failure
  )

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
  case info of SMLFile => true | _ => false

fun skipFile (filepath, info) =
  print ("skipping file " ^ filepath ^ ": "
  ^ (case info of
      MissingExtension => "missing extension"
    | MLBFile => ".mlb not implemented yet"
    | Unsupported e => "unsupported file extension: " ^ e
    | FileError exn => exnMessage exn
    | _ => raise Fail "Error! Bug! Please submit an error report...")
  ^ "\n")

val (filesToDo, filesToSkip) =
  List.partition (fn (_, SMLFile) => true | _ => false)
  (List.map (fn x => (x, fileinfo x)) inputfiles)

val _ =
  List.app skipFile filesToSkip

val _ =
  List.app (doSML o #1) filesToDo
