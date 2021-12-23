(** Copyright (c) 2021 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

val mlbPathVars = CommandLineArgs.parseStrings "mlb-path-var"
val ribbonFrac = CommandLineArgs.parseReal "ribbon-frac" 1.0
val maxWidth = CommandLineArgs.parseInt "max-width" 80
val tabWidth = CommandLineArgs.parseInt "tab-width" 4
val indentWidth = CommandLineArgs.parseInt "indent-width" 2
fun infile () = List.hd (CommandLineArgs.positional ())

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


fun doMLB () =
  if true then
    raise Fail "TODO: handle .mlb ..."
  else
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
  handle exn => handleLexOrParseError exn

fun doSML () =
  let
    val source = Source.loadFromFile (FilePath.fromUnixPath (infile ()))
    val ast = Parser.parse source
  in
    TerminalColorString.print
      (PrettyPrintAst.pretty
        { ribbonFrac=ribbonFrac
        , maxWidth=maxWidth
        , tabWidth=tabWidth
        , indent=indentWidth
        }
        ast);
    print "\n"
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

val _ =
  (case OS.Path.ext (infile ()) of
    SOME "mlb" => doMLB()
  | SOME e =>
      if List.exists (fn e' => e = e') ["sml","fun","sig"] then
        doSML ()
      else
        extError (SOME e)
  | _ => extError NONE)
  handle e =>
    ( printErr (exnMessage e ^ "\n\n")
    ; printErr (usage ())
    )
