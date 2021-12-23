(** Copyright (c) 2021 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

val mlbPathVars = CommandLineArgs.parseStrings "mlb-path-var"
val ribbonFrac = CommandLineArgs.parseReal "ribbon-frac" 1.0
val maxWidth = CommandLineArgs.parseInt "max-width" 80
val tabWidth = CommandLineArgs.parseInt "tab-width" 4
val indentWidth = CommandLineArgs.parseInt "indent-width" 2
val infile = List.hd (CommandLineArgs.positional ())

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
    raise Fail "smlfmt doMLB needs work"
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
        (FilePath.fromUnixPath infile)
  in
    print "\nParsing succeeded.\n"
  end
  handle exn => handleLexOrParseError exn

fun doSML () =
  let
    val source = Source.loadFromFile (FilePath.fromUnixPath infile)
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
