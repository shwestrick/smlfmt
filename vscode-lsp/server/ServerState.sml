structure ServerState =
struct

  fun log str =
    TextIO.output (TextIO.stdErr, str ^ "\n")

  fun uriCmp (u1, u2) =
    String.compare (URI.toString u1, URI.toString u2)

  structure URIDict =
    Dict (struct type t = URI.t val compare = uriCmp end)

  datatype t =
    T of
      { openFiles: Source.t URIDict.t  (* lexer output for now *)
      }

  fun textDocumentDidOpen (T {openFiles}) uri =
    let
      val filepath = FilePath.fromUnixPath (URI.path uri)
      val source = Source.loadFromFile filepath
    in
      log ("loaded file " ^ FilePath.toUnixPath filepath);
      T {openFiles = URIDict.insert openFiles (uri, source)}
    end

  fun textDocumentDidChange (T {openFiles}) {uri, contentChanges} =
    let
      fun loop changes source =
        case changes of
          [] => source
        | Message.ContentChange.Range {range, text} :: changes' =>
            loop changes' (Source.edit source range text)
        | Message.ContentChange.Whole text :: changes' =>
            let
              val range =
                { start = Source.absoluteStart source
                , stop = Source.absoluteEnd source
                }
            in
              loop changes' (Source.edit source range text)
            end

      val newSource = loop contentChanges (URIDict.lookup openFiles uri)
    in
      T {openFiles = URIDict.insert openFiles (uri, newSource)}
    end

  val initialState = T {openFiles = URIDict.empty}

  fun get (T {openFiles}) uri =
    URIDict.lookup openFiles uri

end
