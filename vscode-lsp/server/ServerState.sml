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
      { openFiles: Token.t Seq.t URIDict.t  (* lexer output for now *)
      }

  fun textDocumentDidOpen uri (T {openFiles}) =
    let
      val filepath = FilePath.fromUnixPath (URI.path uri)
      val tokens = Lexer.tokens (Source.loadFromFile filepath)
    in
      log ("loaded file " ^ FilePath.toUnixPath filepath);
      T {openFiles = URIDict.insert openFiles (uri, tokens)}
    end

  val initialState = T {openFiles = URIDict.empty}

end
