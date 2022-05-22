structure ServerState:
sig
  type t
  type state = t

  val initialState: state

  val roots: state -> {name: string, path: FilePath.t} list

  val get: state -> URI.t -> Source.t

  val initialize:
    state
    -> {workspaceFolders: {name: string, uri: URI.t} list option}
    -> state

  val textDocumentDidOpen:
    state
    -> {uri: URI.t, text: string}
    -> state

  val textDocumentDidChange:
    state
    -> {uri: URI.t, contentChanges: Message.ContentChange.t list}
    -> state
end =
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
      , roots: {name: string, path: FilePath.t} list
      }

  type state = t

  fun roots (T {roots=r, ...}) = r

  fun initialize (s as T {openFiles, roots}) {workspaceFolders} =
    case workspaceFolders of
      NONE => s
    | SOME folders =>
        T { openFiles = openFiles
          , roots =
              List.map
                (fn {name, uri} =>
                  { name = name
                  , path = FilePath.fromUnixPath (URI.path uri)
                  })
                folders
          }

  fun textDocumentDidOpen (T {openFiles, roots}) {uri, text} =
    let
      val filepath = FilePath.fromUnixPath (URI.path uri)
      val text = JsonUtil.unescape text
      val contents =
        Seq.tabulate (fn i => String.sub (text, i)) (String.size text)
      val source = Source.fromData (filepath, contents)
    in
      log ("loaded file " ^ FilePath.toUnixPath filepath);
      T { openFiles = URIDict.insert openFiles (uri, source)
        , roots = roots
        }
    end

  fun textDocumentDidChange (T {openFiles, roots}) {uri, contentChanges} =
    let
      fun loop changes source =
        case changes of
          [] => source
        | Message.ContentChange.Range {range, text} :: changes' =>
            loop changes' (Source.edit source range (JsonUtil.unescape text))
        | Message.ContentChange.Whole text :: changes' =>
            let
              val range =
                { start = Source.absoluteStart source
                , stop = Source.absoluteEnd source
                }
            in
              loop changes' (Source.edit source range (JsonUtil.unescape text))
            end

      val newSource = loop contentChanges (URIDict.lookup openFiles uri)
    in
      T { openFiles = URIDict.insert openFiles (uri, newSource)
        , roots = roots
        }
    end

  val initialState = T
    { openFiles = URIDict.empty
    , roots = []
    }

  fun get (T {openFiles, ...}) uri =
    URIDict.lookup openFiles uri

end
