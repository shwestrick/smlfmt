structure Message =
struct

  fun log str =
    TextIO.output (TextIO.stdErr, str ^ "\n")

  fun die msg =
    ( log msg
    ; OS.Process.exit OS.Process.failure
    )

  structure Id :>
  sig
    type t
    val fromJson: Json.t -> t
    val toJson: t -> Json.t
    val toString: t -> string
  end =
  struct
    type t = Json.t
    fun fromJson x = x
    fun toJson x = x
    fun toString x = Json.toString x
  end

  structure ContentChange =
  struct
    datatype t =
      Range of {range: Source.range, text: string}
    | Whole of string

    fun lctos {line, col} =
      "{line = " ^ Int.toString line ^ ", col = " ^ Int.toString col ^ "}"

    fun toString x =
      case x of
        Range {range = {start, stop}, text} =>
          "Range(start = " ^ lctos start
          ^ ", stop = " ^ lctos stop
          ^ ", text = \"" ^ text ^ "\")"
      | Whole text =>
          "Whole(\"" ^ text ^ "\")"
  end

  datatype t =
    Initialize of
      { id: Id.t
      , processId: int option
      , clientInfo: {name: string, version: string option} option
      , locale: string option
      , initializationOptions: Json.t option
      , capabilities: Json.t
      , trace: Json.t option
      , workspaceFolders: {name: string, uri: URI.t} list option
      }

  | Initialized

  | TextDocumentDidOpen of
      { uri: URI.t
      , languageId: string
      , version: int
      , text: string
      }

  | TextDocumentDidChange of
      { uri: URI.t
      , version: int
      , contentChanges: ContentChange.t list
      }

  | TextDocumentSemanticTokensFull of
      { id: Id.t
      , uri: URI.t
      }

  | TextDocumentDefinition of
      { id: Id.t
      , uri: URI.t
      , position: {line: int, col: int}
      }

  | Other of Json.obj


  fun required parser key obj =
    case Json.objLook obj key of
      SOME x => parser x
    | NONE => raise Fail ("Message.required: missing key '" ^ key ^ "'")

  fun optional (parser: Json.t -> 'a) key obj : 'a option =
    case Json.objLook obj key of
      SOME x => SOME (parser x)
    | NONE => NONE

  fun object j =
    case j of
      Json.OBJECT obj => obj
    | _ => raise Fail "Message.object: non-object"

  fun int (x: Json.t) : int =
    case x of
      Json.NUMBER s => valOf (Int.fromString s)
    | _ => raise Fail "Message.int: non-number"

  fun array parser x =
    case x of
      Json.ARRAY elems => List.map parser elems
    | _ => raise Fail "Message.array: non-array"

  fun string j =
    case j of
      Json.STRING s => s
    | _ => raise Fail "Message.string: non-string"

  fun json j = j

  fun makeInitialize obj =
    let
      val id = required Id.fromJson "id" obj
      val params = required object "params" obj

      fun clientInfo x =
        { name = required string "name" x
        , version = optional string "version" x
        }

      fun workspaceFolder x =
        { name = required string "name" x
        , uri = required (URI.fromString o string) "uri" x
        }

      val workspaceFolders = array (workspaceFolder o object)
    in
      Initialize
        { id = id
        , processId = optional int "processId" params
        , clientInfo = optional (clientInfo o object) "clientInfo" params
        , locale = optional string "locale" params
        , initializationOptions = optional json "initializationOptions" params
        , capabilities = required json "capabilities" params
        , trace = optional json "trace" params
        , workspaceFolders = optional workspaceFolders "workspaceFolders" params
        }
    end


  fun makeTextDocumentDidOpen obj =
    let
      val params = required object "params" obj
      val textDocument = required object "textDocument" params
    in
      TextDocumentDidOpen
        { uri = required (URI.fromString o string) "uri" textDocument
        , languageId = required string "languageId" textDocument
        , version = required int "version" textDocument
        , text = required string "text" textDocument
        }
    end


  fun makeTextDocumentSemanticTokensFull obj =
    let
      val id = required Id.fromJson "id" obj
      val params = required object "params" obj
      val textDocument = required object "textDocument" params
      val uri = required (URI.fromString o string) "uri" textDocument
    in
      TextDocumentSemanticTokensFull
        { id = id
        , uri = uri
        }
    end


  fun makeTextDocumentDidChange obj =
    let
      val params = required object "params" obj
      val textDocument = required object "textDocument" params
      val uri = required (URI.fromString o string) "uri" textDocument
      val version = required int "version" textDocument

      fun positionSourceLineCol obj =
        let
          val line = required int "line" obj
          val character = required int "character" obj
        in
          (* convert to 1-indexing *)
          {line = line+1, col = character+1}
        end

      fun range obj =
        let
          val start = required (positionSourceLineCol o object) "start" obj
          val stop = required (positionSourceLineCol o object) "end" obj
        in
          {start = start, stop = stop}
        end

      fun contentChange obj =
        let
          val text = required string "text" obj
          val range = optional (range o object) "range" obj
        in
          case range of
            SOME r => ContentChange.Range {range = r, text = text}
          | NONE => ContentChange.Whole text
        end

      val contentChanges =
        required (array (contentChange o object)) "contentChanges" params
    in
      TextDocumentDidChange
        { uri = uri
        , version = version
        , contentChanges = contentChanges
        }
    end


  fun makeTextDocumentDefinition obj =
    let
      val id = required Id.fromJson "id" obj
      val params = required object "params" obj
      val textDocument = required object "textDocument" params
      val uri = required (URI.fromString o string) "uri" textDocument

      fun position obj =
        let
          val line = required int "line" obj
          val character = required int "character" obj
        in
          (* convert to 1-indexing *)
          {line = line+1, col = character+1}
        end

      val position = required (position o object) "position" params
    in
      TextDocumentDefinition
        { id = id
        , uri = uri
        , position = position
        }
    end


  fun fromJson json =
    let
      val obj =
        case json of
          Json.OBJECT obj => obj
        | _ => raise Fail "Message.fromJson: non-object"
    in
      case Json.objLook obj "method" of
        SOME (Json.STRING "initialize") =>
          makeInitialize obj

      | SOME (Json.STRING "initialized") =>
          Initialized

      | SOME (Json.STRING "textDocument/didOpen") =>
          makeTextDocumentDidOpen obj

      | SOME (Json.STRING "textDocument/didChange") =>
          makeTextDocumentDidChange obj

      | SOME (Json.STRING "textDocument/definition") =>
          makeTextDocumentDefinition obj

      | SOME (Json.STRING "textDocument/semanticTokens/full") =>
          makeTextDocumentSemanticTokensFull obj

      | _ =>
          Other obj
    end

  fun expect str =
    let
      val n = String.size str
      val stuff = TextIO.inputN (TextIO.stdIn, n)
    in
      if stuff = str then () else
      die ("Expected '" ^ str ^ "' but got '" ^ stuff ^ "'")
  	end

  fun getUntilSeparator () =
    let
      fun loop (#"\n" :: #"\r" :: rest) = String.implode (List.rev rest)
        | loop acc =
            case TextIO.input1 TextIO.stdIn of
              SOME c => loop (c :: acc)
            | NONE => die "Input stream ended unexpectedly"
    in
      loop []
    end

  fun receive () =
    let
      val _ = expect "Content-Length:"
      val contentLength = valOf (Int.fromString (getUntilSeparator ()))
      val _ = getUntilSeparator()
      val payload = TextIO.inputN (TextIO.stdIn, contentLength)
    in
      (* log ("received payload: " ^ payload); *)
      fromJson (Json.fromString payload)
    end

  fun myJsonToString json =
    case json of
      Json.RAW x => "RAW(" ^ x ^ ")"
    | Json.OBJECT obj => "OBJECT(...)"
    | Json.STRING str => "STRING(" ^ str ^ ")"
    | Json.ARRAY xs => "ARRAY[" ^ String.concatWith "," (List.map myJsonToString xs) ^ "]"
    | Json.NULL => "NULL"
    | Json.BOOL b => "BOOL(" ^ (if b then "true" else "false") ^ ")"
    | Json.NUMBER x => "NUMBER(" ^ x ^ ")"

  fun toString msg =
    case msg of
      Other obj => "OTHER(" ^ Json.toString (Json.OBJECT obj) ^ ")"
    | Initialize {id, capabilities, workspaceFolders, ...} =>
        "Initialize(id = " ^ Id.toString id
        ^ ", ..., capabilities = " ^ Json.toString capabilities
        ^ (case workspaceFolders of
            NONE => ""
          | SOME folders =>
              ", workspaceFolders = ["
              ^ String.concatWith "," (List.map
                  (fn {name, uri} =>
                    "{name = \"" ^ name ^ "\", uri = \"" ^ URI.toString uri ^ "\"}")
                  folders)
              ^ "]")
        ^ ")"
    | Initialized =>
        "Initialized()"
    | TextDocumentDidOpen {uri, languageId, version, text} =>
        "TextDocumentDidOpen("
        ^ "uri = " ^ URI.toString uri ^ " , "
        ^ "languageId = " ^ languageId ^ ", "
        ^ "version = " ^ Int.toString version ^ ", "
        ^ "text = " ^ Json.toString (Json.STRING text)
        ^ ")"
    | TextDocumentDidChange {uri, version, contentChanges} =>
        "TextDocumentDidChange("
        ^ "uri = " ^ URI.toString uri ^ " , "
        ^ "version = " ^ Int.toString version ^ ", "
        ^ "contentChanges = [" ^
            String.concatWith ","
              (List.map ContentChange.toString contentChanges)
          ^ "]"
        ^ ")"
    | TextDocumentDefinition {id, uri, position} =>
        "TextDocumentDefinition("
        ^ "id = " ^ Id.toString id ^ ", "
        ^ "uri = " ^ URI.toString uri ^ " , "
        ^ "position = " ^ ContentChange.lctos position ^ ")"
    | TextDocumentSemanticTokensFull {id, uri} =>
        "TextDocumentSemanticTokensFull("
        ^ "id = " ^ Id.toString id ^ ", "
        ^ "uri = " ^ URI.toString uri ^ " )"

end
