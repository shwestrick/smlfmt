fun log str =
  TextIO.output (TextIO.stdErr, str ^ "\n")

fun die msg =
  ( log msg
  ; OS.Process.exit OS.Process.failure
  )

(*
fun receiveMessages () : Json.t list =
  let
    fun loop msgs =
      case TextIO.canInput (TextIO.stdIn, 1) of
        NONE =>
          List.rev msgs
      | SOME _ =>
          loop (receiveMessage () :: msgs)
  in
    loop [receiveMessage ()]
  end
*)

fun inspect x =
  case x of
    Json.OBJECT obj =>
      Json.objFold op:: [] obj
  | _ => raise Fail "expected json object"

fun object kvs = Json.OBJECT (Json.objFromList kvs)
fun bool b = Json.BOOL b
fun num n = Json.NUMBER (Int.toString n)
fun array a = Json.ARRAY a

val tokenModifiers = [
        "declaration",
        "definition",
        "readonly",
        "static",
        "deprecated",
        "abstract",
        "async",
        "modification",
        "documentation",
        "defaultLibrary"
      ]

val tokenTypes = [
        "namespace",
        "type",
        "class",
        "enum",
        "interface",
        "struct",
        "typeParameter",
        "parameter",
        "variable",
        "property",
        "enumMember",
        "event",
        "function",
        "method",
        "macro",
        "keyword",
        "modifier",
        "comment",
        "string",
        "number",
        "regexp",
        "operator"
      ]

val initialResponseResult =
  object
    [ ( "capabilities"
      , object
          [ ( "textDocumentSync"
            , object
                [ ("openClose", bool true)
                , ("change", num 2) (* TextDocumentSyncKind.Incremental *)
                ]
            )
          , ( "semanticTokensProvider"
            , object
                [ ( "full"
                  , bool true
                  )
                , ( "legend"
                  , object
                      [ ( "tokenTypes"
                        , array (List.map Json.STRING tokenTypes)
                        )
                      , ( "tokenModifiers"
                        , array (List.map Json.STRING tokenModifiers)
                        )
                      ]
                  )
                ]
            )
          ]
      )
    ]

fun initialResponse requestId =
  object
    [ ("id", requestId)
    , ("result", initialResponseResult)
    ]

fun serializeMessage json =
  let
    val str = Json.toString json
    val size = String.size str
  in
    "Content-Length:" ^ Int.toString size ^ "\r\n"
    ^ "\r\n"
    ^ str
  end

fun mainLoop state =
  let
    val msg = Message.receive ()
    val _ = log ("received: " ^ Message.toString msg)

    val state =
      case msg of
        Message.Initialize {id, ...} =>
          ( print (serializeMessage (initialResponse (Message.Id.toJson id)))
          ; state
          )
      | Message.Initialized =>
          ( log "initialization handshake completed"
          ; state
          )
      | Message.TextDocumentDidOpen {uri, ...} =>
          if URI.scheme uri <> "file" then
            state
          else
            ServerState.textDocumentDidOpen uri state

      | _ => state
  in
    mainLoop state
  end
  handle e =>
    ( log ("Whoops: " ^ exnMessage e)
    ; mainLoop state
    )

val state = ServerState.initialState
val _ = mainLoop state

(*
fun consumeInput () =
  let
    val _ = expect "Content-Length:"
    val contentLength = valOf (Int.fromString (getUntilSeparator ()))
    val _ = getUntilSeparator()

    val payload = TextIO.inputN (TextIO.stdIn, contentLength)
    val json = Json.fromString payload
    val kvs = inspect json
  in
    TextIO.output (TextIO.stdErr, "Received contents: " ^ myToString json ^ "\n");
    TextIO.output (TextIO.stdErr,
      String.concatWith "\n" (List.map (fn (k, v) => k ^ ": " ^ myToString v) kvs)
      ^ "\n");
    TextIO.output (TextIO.stdErr, "\nSending: " ^ Json.toString initialResponse ^ "\n");
    print (serializeMessage initialResponse);
    TextIO.flushOut TextIO.stdOut;
    consumeInput ()
  end

val _ = consumeInput () *)
