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

fun mainLoop () =
  let
    val msg = Message.receive ()
    val _ = log ("received: " ^ Message.toString msg)
  in
    case msg of
      Message.Initialize {id, ...} =>
        print (serializeMessage (initialResponse (Message.Id.toJson id)))
    | Message.Initialized =>
        log "initialization handshake completed"
    | _ => ();

    mainLoop ()
  end
  handle e =>
    ( log ("Whoops: " ^ exnMessage e)
    ; mainLoop ()
    )

val _ = mainLoop ()

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
