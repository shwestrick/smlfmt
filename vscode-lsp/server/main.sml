fun log str =
  TextIO.output (TextIO.stdErr, str ^ "\n")

fun die msg =
  ( log msg
  ; OS.Process.exit OS.Process.failure
  )

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

fun receiveMessage () =
  let
    val _ = expect "Content-Length:"
    val contentLength = valOf (Int.fromString (getUntilSeparator ()))
    val _ = getUntilSeparator()
    val payload = TextIO.inputN (TextIO.stdIn, contentLength)
  in
    Json.fromString payload
  end

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

fun myToString json =
  case json of
    Json.RAW x => "RAW(" ^ x ^ ")"
  | Json.OBJECT obj => "OBJECT(...)"
  | Json.STRING str => "STRING(" ^ str ^ ")"
  | Json.ARRAY xs => "ARRAY[" ^ String.concatWith "," (List.map myToString xs) ^ "]"
  | Json.NULL => "NULL"
  | Json.BOOL b => "BOOL(" ^ (if b then "true" else "false") ^ ")"
  | Json.NUMBER x => "NUMBER(" ^ x ^ ")"

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
    val msg as (Json.OBJECT obj) = receiveMessage ()
    val kvs = inspect msg
    val _ = log "received message:"
    val _ =
      log
        (String.concatWith "\n"
          (List.map (fn (k, v) => "  " ^ k ^ ": " ^ myToString v) kvs))
  in
    case Json.objLook obj "method" of
      SOME (Json.STRING "initialize") =>
        print (serializeMessage (initialResponse (valOf (Json.objLook obj "id"))))
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
