fun die msg =
  ( TextIO.output (TextIO.stdErr, msg ^ "\n")
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

fun consumeInput () =
  let
    val _ = expect "Content-Length:"
    val contentLength = valOf (Int.fromString (getUntilSeparator ()))
    val _ = getUntilSeparator()

    val jsonContents = TextIO.inputN (TextIO.stdIn, contentLength)
  in
    TextIO.output (TextIO.stdErr, "Received contents: " ^ jsonContents);
    consumeInput ()
  end

val _ = consumeInput ()
