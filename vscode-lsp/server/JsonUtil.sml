structure JsonUtil:
sig

  (** takes a json string as input, and interprets its escaped sequences
    * into true characters.
    *
    * For example:
    *   val s = unescape "\\\r\n"
    *   val 3 = String.size s
    *   val 10 = Char.ord (String.sub (s, 2))
    *)
  val unescape: string -> string

end =
struct

  fun unescape str =
    let
      val n = String.size str
      fun char i = String.sub (str, i)
      fun string i len = String.substring (str, i, len)

      fun loop acc i =
        if i >= String.size str then
          String.implode (List.rev acc)
        else case char i of
          #"\\" =>
            (case char (i+1) of
              #"\"" => loop (#"\"" :: acc) (i+2)
            | #"\\" => loop (#"\\" :: acc) (i+2)
            | #"b" => loop (#"\b" :: acc) (i+2)
            | #"f" => loop (#"\f" :: acc) (i+2)
            | #"n" => loop (#"\n" :: acc) (i+2)
            | #"r" => loop (#"\r" :: acc) (i+2)
            | #"t" => loop (#"\t" :: acc) (i+2)
            | #"u" =>
                raise Fail "JsonUtil.unescape: unicode escape not permitted"
            | other =>
                raise Fail "JsonUtil.unescape: invalid escape sequence"
            )
        | other => loop (other :: acc) (i+1)
    in
      loop [] 0
    end

end
