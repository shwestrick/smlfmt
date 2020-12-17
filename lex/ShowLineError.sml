(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure ShowLineError:
sig
  (** `show position message` *)
  val show: Source.t -> string -> string
end =
struct

  fun repeatChar n c =
    CharVector.tabulate (n, fn _ => c)

  fun show pos msg =
    let
      val {line=lineNum, col=colStart} = Source.absoluteStart pos
      val {line=lineNum', col=colEnd} = Source.absoluteEnd pos
      val _ =
        if lineNum = lineNum' then ()
        else raise Fail "ShowLineError.show: end of position past end of line"

      val line = Source.wholeLine pos lineNum

      val leftMargin = Int.toString lineNum ^ "| "

      val colOffset = colStart-1
      val highlightLen = colEnd - colStart

      val leftSpaces =
        repeatChar (String.size leftMargin + colOffset) #" "

      val result =
        leftMargin ^ Source.toString line ^ "\n"
        ^ leftSpaces ^ repeatChar highlightLen #"^" ^ "\n"
        ^ leftSpaces ^ msg ^ "\n"
    in
      result
    end

end
