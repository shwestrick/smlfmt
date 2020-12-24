(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure LineError:
sig
  type t =
    { pos: Source.t
    , what: string
    , explain: string option
    }

  type err = t

  val show: err -> string
end =
struct

  type t =
    { pos: Source.t
    , what: string
    , explain: string option
    }

  type err = t

  fun repeatChar n c =
    CharVector.tabulate (n, fn _ => c)

  fun lcToStr {line, col} =
    "(line "
    ^ Int.toString line
    ^ ", col "
    ^ Int.toString col
    ^ ")"

  val desiredWidth = Int.min (Terminal.currentCols, 80)

  fun leftPadWith char str =
    if String.size str >= desiredWidth then
      str
    else
      repeatChar (desiredWidth - String.size str) char
      ^ str

  fun rightPadWith char str =
    if String.size str >= desiredWidth then
      str
    else
      str
      ^ repeatChar (desiredWidth - String.size str) char

  fun centerPadWith char str =
    if String.size str >= desiredWidth then
      str
    else
      let
        val count = desiredWidth - String.size str
        val leftCount = count div 2
        val rightCount = count - leftCount
      in
        repeatChar leftCount char
        ^ str
        ^ repeatChar rightCount char
      end

  fun justifyWith char (left, right) =
    if String.size left + String.size right >= desiredWidth then
      left ^ right
    else
      let
        val count = desiredWidth - (String.size left + String.size right)
      in
        left
        ^ repeatChar count char
        ^ right
      end

  fun textWrap str =
    let
      fun finishLine ln = String.concatWith " " (List.rev ln)
      fun loop lines (currLine, currLen) toks =
        case toks of
          tok :: remaining =>
            if currLen + String.size tok + 1 > desiredWidth then
              loop
                (finishLine currLine :: lines)
                ([], 0)
                toks
            else
              loop
                lines
                (tok :: currLine, currLen + String.size tok + 1)
                remaining
        | [] =>
            String.concatWith "\n" (List.rev (finishLine currLine :: lines))
    in
      loop [] ([], 0) (String.tokens Char.isSpace str)
    end


  fun show {pos, what=msg, explain=extraInfo} =
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

      val extra =
        case extraInfo of
          NONE => ""
        | SOME info => textWrap info
    in
      justifyWith #"-"
        ("-- SYNTAX ERROR ", " " ^ FilePath.toHostPath (Source.fileName pos))
      ^ "\n\n"
      ^ msg ^ "\n\n"
      ^ leftMargin ^ Source.toString line ^ "\n"
      ^ leftSpaces ^ repeatChar highlightLen #"^" ^ "\n"
      ^ extra ^ "\n"
    end

end
