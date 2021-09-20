(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure LineError:
sig
  type t =
    { header: string
    , pos: Source.t
    , what: string
    , explain: string option
    }

  type err = t

  val show: err -> string
end =
struct

  type t =
    { header: string
    , pos: Source.t
    , what: string
    , explain: string option
    }

  type err = t

(*
  fun show {header, pos, what=msg, explain=extraInfo} =
    let
      val {line=lineNum, col=colStart} = Source.absoluteStart pos
      val {line=lineNum', col=colEnd} = Source.absoluteEnd pos
      val _ =
        if lineNum = lineNum' then ()
        else raise Fail "ShowLineError.show: end of position past end of line"

      val desiredWidth =
        Int.min (Terminal.currentCols (), 80)

      val line = Source.wholeLine pos lineNum

      val leftMargin = Int.toString lineNum ^ "| "

      val colOffset = colStart-1
      val highlightLen = colEnd - colStart

      val leftSpaces =
        TextFormat.repeatChar (String.size leftMargin + colOffset) #" "

      val extra =
        case extraInfo of
          NONE => ""
        | SOME info => TextFormat.textWrap desiredWidth info
    in
      TextFormat.spreadWith #"-" desiredWidth
        { left = "-- " ^ header ^ " "
        , right = " " ^ FilePath.toHostPath (Source.fileName pos)
        }
      ^ "\n\n"
      ^ msg ^ "\n\n"
      ^ leftMargin ^ Source.toString line ^ "\n"
      ^ leftSpaces ^ TextFormat.repeatChar highlightLen #"^" ^ "\n"
      ^ extra ^ "\n"
    end
*)

  fun show {header, pos, what, explain} =
    let
      open ErrorReport

      val elems =
        [ Paragraph what
        , SourceReference pos
        ]

      val more =
        case explain of
          NONE => []
        | SOME s => [Paragraph s]
    in
      show
        { header = header
        , content = elems @ more
        }
    end

end
