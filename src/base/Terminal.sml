(** Copyright (c) 2020-2021 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure Terminal =
struct

  open MLton.Process

  val defaultCols = 60

  val tputPath =
    case FindInPath.find "tput" of
      SOME path => FilePath.toHostPath path
    | NONE => "tput"

  fun currentCols () =
    let
      val p = create
        { path = tputPath
        , env = NONE
        , args = [ "cols" ]
        , stderr = Param.self
        , stdin = Param.null
        , stdout = Param.pipe
        }
    in
      valOf (Int.fromString (TextIO.inputAll (Child.textIn (getStdout p))))
    end
    handle _ => defaultCols

end
