(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure Terminal =
struct

  (** This is a major hack. Going to need to find a better way. *)

  open MLton.Process

  val p = create
    { path = "/usr/bin/tput"
    , env = NONE
    , args = [ "cols" ]
    , stderr = Param.self
    , stdin = Param.null
    , stdout = Param.pipe
    }

  val currentCols =
    valOf (Int.fromString (TextIO.inputAll (Child.textIn (getStdout p))))

end
