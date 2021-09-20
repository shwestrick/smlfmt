(** Copyright (c) 2021 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure Error:
sig

  datatype err =
    LineError of LineError.t
  | ErrorReport of ErrorReport.t

  exception Error of err

  val show: err -> string
end =
struct

  datatype err =
    LineError of LineError.t
  | ErrorReport of ErrorReport.t

  exception Error of err

  fun show err =
    case err of
      LineError e => LineError.show e
    | ErrorReport e => ErrorReport.show e

end
