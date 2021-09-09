(** Copyright (c) 2021 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure MLBToken =
struct

  datatype reserved =
    Bas
  | Basis
  | Ann

  datatype class =
    SMLPath
  | MLBPath
  | Reserved of reserved
  | SML of Token.class

  type token = class WithSource.t
  type t = token

  fun make src class =
    WithSource.make {value = class, source = src}

  fun reserved src rclass =
    WithSource.make {value = Reserved rclass, source = src}

  fun fromSMLToken tok =
    make (Token.getSource tok) (SML (Token.getClass tok))

end
