(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure InfixDict :>
sig
  type t

  exception TopLevel
  val popScope: t -> t
  val newScope: t -> t
  val numScopes: t -> int

  val initialTopLevel: t

  val insert: t -> (Token.t * int) -> t
  val contains: t -> Token.t -> bool

  exception NotFound
  val lookup: t -> Token.t -> int

  val higherPrecedence: t -> (Token.t * Token.t) -> bool

end =
struct
  structure D =
    ScopedDict
      (struct
        type t = string
        val equal: t * t -> bool = op=
      end)

  open D

  type t = int D.t

  val initialTopLevel: t =
    topLevelFromList
      [ ("div", 7), ("mod", 7), ("*", 7), ("/", 7)
      , ("+", 6), ("-", 6), ("^", 6)
      , ("::", 5), ("@", 5)
      , ("=", 4), ("<", 4), (">", 4), ("<=", 4), (">=", 4), ("<>", 4)
      , (":=", 3), ("o", 3)
      , ("before", 0)
      ]

  fun contains d tok =
    D.contains d (Token.toString tok)

  fun insert d (tok, prec) =
    D.insert d (Token.toString tok, prec)

  fun lookup d tok =
    D.lookup d (Token.toString tok)

  fun higherPrecedence d (tok1, tok2) =
    lookup d tok1 > lookup d tok2
end
