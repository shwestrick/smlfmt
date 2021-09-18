(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure InfixDict :>
sig
  type t

  datatype assoc = AssocLeft | AssocRight

  exception TopLevel
  val popScope: t -> t
  val newScope: t -> t
  val numScopes: t -> int

  val empty: t
  val initialTopLevel: t

  val insert: t -> (Token.t * int * assoc) -> t
  val remove: t -> Token.t -> t
  val contains: t -> Token.t -> bool

  exception NotFound
  val lookupPrecedence: t -> Token.t -> int
  val lookupAssoc: t -> Token.t -> assoc
  val associatesLeft: t -> Token.t -> bool
  val associatesRight: t -> Token.t -> bool

  val higherPrecedence: t -> (Token.t * Token.t) -> bool
  val samePrecedence: t -> (Token.t * Token.t) -> bool

end =
struct
  structure D =
    ScopedDict
      (struct
        type t = string
        val equal: t * t -> bool = op=
      end)

  open D

  datatype assoc = AssocLeft | AssocRight

  type t = (int * assoc) D.t

  fun L (str, p) = (str, (p, AssocLeft))
  fun R (str, p) = (str, (p, AssocRight))

  val initialTopLevel: t =
    topLevelFromList
      [ L("div", 7), L("mod", 7), L("*", 7), L("/", 7)
      , L("+", 6), L("-", 6), L("^", 6)
      , R("::", 5), R("@", 5)
      , L("=", 4), L("<", 4), L(">", 4), L("<=", 4), L(">=", 4), L("<>", 4)
      , L(":=", 3), L("o", 3)
      , L("before", 0)
      ]

  val empty = topLevelFromList []

  fun contains d tok =
    D.contains d (Token.toString tok)

  fun insert d (tok, prec, assoc) =
    D.insert d (Token.toString tok, (prec, assoc))

  fun remove d tok =
    D.remove d (Token.toString tok)

  fun lookupPrecedence (d: t) tok =
    #1 (D.lookup d (Token.toString tok))

  fun lookupAssoc (d: t) tok =
    #2 (D.lookup d (Token.toString tok))

  fun associatesLeft d tok =
    AssocLeft = lookupAssoc d tok

  fun associatesRight d tok =
    AssocRight = lookupAssoc d tok

  fun higherPrecedence d (tok1, tok2) =
    lookupPrecedence d tok1 > lookupPrecedence d tok2

  fun samePrecedence d (tok1, tok2) =
    lookupPrecedence d tok1 = lookupPrecedence d tok2
end
