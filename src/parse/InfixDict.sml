(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure InfixDict :>
sig
  type t

  datatype assoc = AssocLeft | AssocRight

  val empty: t
  val initialTopLevel: t

  val setInfix: t -> (Token.t * int * assoc) -> t
  val setNonfix: t -> Token.t -> t
  val isInfix: t -> Token.t -> bool
  val merge: t * t -> t

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
    Dict
      (struct
        type t = string
        val compare: t * t -> order = String.compare
      end)

  open D

  datatype assoc = AssocLeft | AssocRight
  datatype fixity = Nonfix | Infix of (int * assoc)

  type t = fixity D.t

  fun L (str, p) = (str, Infix (p, AssocLeft))
  fun R (str, p) = (str, Infix (p, AssocRight))

  val initialTopLevel: t =
    fromList
      [ L("div", 7), L("mod", 7), L("*", 7), L("/", 7)
      , L("+", 6), L("-", 6), L("^", 6)
      , R("::", 5), R("@", 5)
      , L("=", 4), L("<", 4), L(">", 4), L("<=", 4), L(">=", 4), L("<>", 4)
      , L(":=", 3), L("o", 3)
      , L("before", 0)
      ]

  fun isInfix d tok =
    case D.find d (Token.toString tok) of
      SOME (Infix _) => true
    | _ => false

  fun setInfix d (tok, prec, assoc) =
    D.insert d (Token.toString tok, Infix (prec, assoc))

  fun setNonfix d tok =
    D.insert d (Token.toString tok, Nonfix)

  fun merge (d1, d2) =
    D.unionWith (fn (_, x) => x) (d1, d2)

  fun lookupPrecedence (d: t) tok =
    case D.find d (Token.toString tok) of
      SOME (Infix (p, _)) => p
    | _ => raise NotFound

  fun lookupAssoc (d: t) tok =
    case D.find d (Token.toString tok) of
      SOME (Infix (_, a)) => a
    | _ => raise NotFound

  fun associatesLeft d tok =
    AssocLeft = lookupAssoc d tok

  fun associatesRight d tok =
    AssocRight = lookupAssoc d tok

  fun higherPrecedence d (tok1, tok2) =
    lookupPrecedence d tok1 > lookupPrecedence d tok2

  fun samePrecedence d (tok1, tok2) =
    lookupPrecedence d tok1 = lookupPrecedence d tok2
end
