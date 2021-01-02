(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure InfixDict :>
sig
  type 'a t

  exception TopLevel
  val popScope: 'a t -> 'a t
  val newScope: 'a t -> 'a t
  val numScopes: 'a t -> int

  val initialTopLevel: int t

  val insert: 'a t -> (Token.t * 'a) -> 'a t
  val contains: 'a t -> Token.t -> bool

  exception NotFound
  val lookup: 'a t -> Token.t -> 'a

end =
  struct
    structure D =
      ScopedDict
        (struct
          type t = string
          val equal: t * t -> bool = op=
        end)

    open D

    val initialTopLevel: int t =
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
  end
