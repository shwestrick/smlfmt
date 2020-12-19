(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)


(** Think of ASTs as sitting on top of a token sequence. Each node of the AST
  * will contain a slice of tokens. Each token of the input should appear
  * exactly once. Traversing the AST can be used to recover the original token
  * sequence. This requires including tokens for mundane things (like the
  * colons in type annotations) but is great for completely tracking
  * provenance!
  *)
signature AST =
sig

  (** A maybe-long thing can be prefaced by structure identifiers, like
    * `Hello.World.thing` but also just `thing` (which has no qualifiers)
    *)
  structure MaybeLong:
  sig
    type t =
      { qualifiers: Token.t Seq.t
      , thing: Token.t
      }
  end


  (** Used for syntactic classes that look like:
    *
    *   Xseq  ::=               -- empty
    *           | X             -- singleton
    *           | (X, ..., X)   -- sequence enclosed by parens
    *
    * e.g. tyseq, tyvarseq, etc.
    *)
  structure SyntaxSeq:
  sig
    datatype 'a t =
      Empty
    | One of Token.t
    | Many of
        { left: Token.t         (** open paren *)
        , row: 'a Seq.t         (** elements *)
        , delims: Token.t Seq.t (** commas between elements *)
        , right: Token.t        (** close paren *)
        }
  end


  structure Ty:
  sig
    datatype ty =
      Var of Token.t

    (** { lab : ty, ..., lab : ty } *)
    | Record of
        { left: Token.t
        , row: {lab: Token.t, colon: Token.t, ty: ty} Seq.t
        , delims: Token.t Seq.t
        , right: Token.t
        }

    (** tyseq longtycon *)
    | Con of
        { args: ty SyntaxSeq.t
        , id: MaybeLong.t
        }

    (** ty -> ty *)
    | Arrow of
        { from: ty
        , arrow: Token.t
        , to: ty
        }

    (** ( ty ) *)
    | Parens of
        { left: Token.t
        , ty: ty
        , right: Token.t
        }
  end


  structure Pat:
  sig
    datatype atpat =
      Wild of Token.t

    | Const of Token.t

    (** [op] longvid *)
    | Ident of
        { opp: Token.t option
        , id: MaybeLong.t
        }

    (** { lab = pat, ..., lab = pat } *)
    | Record of
        { left: Token.t
        , row: {lab: Token.t, eq: Token.t, pat: pat} Seq.t
        , delims: Token.t Seq.t  (** Gotta remember the commas too! *)
        , right: Token.t
        }

    (** ( pat ) *)
    | Parens of
        { left: Token.t
        , pat: pat
        , right: Token.t
        }


    datatype pat =
      Atpat of atpat

    (** [op] longvid atpat *)
    | Con of
        { opp: Token.t option
        , id: MaybeLong.t
        , atpat: atpat
        }

    (** pat vid pat *)
    | Infix of
        { left: pat
        , id: Token.t
        , right: pat
        }

    (** pat : ty *)
    | Typed of
        { pat: pat
        , colon: Token.t
        , ty: Ty.t
        }

    (** [op] vid [:ty] as pat *)
    | Layered of
        { opp: Token.t option
        , id: Token.t
        , ty: {colon: Token.t, ty: Ty.t} option
        , ass: Token.t    (** the `as` of course *)
        , pat: pat
        }
  end


  datatype ast =
    Foobar

  (** A fallback for unimplemented features, or for incrementality *)
  | Unknown of Token.t Seq.t

  type t = ast

end
