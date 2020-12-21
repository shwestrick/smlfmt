(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)


(** Think of ASTs as sitting on top of a token sequence. Each node of the AST
  * represents a slice of tokens. Each token of the input should appear
  * exactly once. Traversing the AST can be used to recover the original token
  * sequence. This requires including tokens for mundane things (e.g. such as
  * the colons in type annotations) but is great for completely tracking
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
        , elems: 'a Seq.t         (** elements *)
        , delims: Token.t Seq.t (** commas between elements *)
        , right: Token.t        (** close paren *)
        }
  end


  (** ======================================================================
    * Types.
    *)
  structure Ty:
  sig
    datatype ty =
      Var of Token.t

    (** { lab : ty, ..., lab : ty } *)
    | Record of
        { left: Token.t
        , elems: {lab: Token.t, colon: Token.t, ty: ty} Seq.t
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

    type t = ty
  end


  (** ======================================================================
    * Patterns.
    *)
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
        , elems: {lab: Token.t, eq: Token.t, pat: pat} Seq.t
        , delims: Token.t Seq.t  (** Gotta remember the commas too! *)
        , right: Token.t
        }

    (** ( pat ) *)
    | Parens of
        { left: Token.t
        , pat: pat
        , right: Token.t
        }


    and pat =
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

    type t = pat
  end


  (** ======================================================================
    * Expressions and declarations.
    *)
  structure Exp:
  sig


    (** tyvarseq tycon = conbind [and tyvarseq tycon = conbind ...]
      * where conbind ::= [op] vid [of ty] [| [op] vid [of ty] ...]
      *)
    type datbind =
      { elems:
        { tyvars: Token.t SyntaxSeq.t
        , tycon: MaybeLong.t
        , eq: Token.t
        , elems:
            { opp: Token.t option
            , id: Token.t
            , arg: {off: Token.t, ty: Ty.t} option
            } Seq.t
        (** the `|` delimiters between bindings *)
        , delims: Token.t Seq.t
        } Seq.t

      (** the `and` delimiters between bindings *)
      , delims: Token.t Seq.t
      }




    datatype atexp =
      Const of Token.t

    (** [op] longvid *)
    | Ident of
        { opp: Token.t option
        , id: MaybeLong.t
        }

    (** { lab = pat, ..., lab = pat } *)
    | Record of
        { left: Token.t
        , elems: {lab: Token.t, eq: Token.t, exp: exp} Seq.t
        , delims: Token.t Seq.t  (** Gotta remember the commas too! *)
        , right: Token.t
        }

    (** ( pat ) *)
    | Parens of
        { left: Token.t
        , exp: exp
        , right: Token.t
        }

    (** let dec in exp end *)
    | LetInEnd of
        { lett: Token.t
        , dec: dec
        , inn: Token.t
        , exp: exp
        , endd: Token.t
        }

    and exp =
      Atexp of atexp

    (** exp atexp *)
    | App of
        { left: exp     (** the function expression *)
        , right: atexp  (** the argument expression *)
        }

    (** exp vid exp *)
    | Infix of
       { left: exp
       , id: Token.t
       , right: exp
       }

    (** exp : ty *)
    | Typed of
        { exp: exp
        , colon: Token.t
        , ty: Ty.t
        }

    (** exp handle pat => exp [| pat => exp ...] *)
    | Handle of
        { exp: exp
        , handlee: Token.t
        , elems: {pat: Pat.t, arrow: Token.t, exp: exp} Seq.t
        , delims: Token.t Seq.t   (** the bars between match rules *)
        }

    (** raise exp *)
    | Raise of
        { raisee: Token.t
        , exp: exp
        }

    (** fn pat => exp [| pat => exp ...] *)
    | Fn of
        { fnn: Token.t
        , elems: {pat: Pat.t, arrow: Token.t, exp: exp} Seq.t
        , delims: Token.t Seq.t   (** the bars between match rules *)
        }




    and dec =
      DecEmpty

    (** val tyvarseq [rec] pat = exp [and [rec] pat = exp ...] *)
    | DecVal of
        { vall: Token.t
        , tyvars: Token.t SyntaxSeq.t
        , elems:
            { recc: Token.t option
            , pat: Pat.t,
            , eq: Token.t
            , exp: exp
            } Seq.t
        (** the `and` delimiters between bindings *)
        , delims: Token.t Seq.t
        }

    (** type tyvarseq tycon = ty [and tyvarseq tycon = ty ...] *)
    | DecType of
        { typee: Token.t
        , elems:
            { tyvars: Token.t SyntaxSeq.t
            , tycon: MaybeLong.t
            , eq: Token.t
            , ty: Ty.t
            } Seq.t
        (** the `and` delimiters between bindings *)
        , delims: Token.t Seq.t
        }

    (** datatype datbind *)
    | DecDatatype of
        { datatypee: Token.t
        , datbind: datbind
        }

    (** datatype tycon = datatype longtycon *)
    | DecReplicateDatatype of
        { left_datatypee: Token.t
        , left_id: Token.t
        , eq: Token.t
        , right_datatypee: Token.t
        , right_id: MaybeLong.t
        }

    (** abstype datbind with dec end *)
    | DecAbstype of
        { abstypee: Token.t
        , datbind: datbind
        , dec: dec
        }

    (** exception exbind *)
    | DecException of
        { exceptionn: Token.t
        , elems: exbind Seq.t
        (** the `and` delimiters between bindings *)
        , delims: Token.t Seq.t
        }

    (** local dec in dec end *)
    | DecLocal of
        { locall: Token.t
        , left_dec: dec
        , inn: Token.t
        , right_dec: dec
        , endd: Token.t
        }

    (** open longstrid [longstrid ...] *)
    | DecOpen of
        { openn: Token.t
        , elems: MaybeLong.t Seq.t
        }

    (** dec [[;] dec ...] *)
    | DecMultiple of
        { elems: dec Seq.t
        , delims: Token.t option Seq.t
        }

    (** infix [d] vid [vid ...] *)
    | DecInfix of
        { infixx: Token.t
        , precedence: Token.t option
        , elems: Token.t Seq.t
        }

    (** infix [d] vid [vid ...] *)
    | DecInfixr of
        { infixrr: Token.t
        , precedence: Token.t option
        , elems: Token.t Seq.t
        }

    (** nonfix vid [vid ...] *)
    | DecNonfix of
        { nonfixx: Token.t
        , elems: Token.t Seq.t
        }




    and exbind =
      ExnNew of
        { opp: Token.t option
        , id: Token.t
        , arg: {off: Token.t, ty: Ty.t} option
        }

    | ExnReplicate of
        { opp: Token.t option
        , left_id: Token.t
        , eq: Token.t
        , right_id: MaybeLong.t
        }

  end

  datatype ast =
    Foobar

  (** A fallback for unimplemented features, or for incrementality *)
  | Unknown of Token.t Seq.t

  type t = ast

end
