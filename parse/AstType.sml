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
structure AstType =
struct

  (** A maybe-long thing can be prefaced by structure identifiers, like
    * `Hello.World.thing` but also just `thing` (which has no qualifiers)
    *)
  structure MaybeLong :>
  sig
    type t
    val make: Token.t -> t
    val getToken: t -> Token.t
  end =
  struct
    type t = Token.t

    fun make (tok: Token.t) : t =
      if Token.isMaybeLongIdentifier tok then
        tok
      else
        raise Fail ("Ast.MaybeLong.make: given non-identifier ("
                    ^ Token.classToString (Token.getClass tok) ^ ")")

    fun getToken tok = tok
  end


  (** Used for syntactic classes that look like:
    *
    *   Xseq  ::=               -- empty
    *           | X             -- singleton
    *           | (X, ..., X)   -- sequence enclosed by parens
    *
    * e.g. tyseq, tyvarseq, etc.
    *)
  structure SyntaxSeq =
  struct
    datatype 'a t =
      Empty
    | One of 'a
    | Many of
        { left: Token.t         (** open paren *)
        , elems: 'a Seq.t       (** elements *)
        , delims: Token.t Seq.t (** commas between elements *)
        , right: Token.t        (** close paren *)
        }
  end


  (** ======================================================================
    * Types.
    *)
  structure Ty =
  struct
    datatype ty =
      Var of Token.t

    (** { lab : ty, ..., lab : ty } *)
    | Record of
        { left: Token.t
        , elems: {lab: Token.t, colon: Token.t, ty: ty} Seq.t
        , delims: Token.t Seq.t
        , right: Token.t
        }

    (** ty * ... * ty *)
    | Tuple of
        { elems: ty Seq.t
        , delims: Token.t Seq.t
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
  structure Pat =
  struct

    datatype patrow =
      DotDotDot of Token.t  (** can only appear at end of record pattern *)

    | LabEqPat of
        { lab: Token.t
        , eq: Token.t
        , pat: pat
        }

    | LabAsPat of
        { id: Token.t
        , ty: {colon: Token.t, ty: Ty.t} option
        , aspat: {ass: Token.t, pat: pat} option
        }


    and pat =
      Wild of Token.t

    | Const of Token.t

    | Unit of
        { left: Token.t
        , right: Token.t
        }

    (** [op] longvid *)
    | Ident of
        { opp: Token.t option
        , id: MaybeLong.t
        }

    (** [ pat, ..., pat ] *)
    | List of
        { left: Token.t
        , elems: pat Seq.t
        , delims: Token.t Seq.t
        , right: Token.t
        }

    (** ( pat, ..., pat ) *)
    | Tuple of
        { left: Token.t
        , elems: pat Seq.t
        , delims: Token.t Seq.t  (** Gotta remember the commas too! *)
        , right: Token.t
        }

    (** { lab = pat, ..., lab = pat } *)
    | Record of
        { left: Token.t
        , elems: patrow Seq.t
        , delims: Token.t Seq.t
        , right: Token.t
        }

    (** ( pat ) *)
    | Parens of
        { left: Token.t
        , pat: pat
        , right: Token.t
        }

    (** [op] longvid atpat *)
    | Con of
        { opp: Token.t option
        , id: MaybeLong.t
        , atpat: pat
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
  structure Exp =
  struct

    (** tyvarseq tycon = ty [and tyvarseq tycon = ty ...] *)
    type typbind =
      { elems:
          { tyvars: Token.t SyntaxSeq.t
          , tycon: Token.t
          , eq: Token.t
          , ty: Ty.t
          } Seq.t
      (** the `and` delimiters between bindings *)
      , delims: Token.t Seq.t
      }


    (** tyvarseq tycon = conbind [and tyvarseq tycon = conbind ...]
      * where conbind ::= [op] vid [of ty] [| [op] vid [of ty] ...]
      *)
    type datbind =
      { elems:
        { tyvars: Token.t SyntaxSeq.t
        , tycon: Token.t
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


    type 'exp fvalbind =
      { elems:
          { elems:
              { opp: Token.t option
              , id: Token.t
              , args: Pat.t Seq.t  (** NOTE: must be atomic patterns *)
              , ty: {colon: Token.t, ty: Ty.t} option
              , eq: Token.t
              , exp: 'exp
              } Seq.t

          (** the `|` delimiters *)
          , delims: Token.t Seq.t
          } Seq.t

      (** the `and` delimiters *)
      , delims: Token.t Seq.t
      }




    datatype exp =
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

    (** # label *)
    | Select of
        { hash: Token.t
        , label: Token.t
        }

    (** () *)
    | Unit of
        { left: Token.t
        , right: Token.t
        }

    (** (exp, ..., exp) *)
    | Tuple of
        { left: Token.t         (** open paren *)
        , elems: exp Seq.t      (** elements *)
        , delims: Token.t Seq.t (** commas between elements *)
        , right: Token.t        (** close paren *)
        }

    (** [exp, ..., exp] *)
    | List of
        { left: Token.t
        , elems: exp Seq.t
        , delims: Token.t Seq.t
        , right: Token.t
        }

    (** (exp; ...; exp) *)
    | Sequence of
        { left: Token.t
        , elems: exp Seq.t
        , delims: Token.t Seq.t
        , right: Token.t
        }

    (** let dec in exp [; exp ...] end *)
    | LetInEnd of
        { lett: Token.t
        , dec: dec
        , inn: Token.t
        , exps: exp Seq.t
        , delims: Token.t Seq.t
        , endd: Token.t
        }

    (** ( exp ) *)
    | Parens of
        { left: Token.t
        , exp: exp
        , right: Token.t
        }

    (** exp exp
      * (Note: needs to be restricted by AtExp < AppExp < InfExp < Exp)
      *)
    | App of
        { left: exp     (** the function expression *)
        , right: exp    (** the argument expression *)
        }

    (** exp vid exp
      * (Note: needs to be restricted by AtExp < AppExp < InfExp < Exp)
      *)
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

    (** exp andalso exp *)
    | Andalso of
        { left: exp
        , andalsoo: Token.t
        , right: exp
        }

    (** exp orelse exp *)
    | Orelse of
        { left: exp
        , orelsee: Token.t
        , right: exp
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

    (** if exp then exp else exp *)
    | IfThenElse of
        { iff: Token.t
        , exp1: exp
        , thenn: Token.t
        , exp2: exp
        , elsee: Token.t
        , exp3: exp
        }

    (** while exp do exp *)
    | While of
        { whilee: Token.t
        , exp1: exp
        , doo: Token.t
        , exp2: exp
        }

    (** case exp of pat => exp [| pat => exp ...] *)
    | Case of
        { casee: Token.t
        , exp: exp
        , off: Token.t
        , elems: {pat: Pat.t, arrow: Token.t, exp: exp} Seq.t
        , delims: Token.t Seq.t   (** the bars between match rules *)
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
            , pat: Pat.t
            , eq: Token.t
            , exp: exp
            } Seq.t
        (** the `and` delimiters between bindings *)
        , delims: Token.t Seq.t
        }

    (** fun tyvarseq [op]vid atpat ... atpat [: ty] = exp [| ...] *)
    | DecFun of
        { funn: Token.t
        , tyvars: Token.t SyntaxSeq.t
        , fvalbind: exp fvalbind
        }

    (** type tyvarseq tycon = ty [and tyvarseq tycon = ty ...] *)
    | DecType of
        { typee: Token.t
        , typbind: typbind
        }

    (** datatype datbind [withtype typbind] *)
    | DecDatatype of
        { datatypee: Token.t
        , datbind: datbind
        , withtypee: {withtypee: Token.t, typbind: typbind} option
        }

    (** datatype tycon = datatype longtycon *)
    | DecReplicateDatatype of
        { left_datatypee: Token.t
        , left_id: Token.t
        , eq: Token.t
        , right_datatypee: Token.t
        , right_id: MaybeLong.t
       }

    (** abstype datbind [withtype typbind] with dec end *)
    | DecAbstype of
        { abstypee: Token.t
        , datbind: datbind
        , withtypee: {withtypee: Token.t, typbind: typbind} option
        , dec: dec
        , endd: Token.t
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


  (** =======================================================================
    * Module Signatures
    *)
  structure Sig =
  struct

    datatype spec =
      EmptySpec

    (** val vid : ty [and vid : ty and ...] *)
    | Val of
        { vall: Token.t
        , elems:
            { vid: Token.t
            , colon: Token.t
            , ty: Ty.t
            } Seq.t
        (** 'and' delimiters between mutually recursive values *)
        , delims: Token.t Seq.t
        }

    (** type tyvarseq tycon [and tyvarseq tycon ...] *)
    | Type of
        { typee: Token.t
        , elems:
            { tyvars: Token.t SyntaxSeq.t
            , tycon: Token.t
            } Seq.t
        (** 'and' delimiters between mutually recursive types *)
        , delims: Token.t Seq.t
        }

    (** eqtype tyvarseq tycon [and tyvarseq tycon ...] *)
    | Eqtype of
        { eqtypee: Token.t
        , elems:
            { tyvars: Token.t SyntaxSeq.t
            , tycon: Token.t
            } Seq.t
        (** 'and' delimiters between mutually recursive types *)
        , delims: Token.t Seq.t
        }

    (** datatype tyvarseq tycon = condesc [and tyvarseq tycon ...] *)
    | Datatype of
        { datatypee: Token.t
        , elems:
            { tyvars: Token.t SyntaxSeq.t
            , tycon: Token.t
            , eq: Token.t
            , elems:
                { vid: Token.t
                , arg: {off: Token.t, ty: Ty.t} option
                } Seq.t
            (** '|' delimiters between clauses *)
            , delims: Token.t Seq.t
            } Seq.t
        (** 'and' delimiters between mutually recursive datatypes *)
        , delims: Token.t Seq.t
        }

    (** datatype tycon = datatype longtycon *)
    | ReplicateDatatype of
        { left_datatypee: Token.t
        , left_id: Token.t
        , eq: Token.t
        , right_datatypee: Token.t
        , right_id: MaybeLong.t
        }

    (** exception vid [of ty] [and vid [of ty] ...] *)
    | Exception of
        { exceptionn: Token.t
        , elems:
            { vid: Token.t
            , arg: {off: Token.t, ty: Ty.t} option
            } Seq.t
        (** 'and' delimiters between exceptions *)
        , delims: Token.t Seq.t
        }

    (** structure strid : sigexp [and strid : sigep ...] *)
    | Structure of
        { structuree: Token.t
        , elems:
            { id: Token.t
            , colon: Token.t
            , sigexp: sigexp
            } Seq.t
        , delims: Token.t Seq.t
        }

    (** include sigexp *)
    | Include of
        { includee: Token.t
        , sigexp: sigexp
        }

    (** spec sharing type longtycon1 = ... = longtyconn *)
    | Sharing of
        { spec: spec
        , sharingg: Token.t
        , typee: Token.t
        , elems: MaybeLong.t Seq.t
        (** the '=' delimiters between longtycons *)
        , delims: Token.t Seq.t
        }

    (** spec [[;] spec ...] *)
    | Multiple of
        { elems: spec Seq.t
        , delims: Token.t option Seq.t
        }



    and sigexp =
      Ident of Token.t

    (** sig spec end *)
    | Spec of
        { sigg: Token.t
        , spec: spec
        , endd: Token.t
        }

    (** sigexp where type tyvarseq tycon = ty [where type ...]
      *
      * NOTE: permitted to do 'and type' instead of 'where type' if it is
      * not the first in the sequence, for example
      * sig ... end where type ... and type ...
      *)
    | WhereType of
        { sigexp: sigexp
        , elems:
            { wheree: Token.t
            , typee: Token.t
            , tyvars: Token.t SyntaxSeq.t
            , tycon: MaybeLong.t
            , eq: Token.t
            , ty: Ty.t
            } Seq.t
        }

    (** TODO finish 'sigexp' type *)



    and sigdec =

    (** signature sigid = sigexp [and ...] *)
      Signature of
        { signaturee: Token.t
        , elems:
            { ident: Token.t
            , eq: Token.t
            , sigexp: sigexp
            } Seq.t

        (** 'and' between elems *)
        , delims: Token.t Seq.t
        }

  end


  (** =======================================================================
    * Module Structures
    *)
  structure Str =
  struct

    (** TODO: finish *)
    datatype strexp = SE

    (** TODO: finish *)
    datatype strdec =
      Dec of Exp.dec

  end


  (** =======================================================================
    * Module Functors
    *)
  structure Fun =
  struct

    (** TODO: finish *)
    datatype funexp = FE
    datatype fundec = FD

  end


  (** =======================================================================
    * Top-level. Programs are sequences of top-level declarations.
    * Something a little cumbersome: strdec permits standard declarations too,
    * of values, types, etc. IMO this doesn't align with anyone's intuitive
    * understanding of the language, but (I suppose) it is somewhat convenient
    * for avoid unnecessary ambiguity in the grammar.
    *)
  datatype topdec =
    SigDec of Sig.sigdec
  | StrDec of Str.strdec
  | FunDec of Fun.fundec

  datatype ast =
    Ast of topdec Seq.t

  type t = ast

end
