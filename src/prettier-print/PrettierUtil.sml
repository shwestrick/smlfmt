(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettierUtil:
sig
  type tab = TabbedTokenDoc.tab
  type doc = TabbedTokenDoc.doc
  type style = TabbedTokenDoc.style

  val indented: style
  val indentedAtLeastBy: int -> style

  val maybeAt: tab -> bool -> doc -> doc

  type 'a shower = tab -> 'a -> doc
  val withNewChild: 'a shower -> 'a shower
  val withNewChildWithStyle: style -> 'a shower -> 'a shower

  val spaces: int -> doc

  val showSequence:
    ('a -> bool)  (* elem starts with star? *)
    -> 'a shower
    -> { openn: Token.t
       , elems: 'a Seq.t
       , delims: Token.t Seq.t
       , close: Token.t
       }
       shower

  val showSyntaxSeq: 'a shower -> 'a Ast.SyntaxSeq.t shower
  val showTokenSyntaxSeq: Token.t Ast.SyntaxSeq.t shower

  val showOption: ('a -> doc) -> 'a option -> doc

  val showMaybeOpToken: Token.t option -> Token.t -> doc

  val showThingSimilarToLetInEnd:
    { lett: Token.t
    , isEmpty1: bool
    , doc1: doc
    , inn: Token.t
    , doc2: doc
    , endd: Token.t
    }
    shower

  val expStartsWithStar: Ast.Exp.exp -> bool
  val patStartsWithStar: Ast.Pat.t -> bool

end =
struct

  open TabbedTokenDoc
  infix 2 ++
  fun x ++ y = concat (x, y)

  val indented = Indented NONE

  fun indentedAtLeastBy x = Indented (SOME {minIndent=x})

  fun maybeAt tab b doc =
    if b then at tab doc else doc

  type 'a shower = tab -> 'a -> doc

  fun withNewChild shower tab x =
    newTab tab (fn inner => at inner (shower inner x))

  fun withNewChildWithStyle style shower tab x =
    newTabWithStyle tab (style, fn inner => at inner (shower inner x))


  fun spaces n =
    List.foldl op++ empty (List.tabulate (n, fn _ => space))


  fun showSequence elemStartsWithStar (elemShower: 'a shower) tab {openn, elems: 'a Seq.t, delims, close} =
    if Seq.length elems = 0 then
      token openn ++ nospace ++ token close
    else if Seq.length elems = 1 then
      token openn ++ nospace
      ++ elemShower tab (Seq.nth elems 0)
      ++ nospace ++ token close
    else
      newTab tab (fn inner =>
        let
          val topspacer =
            if
              Token.isOpenParen openn
              andalso
              elemStartsWithStar (Seq.nth elems 0)
            then
              empty
            else
              cond inner {inactive = nospace, active = space}

          val top =
            token openn ++ topspacer ++
            elemShower inner (Seq.nth elems 0)

          fun f (delim, x) =
            nospace ++ at inner (token delim ++ elemShower inner x)
        in
          at inner
            (Seq.iterate op++ top
              (Seq.map f (Seq.zip (delims, Seq.drop elems 1)))
            ++
            nospace ++ at inner (token close))
        end)


  fun showSyntaxSeq elemShower tab s =
    case s of
      Ast.SyntaxSeq.Empty => empty
    | Ast.SyntaxSeq.One x => elemShower tab x
    | Ast.SyntaxSeq.Many {left, elems, delims, right} =>
        showSequence (fn _ => false) elemShower tab
          { openn = left
          , elems = elems
          , delims = delims
          , close = right
          }


  fun showTokenSyntaxSeq tab s =
    showSyntaxSeq (fn _ => fn tok => token tok) tab s


  fun showOption f x =
    case x of
      NONE => empty
    | SOME xx => f xx


  fun showThingSimilarToLetInEnd tab {lett, isEmpty1, doc1, inn, doc2, endd} =
    let in
      token lett ++
      (if isEmpty1 then
        token inn
      else
        doc1 ++ at tab (token inn))
      ++ doc2
      ++ at tab (token endd)
    end


  fun showMaybeOpToken oppo tok =
    case oppo of
      NONE => token tok
    | SOME opp =>
        let
          val wantsToTouch =
            Token.isSymbolicIdentifier tok
            andalso
            not (Token.isLongIdentifier tok)
        in
          if wantsToTouch then
            token opp ++ nospace ++ token tok
          else
            token opp ++ token tok
        end


  fun expStartsWithStar exp =
    let
      open Ast.Exp
    in
      case exp of
        Ident {opp=NONE, id} =>
          Token.isStar (MaybeLongToken.getToken id)
      | App {left, ...} =>
          expStartsWithStar left
      | Infix {left, ...} =>
          expStartsWithStar left
      | Typed {exp, ...} =>
          expStartsWithStar exp
      | Andalso {left, ...} =>
          expStartsWithStar left
      | Orelse {left, ...} =>
          expStartsWithStar left
      | Handle {exp, ...} =>
          expStartsWithStar exp
      | _ => false
    end


  fun patStartsWithStar pat =
    let
      open Ast.Pat
    in
      case pat of
        Ident {opp=NONE, id} =>
          Token.isStar (MaybeLongToken.getToken id)
      | Infix {left, ...} =>
          patStartsWithStar left
      | Typed {pat, ...} =>
          patStartsWithStar pat
      | Con {opp=NONE, id, ...} =>
          Token.isStar (MaybeLongToken.getToken id)
      | Layered {opp=NONE, id, ...} =>
          Token.isStar id
      | _ => false
    end


end
