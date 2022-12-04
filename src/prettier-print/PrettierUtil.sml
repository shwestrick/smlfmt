(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettierUtil:
sig
  type tab = TabbedTokenDoc.tab
  type doc = TabbedTokenDoc.doc
  type style = TabbedTokenDoc.style

  type 'a shower = tab -> 'a -> doc
  val withNewChild: 'a shower -> 'a shower
  val withNewChildWithStyle: style -> 'a shower -> 'a shower

  val spaces: int -> doc

  val showSequence:
    { openn: Token.t
    , elems: doc Seq.t
    , delims: Token.t Seq.t
    , close: Token.t
    }
    shower

  val showSyntaxSeq: ('a -> doc) -> 'a Ast.SyntaxSeq.t shower

  val showOption: ('a -> doc) -> 'a option -> doc

  val showThingSimilarToLetInEnd:
    { lett: Token.t
    , isEmpty1: bool
    , doc1: doc
    , inn: Token.t
    , doc2: doc
    , endd: Token.t
    }
    shower

end =
struct

  open TabbedTokenDoc
  infix 2 ++
  fun x ++ y = concat (x, y)


  type 'a shower = tab -> 'a -> doc

  fun withNewChild shower tab x =
    newTab tab (fn inner => goto inner ++ shower inner x)

  fun withNewChildWithStyle style shower tab x =
    newTabWithStyle tab (style, fn inner => goto inner ++ shower inner x)


  fun spaces n =
    List.foldl op++ empty (List.tabulate (n, fn _ => space))


  fun showSequence tab {openn, elems, delims, close} =
    if Seq.length elems = 0 then
      token openn ++ nospace ++ token close
    else
      let
        val top = token openn ++ (cond tab {inactive = nospace, active = space}) ++ Seq.nth elems 0
        fun f (delim, x) = nospace ++ goto tab ++ token delim ++ x
      in
        Seq.iterate op++ top (Seq.map f (Seq.zip (delims, Seq.drop elems 1)))
        ++
        nospace ++ goto tab ++ token close
      end


  fun showSyntaxSeq f tab s =
    case s of
      Ast.SyntaxSeq.Empty => empty
    | Ast.SyntaxSeq.One x => f x
    | Ast.SyntaxSeq.Many {left, elems, delims, right} =>
        withNewChild showSequence tab
          { openn = left
          , elems = Seq.map f elems
          , delims = delims
          , close = right
          }


  fun showOption f x =
    case x of
      NONE => empty
    | SOME xx => f xx


  fun showThingSimilarToLetInEnd tab {lett, isEmpty1, doc1, inn, doc2, endd} =
    let in
      token lett ++
      (if isEmpty1 then
          empty
        else
          doc1 ++ goto tab)
      ++ token inn
      ++ doc2
      ++ goto tab ++ token endd
    end


end
