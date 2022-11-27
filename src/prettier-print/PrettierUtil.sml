(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettierUtil:
sig
  type tab = TabbedTokenDoc.tab
  type doc = TabbedTokenDoc.doc
  type style = TabbedTokenDoc.style

  val spaces: int -> doc
  val sequenceAt: tab -> Token.t -> Token.t Seq.t -> Token.t -> doc Seq.t -> doc
  val sequence: tab -> Token.t -> Token.t Seq.t -> Token.t -> doc Seq.t -> doc
  val showSyntaxSeq: tab -> 'a Ast.SyntaxSeq.t -> ('a -> doc) -> doc
  val showOption: ('a -> doc) -> 'a option -> doc


  type 'a shower = tab -> 'a -> doc
  val withNewChild: 'a shower -> 'a shower
  val withNewChildWithStyle: style -> 'a shower -> 'a shower

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


  fun spaces n =
    List.foldl op++ empty (List.tabulate (n, fn _ => space))


  fun sequenceAt tab openn delims close (xs: doc Seq.t) =
    if Seq.length xs = 0 then
      token openn ++ nospace ++ token close
    else
      let
        val top = token openn ++ (cond tab {inactive = nospace, active = space}) ++ Seq.nth xs 0
        fun f (delim, x) = nospace ++ at tab ++ token delim ++ x
      in
        Seq.iterate op++ top (Seq.map f (Seq.zip (delims, Seq.drop xs 1)))
        ++
        nospace ++ at tab ++ token close
      end


  fun sequence currentTab openn delims close (xs: doc Seq.t) =
    newTab currentTab (fn tab => at tab ++ sequenceAt tab openn delims close xs)


  fun showSyntaxSeq currentTab s f =
    case s of
      Ast.SyntaxSeq.Empty => empty
    | Ast.SyntaxSeq.One x => f x
    | Ast.SyntaxSeq.Many {left, elems, delims, right} =>
        sequence currentTab left delims right (Seq.map f elems)


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
          doc1 ++ at tab)
      ++ token inn
      ++ doc2
      ++ at tab ++ token endd
    end


  type 'a shower = tab -> 'a -> doc

  fun withNewChild shower tab x =
    newTab tab (fn inner => at inner ++ shower inner x)

  fun withNewChildWithStyle style shower tab x =
    newTabWithStyle tab (style, fn inner => at inner ++ shower inner x)


end
