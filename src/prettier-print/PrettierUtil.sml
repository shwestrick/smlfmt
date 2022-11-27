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
  val showThingSimilarToLetInEnd: tab
                               -> ( Token.t
                                  * (bool * (unit -> doc))
                                  * Token.t
                                  * (unit -> doc)
                                  * Token.t
                                  )
                               -> doc

  type 'a shower = tab -> 'a -> doc
  val withNewChild: 'a shower -> 'a shower
  val withNewChildWithStyle: style -> 'a shower -> 'a shower
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


  fun showThingSimilarToLetInEnd tab (start, (isEmptyA, showA), mid, showB, stop) =
    let in
      token start ++
      (if isEmptyA then
          empty
        else
          showA () ++ at tab)
      ++ token mid
      ++ showB ()
      ++ at tab ++ token stop
    end


  type 'a shower = tab -> 'a -> doc

  fun withNewChild shower tab x =
    newTab tab (fn inner => at inner ++ shower inner x)

  fun withNewChildWithStyle style shower tab x =
    newTabWithStyle tab (style, fn inner => at inner ++ shower inner x)


end
