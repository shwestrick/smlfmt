(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettierUtil =
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


  fun showThingSimilarToLetInEnd outerTab
        (start, (isEmptyA, showA), mid, showB, stop)
    =
    let in
      token start ++
      (if isEmptyA then
          empty
        else
          showA () ++ at outerTab)
      ++ token mid
      ++ showB ()
      (* ++ newTab outerTab (fn innerTab => Seq.iterate op++ empty (withDelims innerTab)) *)
      ++ at outerTab ++ token stop
    end

end
