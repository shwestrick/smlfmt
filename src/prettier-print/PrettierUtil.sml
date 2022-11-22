(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettierUtil =
struct

  open TabbedTokenDoc
  infix 2 ++ (*$$ //*)
  (* infix 1 \\ *)
  fun x ++ y = concat (x, y)
  (* fun x $$ y = aboveOrSpace (x, y)
  fun x // y = aboveOrBeside (x, y)
  fun x \\ y = group (x $$ indent y) *)


  fun seqWithSpaces elems f =
    if Seq.length elems = 0 then empty else
    Seq.iterate
      (fn (prev, tok) => prev ++ f tok)
      (f (Seq.nth elems 0))
      (Seq.drop elems 1)


  fun spaces n =
    List.foldl op++ empty (List.tabulate (n, fn _ => space))


  fun sequenceAt tab openn delims close (xs: doc Seq.t) =
    if Seq.length xs = 0 then
      token openn ++ nospace ++ token close
    else
      let
        val top = token openn ++ (cond tab {flat = nospace, notflat = space}) ++ Seq.nth xs 0
        fun f (delim, x) = nospace ++ break tab ++ token delim ++ x
      in
        Seq.iterate op++ top (Seq.map f (Seq.zip (delims, Seq.drop xs 1)))
        ++
        nospace ++ break tab ++ token close
      end

  
  fun sequence currentTab openn delims close (xs: doc Seq.t) =
    newChildTab currentTab (fn tab => break tab ++ sequenceAt tab openn delims close xs)


  fun separateWithSpaces (items: doc option list) : doc =
    let
      val items: doc list = List.mapPartial (fn x => x) items
    in
      case items of
        [] => empty
      | first :: rest =>
          List.foldl (fn (next, prev) => prev ++ next) first rest
    end


  fun maybeShowSyntaxSeq currentTab s f =
    case s of
      Ast.SyntaxSeq.Empty => NONE
    | Ast.SyntaxSeq.One x => SOME (f x)
    | Ast.SyntaxSeq.Many {left, elems, delims, right} =>
        SOME (sequence currentTab left delims right (Seq.map f elems))

end
