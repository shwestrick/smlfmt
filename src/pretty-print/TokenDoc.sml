(** Copyright (c) 2021 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

(** This is essentially an intermediate representation for pretty-printing.
  * It's similar to a pretty doc, but the elements are tokens. The primary
  * transformation on this IR is to add comments.
  *)
structure TokenDoc :>
sig
  type t
  type doc = t

  val empty: doc
  val token: Token.t -> doc

  val indent: int -> doc -> doc

  val beside: doc * doc -> doc

  (** When an "above" is flattened by a group, it can either be replaced by a
    * a space, or it can be put exactly beside (with no extra space).
    *)
  val aboveOrSpace: doc * doc -> doc
  val aboveOrBeside: doc * doc -> doc

  val space: doc
  val softspace: doc
  val group: doc -> doc

  val insertComments: doc -> doc

  val toStringDoc: {tabWidth: int} -> doc -> StringDoc.t
end =
struct

  structure TCS = TerminalColorString

  (** for Space and Above, the boolean indicates whether or not to
    * keep space when undone by group.
    *)
  datatype doc =
    Empty
  | Space of bool
  | Indent of int * doc
  | Token of Token.t
  | Beside of doc * doc
  | Above of bool * doc * doc
  | Group of doc

  type t = doc

  val empty = Empty
  val token = Token
  val group = Group
  fun indent n d = Indent (n, d)

  fun beside (doc1, doc2) =
    case (doc1, doc2) of
      (Empty, _) => doc2
    | (_, Empty) => doc1
    | _ => Beside (doc1, doc2)

  fun above' withSpace (doc1, doc2) =
    case (doc1, doc2) of
      (Empty, _) => doc2
    | (_, Empty) => doc1
    | _ => Above (withSpace, doc1, doc2)

  val aboveOrSpace = above' true
  val aboveOrBeside = above' false

  val empty = Empty
  val space = Space true
  val softspace = Space false

  fun insertComments doc =
    let
      (** Does this doc most recently appear beside something,
        * or below something?
        *)
      datatype mode = BesideMode | AboveMode

      fun tokens mode toks =
        let
          fun combine (doc, tok) =
            case mode of
              BesideMode => beside (doc, beside (space, Token tok))
            | AboveMode => aboveOrSpace (doc, Token tok)
        in
          Seq.iterate combine (Token (Seq.nth toks 0)) (Seq.drop toks 1)
        end

      fun insertCommentsBeforeTok mode tok =
        tokens mode (Seq.append (Token.commentsBefore tok, Seq.$ tok))

      fun insertCommentsAfterTok mode tok =
        tokens mode (Seq.append (Seq.$ tok, Token.commentsAfter tok))

      fun insertAllBefore mode d =
        case d of
          Token tok =>
            insertCommentsBeforeTok mode tok
        | Beside (d1, d2) =>
            Beside (insertAllBefore mode d1, insertAllBefore BesideMode d2)
        | Above (b, d1, d2) =>
            Above (b, insertAllBefore mode d1, insertAllBefore AboveMode d2)
        | Group d =>
            Group (insertAllBefore mode d)
        | Indent (n, d) =>
            Indent (n, insertAllBefore mode d)
        | _ => d

      fun insertOnlyAfterLast mode d =
        case d of
          Token tok =>
            (true, insertCommentsAfterTok mode tok)
        | Group d =>
            let
              val (foundIt, d') = insertOnlyAfterLast mode d
            in
              (foundIt, Group d')
            end
        | Indent (n, d) =>
            let
              val (foundIt, d') = insertOnlyAfterLast mode d
            in
              (foundIt, Indent (n, d'))
            end
        | Beside (d1, d2) =>
            let
              val (foundIt, d2') = insertOnlyAfterLast BesideMode d2
            in
              if foundIt then
                (true, Beside (d1, d2'))
              else
                let
                  val (foundIt, d1') = insertOnlyAfterLast mode d1
                in
                  (foundIt, Beside (d1', d2'))
                end
            end
        | Above (b, d1, d2) =>
            let
              val (foundIt, d2') = insertOnlyAfterLast AboveMode d2
            in
              if foundIt then
                (true, Above (b, d1, d2'))
              else
                let
                  val (foundIt, d1') = insertOnlyAfterLast mode d1
                in
                  (foundIt, Above (b, d1', d2'))
                end
            end
        | _ => (false, d)

      val doc = insertAllBefore AboveMode doc
      val (_, doc) = insertOnlyAfterLast AboveMode doc
    in
      doc
    end


  (** returns whether or not allowed to be grouped *)
  fun tokenToStringDoc tabWidth tok =
    let
      val src = Token.getSource tok

      (** offset (0-indexed) of the beginning of this token within its line *)
      val {col, ...} = Source.absoluteStart src
      val offset = col-1

      fun strip line =
        let
          val (_, ln) =
            TCS.stripEffectiveWhitespace
              {tabWidth=tabWidth, removeAtMost=offset}
              line
        in
          ln
        end

      val t = SyntaxHighlighter.highlightToken tok

      val pieces =
        Seq.map
          (fn (i, j) => StringDoc.text (strip (TCS.substring (t, i, j-i))))
          (Source.lineRanges src)
    in
      if Seq.length pieces = 1 then
        (true, StringDoc.text t)
      else
        ( false
        , Seq.iterate StringDoc.aboveOrSpace StringDoc.empty pieces
        )
    end


  fun toStringDoc {tabWidth} d =
    let
      (** returns whether or not allowed to be grouped *)
      fun loop d =
        case d of
          Empty =>
            (true, StringDoc.empty)

        | Space true =>
            (true, StringDoc.space)

        | Space false =>
            (true, StringDoc.softspace)

        | Token t =>
            tokenToStringDoc tabWidth t

        | Indent (n, d) =>
            let
              val (groupable, d') = loop d
            in
              ( groupable
              , StringDoc.beside
                  ( List.foldl StringDoc.beside StringDoc.empty
                     (List.tabulate (n, fn _ => StringDoc.space))
                  , d'
                  )
              )
            end

        | Beside (d1, d2) =>
            let
              val (g1, d1') = loop d1
              val (g2, d2') = loop d2
            in
              (g1 andalso g2, StringDoc.beside (d1', d2'))
            end

        | Above (true, d1, d2) =>
            let
              val (g1, d1') = loop d1
              val (g2, d2') = loop d2
            in
              (g1 andalso g2, StringDoc.aboveOrSpace (d1', d2'))
            end

        | Above (false, d1, d2) =>
            let
              val (g1, d1') = loop d1
              val (g2, d2') = loop d2
            in
              (g1 andalso g2, StringDoc.aboveOrBeside (d1', d2'))
            end

        | Group d =>
            let
              val (groupable, d') = loop d
            in
              if groupable then
                (true, StringDoc.group d')
              else
                (false, d')
            end

      val (_, d') = loop d
    in
      d'
    end

end
