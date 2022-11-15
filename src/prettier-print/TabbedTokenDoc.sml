(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure TabbedTokenDoc :>
sig
  type 'a doc
  type 'a t = 'a doc

  val empty: 'a doc
  val space: 'a doc
  val token: Token.t -> 'a doc
  val text: string -> 'a doc
  val concat: 'a doc * 'a doc -> 'a doc

  type 'a tab
  val newTab: ('a tab -> 'a doc) -> 'a doc
  val break: 'a tab -> 'a doc
  val breakspace: 'a tab -> 'a doc
  val spaceIfNotFlat: 'a tab -> 'a doc

  val toStringDoc: {tabWidth: int} -> TabbedStringDoc.tab doc -> TabbedStringDoc.t
end =
struct

  (* Just need a unique name *)
  type 'a tab = 'a option ref

  datatype 'a doc =
    Empty
  | Space
  | SpaceIfNotFlat of 'a tab
  | Concat of 'a doc * 'a doc
  | Token of Token.t
  | Text of string
  | Break of {keepSpace: bool, tab: 'a tab}
  | NewTab of {tab: 'a tab, doc: 'a doc}

  type 'a t = 'a doc

  val empty = Empty
  val space = Space
  val token = Token
  val text = Text
  val concat = Concat
  val spaceIfNotFlat = SpaceIfNotFlat

  fun break t = Break {keepSpace=false, tab=t}
  fun breakspace t = Break {keepSpace=true, tab=t}

  fun newTab (genDocUsingTab: 'a tab -> 'a doc) =
    let
      val t = ref (NONE: 'a option)
      val d = genDocUsingTab t
    in
      NewTab {tab=t, doc=d}
    end

  
  (* ====================================================================== *)

  structure TCS = TerminalColorString
  structure D = TabbedStringDoc

  fun tokenToStringDoc tabWidth tok =
    let
      val src = Token.getSource tok

      (** effective offset of the beginning of this token within its line,
        * counting tab-widths appropriately.
        *)
      val effectiveOffset =
        let
          val {col, line=lineNum} = Source.absoluteStart src
          val len = col-1
          val charsBeforeOnSameLine =
            Source.take (Source.wholeLine src lineNum) len
          fun loop effOff i =
            if i >= len then effOff
            else if #"\t" = Source.nth charsBeforeOnSameLine i then
              (* advance up to next tabstop *)
              loop (effOff + tabWidth - effOff mod tabWidth) (i+1)
            else
              loop (effOff+1) (i+1)
        in
          loop 0 0
        end

      fun strip line =
        let
          val (_, ln) =
            TCS.stripEffectiveWhitespace
              {tabWidth=tabWidth, removeAtMost=effectiveOffset}
              line
        in
          ln
        end

      val t = SyntaxHighlighter.highlightToken tok

      val pieces =
        Seq.map
          (fn (i, j) => D.text (strip (TCS.substring (t, i, j-i))))
          (Source.lineRanges src)
    in
      if Seq.length pieces = 1 then
        (false, D.text t)
      else
        ( true
        , D.newTab (fn tab =>
            Seq.iterate
              D.concat (Seq.nth pieces 0)
              (Seq.map (fn x => D.concat (D.break tab, x))
                (Seq.drop pieces 1)))
        )
    end


  (* ====================================================================== *)


  fun toStringDoc (args as {tabWidth}) doc =
    case doc of
      Empty => D.empty
    | Space => D.space
    | SpaceIfNotFlat tab => D.spaceIfNotFlat (Option.valOf (!tab))
    | Concat (d1, d2) => D.concat (toStringDoc args d1, toStringDoc args d2)
    | Text str => D.text (TerminalColorString.fromString str)
    | Token tok =>
        let
          val (shouldBeRigid, doc) = tokenToStringDoc tabWidth tok
        in
          (* TODO: rigidity (don't allow flattening) *)
          doc
        end
    | Break {keepSpace, tab} =>
        if keepSpace then
          D.breakspace (Option.valOf (!tab))
        else
          D.break (Option.valOf (!tab))
    | NewTab {tab, doc} =>
        D.newTab (fn tab' =>
          ( tab := SOME tab'
          ; toStringDoc args doc
          ))

end