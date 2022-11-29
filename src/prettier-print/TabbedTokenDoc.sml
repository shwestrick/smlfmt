(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure TabbedTokenDoc :>
sig
  type doc
  type t = doc

  val empty: doc
  val space: doc
  val nospace: doc
  val token: Token.t -> doc
  val text: string -> doc
  val concat: doc * doc -> doc

  datatype style = Inplace | Indented

  type tab
  val root: tab
  val newTabWithStyle: tab -> style * (tab -> doc) -> doc
  val newTab: tab -> (tab -> doc) -> doc
  val at: tab -> doc
  val cond: tab -> {inactive: doc, active: doc} -> doc

  val toStringDoc: {tabWidth: int, debug: bool} -> doc -> TabbedStringDoc.t
end =
struct

  structure D = TabbedStringDoc

  datatype style = Inplace | Indented

  (* Just need a unique name *)
  datatype tab =
    Tab of {id: int, style: style, parent: tab}
  | Root

  val tabCounter = ref 0

  fun mkTab parent style =
    let
      val c = !tabCounter
    in
      tabCounter := c+1;
      Tab {id = c, style = style, parent = parent}
    end


  val root = Root


  fun parent t =
    case t of
      Root => NONE
    | Tab {parent, ...} => SOME parent


  fun style t =
    case t of
      Root => Inplace
    | Tab {style=s, ...} => s


  fun tabToString t =
    case t of
      Tab {id=c, ...} => "[" ^ Int.toString c ^ "]"
    | Root => "[root]"


  structure TabKey =
  struct
    type t = tab
    fun compare (t1: tab, t2: tab) : order =
      case (t1, t2) of
        (Root, Root) => EQUAL
      | (Tab t1, Tab t2) => Int.compare (#id t1, #id t2)
      | (Tab _, Root) => GREATER
      | (Root, Tab _) => LESS
  end

  structure TabDict = Dict(TabKey)
  structure TabSet = Set(TabKey)

  datatype doc =
    Empty
  | Space
  | NoSpace
  | Concat of doc * doc
  | Token of Token.t
  | Text of string
  | Break of tab
  | NewTab of {tab: tab, doc: doc}
  | Cond of {tab: tab, inactive: doc, active: doc}

  type t = doc

  val empty = Empty
  val nospace = NoSpace
  val space = Space
  val token = Token
  val text = Text
  val at = Break

  fun concat (d1, d2) =
    case (d1, d2) of
      (Empty, _) => d2
    | (_, Empty) => d1
    | _ => Concat (d1, d2)

  fun cond tab {inactive, active} = Cond {tab=tab, inactive=inactive, active=active}

  fun toString doc =
    case doc of
      Empty => ""
    | Space => "_"
    | NoSpace => "NoSpace"
    | Concat (d1, d2) => toString d1 ^ " ++ " ^ toString d2
    | Token t => "Token('" ^ Token.toString t ^ "')"
    | Text t => "Text('" ^ t ^ "')"
    | Break t => "Break(" ^ tabToString t ^ ")"
    | NewTab {tab=t, doc=d, ...} => "NewTab(" ^ tabToString t ^ ", " ^ toString d ^ ")"
    | Cond {tab=t, inactive=df, active=dnf} =>
        "Cond(" ^ tabToString t ^ ", " ^ toString df ^ ", " ^ toString dnf ^ ")"


  fun newTabWithStyle parent (style, genDocUsingTab: tab -> doc) =
    let
      val t = mkTab parent style
      val d = genDocUsingTab t
    in
      NewTab {tab=t, doc=d}
    end

  fun newTab parent f = newTabWithStyle parent (Inplace, f)

  (* ====================================================================== *)
  (* ====================================================================== *)
  (* ====================================================================== *)

  datatype anndoc =
    AnnEmpty
  | AnnNewline
  | AnnNoSpace
  | AnnSpace
  | AnnToken of {at: tab option, tok: Token.t}
  | AnnText of {at: tab option, txt: string}
  | AnnConcat of anndoc * anndoc
  | AnnBreak of {mightBeFirst: bool, tab: tab}
  | AnnNewTab of {tab: tab, doc: anndoc}
  | AnnCond of {tab: tab, inactive: anndoc, active: anndoc}


  fun annToString doc =
    case doc of
      AnnEmpty => ""
    | AnnNewline => "Newline"
    | AnnSpace => "_"
    | AnnNoSpace => "NoSpace"
    | AnnConcat (d1, d2) => annToString d1 ^ " ++ " ^ annToString d2
    | AnnToken {tok=t, ...} => "Token('" ^ Token.toString t ^ "')"
    | AnnText {txt=t, ...} => "Text('" ^ t ^ "')"
    | AnnBreak {mightBeFirst, tab} =>
        "Break" ^ (if mightBeFirst then "!!" else "") ^ "(" ^ tabToString tab ^ ")"
    | AnnNewTab {tab=t, doc=d, ...} => "NewTab(" ^ tabToString t ^ ", " ^ annToString d ^ ")"
    | AnnCond {tab=t, inactive=df, active=dnf} =>
        "Cond(" ^ tabToString t ^ ", " ^ annToString df ^ ", " ^ annToString dnf ^ ")"


  fun annotate doc =
    let
      (* if tab in broken, then tab has definitely had at least one break *)
      fun loop currtab (doc, broken) =
        case doc of
          Empty => (AnnEmpty, broken)
        | Space => (AnnSpace, broken)
        | NoSpace => (AnnNoSpace, broken)
        | Token t => (AnnToken {at=NONE, tok=t}, broken)
        | Text t => (AnnText {at=NONE, txt=t}, broken)
        | Break tab =>
            let
              val (mightBeFirst, broken) =
                if TabSet.contains broken tab then
                  (false, broken)
                else
                  (true, TabSet.insert broken tab)
            in
              ( AnnBreak
                  { mightBeFirst = mightBeFirst
                  , tab = tab
                  }
              , broken
              )
            end
        | Concat (d1, d2) =>
            let
              val (d1, broken) = loop currtab (d1, broken)
              val (d2, broken) = loop currtab (d2, broken)
            in
              (AnnConcat (d1, d2), broken)
            end
        | NewTab {tab, doc} =>
            let
              val (doc, broken) = loop tab (doc, broken)
            in
              ( AnnNewTab {tab = tab, doc = doc}
              , broken
              )
            end
        | Cond {tab, inactive, active} =>
            let
              val (inactive, broken1) = loop currtab (inactive, broken)
              val (active, broken2) = loop currtab (active, broken)
            in
              ( AnnCond {tab=tab, inactive=inactive, active=active}
              , TabSet.intersect (broken1, broken2)
              )
            end

      val (anndoc, _) = loop Root (doc, TabSet.empty)
    in
      anndoc
    end


  fun ensureSpaces debug (doc: anndoc) =
    let
      fun dbgprintln s =
        if not debug then ()
        else print (s ^ "\n")

      datatype edge = Spacey | MaybeNotSpacey
      datatype tab_constraint = Active | Inactive
      type context = tab_constraint TabDict.t

      fun edgeOptToString e =
        case e of
          NONE => "NONE"
        | SOME Spacey => "Spacey"
        | SOME MaybeNotSpacey => "MaybeNotSpacey"

      fun markInactive ctx tab =
        TabDict.insert ctx (tab, Inactive)

      fun markActive ctx tab =
        case tab of
          Root => ctx
        | Tab {parent, ...} =>
            markActive (TabDict.insert ctx (tab, Active)) parent

      fun edge {left: bool} ctx doc =
        let
          fun loop ctx doc =
            case doc of
              AnnEmpty => NONE
            | AnnNewline => SOME Spacey
            | AnnSpace => SOME Spacey
            | AnnNoSpace => SOME Spacey (* pretends to be a space, but then actually is elided *)
            | AnnToken _ => SOME MaybeNotSpacey
            | AnnText _ => SOME MaybeNotSpacey
            | AnnBreak {mightBeFirst, tab} =>
                (case TabDict.find ctx tab of
                  SOME Active =>
                    if mightBeFirst then
                      NONE
                    else
                      SOME Spacey
                | _ => NONE)
            | AnnConcat (d1, d2) =>
                if left then
                  (case loop ctx d1 of
                    SOME xs => SOME xs
                  | NONE => loop ctx d2)
                else
                  (case loop ctx d2 of
                    SOME xs => SOME xs
                  | NONE => loop ctx d1)
            | AnnNewTab {doc=d, ...} => loop ctx d
            | AnnCond {tab, inactive, active} =>
                let
                  val result =
                    case TabDict.find ctx tab of
                      SOME Active =>
                        let
                          val result = loop ctx active
                        in
                          dbgprintln (annToString doc ^ ": ACTIVE: " ^ edgeOptToString result);
                          result
                        end
                    | SOME Inactive =>
                        let
                          val result = loop ctx inactive
                        in
                          dbgprintln (annToString doc ^ ": INACTIVE: " ^ edgeOptToString result);
                          result
                        end
                    | NONE =>
                        let
                          val r1 = loop (markInactive ctx tab) inactive
                          val r2 = loop (markActive ctx tab) active
                          val result =
                            case (r1, r2) of
                              (SOME MaybeNotSpacey, _) => SOME MaybeNotSpacey
                            | (_, SOME MaybeNotSpacey) => SOME MaybeNotSpacey
                            | (SOME Spacey, SOME Spacey) => SOME Spacey
                            | (NONE, _) => NONE
                            | (_, NONE) => NONE
                        in
                          dbgprintln (annToString doc ^ ": UNSURE: ACTIVE? " ^ edgeOptToString r2 ^ "; INACTIVE? " ^ edgeOptToString r1 ^ "; OVERALL: " ^ edgeOptToString result);
                          result
                        end
                in
                  result
                end
        in
          loop ctx doc
        end

      fun leftEdge ctx doc = edge {left=true} ctx doc
      fun rightEdge ctx doc = edge {left=false} ctx doc

      fun checkInsertSpace (needSpaceBefore, needSpaceAfter) doc =
        let
          val origDoc = doc

          val doc =
            if not needSpaceBefore then doc
            else
              ( dbgprintln ("need space before " ^ annToString origDoc)
              ; AnnConcat (AnnSpace, doc)
              )

          val doc =
            if not needSpaceAfter then doc
            else
              ( dbgprintln ("need space after " ^ annToString origDoc)
              ; AnnConcat (doc, AnnSpace)
              )
        in
          doc
        end

      fun loop ctx (needSpace as (needSpaceBefore, needSpaceAfter)) doc : anndoc =
        case doc of
          AnnSpace => doc
        | AnnNoSpace => doc
        | AnnNewline => doc
        | AnnToken t => checkInsertSpace needSpace doc
        | AnnText t => checkInsertSpace needSpace doc
        | AnnEmpty =>
            if needSpaceBefore orelse needSpaceAfter then
              AnnSpace
            else
              AnnEmpty
        | AnnBreak {mightBeFirst, tab} =>
            if not (needSpaceBefore orelse needSpaceAfter) then
              doc
            else
              (case TabDict.find ctx tab of
                SOME Inactive =>
                  ( dbgprintln ("need space at INACTIVE " ^ annToString doc)
                  ; AnnSpace
                  )
              | _ =>
                  ( dbgprintln ("need space at UNKNOWN " ^ annToString doc)
                  ; AnnConcat (AnnSpace, doc)
                  ))
        | AnnNewTab {tab, doc} =>
            AnnNewTab {tab = tab, doc = loop ctx needSpace doc}
        | AnnCond {tab, inactive, active} =>
            let
              val inactive = loop (markInactive ctx tab) needSpace inactive
              val active = loop (markActive ctx tab) needSpace active
            in
              AnnCond {tab = tab, inactive = inactive, active = active}
            end
        | AnnConcat (d1, d2) =>
            let
              val d1 = loop ctx (needSpaceBefore, false) d1

              val needSpaceBefore2 =
                case rightEdge ctx d1 of
                  SOME MaybeNotSpacey => true
                | _ => false

              val d2 = loop ctx (needSpaceBefore2, needSpaceAfter) d2
            in
              AnnConcat (d1, d2)
            end

      val result = loop TabDict.empty (false, false) doc
      (* val _ = dbgprintln ("ensureSpaces OUTPUT: " ^ toString result) *)
    in
      result
    end

  (* ====================================================================== *)
  (* ====================================================================== *)
  (* ====================================================================== *)

  structure TokenKey =
  struct
    type t = Token.t
    fun compare (t1, t2) =
      let
        val s1 = Token.getSource t1
        val s2 = Token.getSource t2
      in
        case Int.compare (Source.absoluteStartOffset s1, Source.absoluteStartOffset s2) of
          EQUAL => Int.compare (Source.absoluteEndOffset s1, Source.absoluteEndOffset s2)
        | other => other
      end
  end

  structure TokenDict = Dict(TokenKey)
  structure TokenSet = Set(TokenKey)

  (* ====================================================================== *)
  (* ====================================================================== *)
  (* ====================================================================== *)

(*
  fun flowAts debug (doc: anndoc) =
    let
      fun dbgprintln s =
        if not debug then ()
        else print (s ^ "\n")

      datatype tab_constraint = Active | Inactive
      type context = tab_constraint TabDict.t

      fun markInactive ctx anntab =
        TabDict.insert ctx (removeTabAnnotation anntab, Inactive)

      fun markActive ctx anntab =
        case anntab of
          AnnRoot => ctx
        | AnnTab {parent, tab} =>
            markActive (TabDict.insert ctx (tab, Active)) parent

      fun loop ctx (flowval, doc) =
        case doc of
          AnnEmpty => (doc, flowval)
        | AnnNewline => (doc, flowval)
        | AnnSpace => (doc, flowval)
        | AnnNoSpace => (doc, flowval)
        | AnnToken {tok, ...} => (AnnToken {tok=tok, at=flowval}, NONE)
        | AnnText {txt, ...} => (AnnText {txt=txt, at=flowval}, NONE)
        | AnnBreak {tab, ...} => (doc, SOME tab)
        | AnnConcat (d1, d2) =>
        | AnnNewTab =>
        | AnnCond =>
    in
    end
*)

  (* ====================================================================== *)
  (* ====================================================================== *)
  (* ====================================================================== *)


  (* TODO: bug: inserting blank lines on 'at's is incorrect. You could
   * have multiple 'at's back-to-back that are essentially no-ops...
   *
   * An idea for a better solution? For each token, compute which tab it is
   * 'at', and insert the appropriate 'cond tab {active=newline, ...}'. To
   * compute which tokens are 'at' tabs, we can flow each 'at tab' downstream
   * until it hits token, text, or another 'at'. If it hits a token or text
   * element, then that element is 'at' the tab.
   *
   * NOTE: This idea also seems like it would work for inserting comments.
   * If a token is 'at' a tab, then we can insert the comment above
   * it at the same tab. If a token is not at a tab, then we can insert the
   * comment in-line before the token.
   *)
  fun insertBlankLines debug (doc: anndoc) =
    let
      fun dbgprintln s =
        if not debug then ()
        else print (s ^ "\n")

      datatype tab_constraint = Active | Inactive
      type context = tab_constraint TabDict.t

      fun markInactive ctx tab =
        TabDict.insert ctx (tab, Inactive)

      fun markActive ctx tab =
        case tab of
          Root => ctx
        | Tab {parent, ...} =>
            markActive (TabDict.insert ctx (tab, Active)) parent

      datatype edge = EdgeText | EdgeTokens of TokenSet.t

      fun edge {left: bool} ctx doc =
        let
          fun loop ctx doc =
            case doc of
              AnnEmpty => NONE
            | AnnNewline => NONE
            | AnnSpace => NONE
            | AnnNoSpace => NONE
            | AnnToken {tok=t, ...} => SOME (EdgeTokens (TokenSet.singleton t))
            | AnnText _ => SOME EdgeText
            | AnnBreak _ => NONE
            | AnnConcat (d1, d2) =>
                if left then
                  (case loop ctx d1 of
                    SOME xs => SOME xs
                  | NONE => loop ctx d2)
                else
                  (case loop ctx d2 of
                    SOME xs => SOME xs
                  | NONE => loop ctx d1)
            | AnnNewTab {doc=d, ...} => loop ctx d
            | AnnCond {tab, inactive, active} =>
                let
                  val result =
                    case TabDict.find ctx tab of
                      SOME Active =>
                        let
                          val result = loop ctx active
                        in
                          result
                        end
                    | SOME Inactive =>
                        let
                          val result = loop ctx inactive
                        in
                          result
                        end
                    | NONE =>
                        let
                          val r1 = loop (markInactive ctx tab) inactive
                          val r2 = loop (markActive ctx tab) active
                          val result =
                            case (r1, r2) of
                              (SOME (EdgeTokens xs), SOME (EdgeTokens ys)) =>
                                SOME (EdgeTokens (TokenSet.union (xs, ys)))
                            | (SOME EdgeText, SOME EdgeText) => SOME EdgeText
                            | (SOME (EdgeTokens _), _) => r1
                            | (_, SOME (EdgeTokens _)) => r2
                            | (NONE, _) => r2
                            | (_, NONE) => r1
                        in
                          result
                        end
                in
                  result
                end
        in
          loop ctx doc
        end

      fun leftEdge ctx doc = edge {left=true} ctx doc
      fun rightEdge ctx doc = edge {left=false} ctx doc

      fun breaksBefore doc tab n =
        if n = 0 then doc else
        let
          val doc =
            AnnConcat
              ( AnnCond {tab = tab, inactive = AnnEmpty, active = AnnNewline}
              , doc
              )
        in
          breaksBefore doc tab (n-1)
        end

      fun loop ctx (prev, doc, next) =
        case doc of
          AnnEmpty => doc
        | AnnNewline => doc
        | AnnNoSpace => doc
        | AnnSpace => doc
        | AnnToken _ => doc
        | AnnText _ => doc
        | AnnConcat (d1, d2) =>
            let
              val prev1 = prev
              val next1 =
                case leftEdge ctx d2 of
                  SOME t => SOME t
                | NONE => next

              val next2 = next
              val prev2 =
                case rightEdge ctx d1 of
                  SOME t => SOME t
                | NONE => prev

              val d1 = loop ctx (prev1, d1, next1)
              val d2 = loop ctx (prev2, d2, next2)
            in
              AnnConcat (d1, d2)
            end
        | AnnBreak {mightBeFirst, tab} =>
            (case (prev, next) of
              (SOME (EdgeTokens ts1), SOME (EdgeTokens ts2)) =>
                let
                  val t1 = List.last (TokenSet.listKeys ts1)
                  val t2 = List.hd (TokenSet.listKeys ts2)
                  val diff = Token.lineDifference (t1, t2) - 1
                  val diff = Int.max (0, Int.min (2, diff))

                  val _ = dbgprintln ("line diff ('" ^ Token.toString t1 ^ "','" ^ Token.toString t2 ^ "'): " ^ Int.toString diff)
                in
                  if diff = 0 then
                    doc
                  else
                    breaksBefore doc tab diff
                end

            | _ => doc)

        | AnnNewTab {tab, doc} =>
            let
              val doc = loop ctx (prev, doc, next)
            in
              AnnNewTab {tab=tab, doc=doc}
            end
        | AnnCond {tab, inactive, active} =>
            let
              val inactive = loop (markInactive ctx tab) (prev, inactive, next)
              val active = loop (markActive ctx tab) (prev, active, next)
            in
              AnnCond {tab=tab, inactive=inactive, active=active}
            end
    in
      loop TabDict.empty (NONE, doc, NONE)
    end

  (* ====================================================================== *)
  (* ====================================================================== *)
  (* ====================================================================== *)

  structure TCS = TerminalColorString

  fun tokenToStringDoc currentTab tabWidth tok =
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
        , D.newTab currentTab (D.Inplace, fn tab =>
            Seq.iterate
              D.concat
              D.empty
              (Seq.map (fn x => D.concat (D.at tab, x)) pieces))
        )
    end


  (* ====================================================================== *)


  fun toStringDoc (args as {tabWidth, debug}) doc =
    let
      fun dbgprintln s =
        if not debug then ()
        else print (s ^ "\n")

      val doc = annotate doc
      (* val _ = dbgprintln ("TabbedTokenDoc.toStringDoc before ensureSpaces: " ^ annToString doc) *)
      val doc = ensureSpaces debug doc
      (* val _ = dbgprintln ("TabbedTokenDoc.toStringDoc before insertBlankLines: " ^ annToString doc) *)
      val doc = insertBlankLines debug doc
      (* val _ = dbgprintln ("TabbedTokenDoc.toStringDoc after insertBlankLines: " ^ annToString doc) *)
      (* val doc = removeAnnotations doc *)

      fun loop currentTab tabmap doc =
        case doc of
          AnnEmpty => D.empty
        | AnnNoSpace => D.empty
        | AnnNewline => D.newline
        | AnnSpace => D.space
        | AnnConcat (d1, d2) =>
            D.concat (loop currentTab tabmap d1, loop currentTab tabmap d2)
        | AnnText {txt, ...} => D.text (TerminalColorString.fromString txt)
        | AnnToken {tok, ...} =>
            let
              val (shouldBeRigid, doc) = tokenToStringDoc currentTab tabWidth tok
            in
              (* TODO: rigidity (don't allow flattening) *)
              doc
            end
        | AnnBreak {tab, ...} =>
            D.at (TabDict.lookup tabmap tab)
        | AnnCond {tab, inactive, active} =>
            D.cond (TabDict.lookup tabmap tab)
              { inactive = loop currentTab tabmap inactive
              , active = loop currentTab tabmap active
              }
        | AnnNewTab {tab, doc} =>
            let
              val s = case style tab of Inplace => D.Inplace | Indented => D.Indented
            in
              D.newTab
                (TabDict.lookup tabmap (valOf (parent tab)))
                (s, fn tab' =>
                  loop tab' (TabDict.insert tabmap (tab, tab')) doc)
            end
    in
      loop D.root (TabDict.singleton (Root, D.root)) doc
    end

end