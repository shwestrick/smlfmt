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
  | AnnToken of {at: TabSet.t option, tok: Token.t}
  | AnnText of {at: TabSet.t option, txt: string}
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

  (* ====================================================================== *)
  (* ====================================================================== *)
  (* ====================================================================== *)

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

  fun flowAts debug (doc: anndoc) =
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

      fun loop ctx (flowval, doc) =
        case doc of
          AnnEmpty => (flowval, doc)
        | AnnNewline => (flowval, doc)
        | AnnSpace => (flowval, doc)
        | AnnNoSpace => (flowval, doc)
        | AnnToken {tok, ...} =>
            let
              val _ =
                Option.app (fn ts =>
                  dbgprintln
                    ("token '" ^ Token.toString tok ^ "' at: " ^
                     String.concatWith " " (List.map tabToString (TabSet.listKeys ts))))
                flowval
            in
              (NONE, AnnToken {tok=tok, at=flowval})
            end
        | AnnText {txt, ...} =>
            let
              val _ =
                Option.app (fn ts =>
                  dbgprintln
                    ("text '" ^ txt ^ "' at: " ^
                     String.concatWith " " (List.map tabToString (TabSet.listKeys ts))))
                flowval
            in
              (NONE, AnnText {txt=txt, at=flowval})
            end
        | AnnBreak {tab, ...} => (SOME (TabSet.singleton tab), doc)
        | AnnConcat (d1, d2) =>
            let
              val (flowval, d1) = loop ctx (flowval, d1)
              val (flowval, d2) = loop ctx (flowval, d2)
            in
              (flowval, AnnConcat (d1, d2))
            end
        | AnnNewTab {tab, doc} =>
            let
              val (flowval, doc) = loop ctx (flowval, doc)
            in
              (flowval, AnnNewTab {tab=tab, doc=doc})
            end
        | AnnCond {tab, active, inactive} =>
            case TabDict.find ctx tab of
              SOME Active => loop ctx (flowval, active)
            | SOME Inactive => loop ctx (flowval, inactive)
            | _ =>
                let
                  val (flow1, inactive) = loop (markInactive ctx tab) (flowval, inactive)
                  val (flow2, active) = loop (markActive ctx tab) (flowval, active)
                  val flowval =
                    case (flow1, flow2) of
                      (SOME ts1, SOME ts2) => SOME (TabSet.union (ts1, ts2))
                    | (NONE, _) => flow2
                    | (_, NONE) => flow1
                in
                  (flowval, AnnCond {tab=tab, active=active, inactive=inactive})
                end

      val (_, doc) = loop TabDict.empty (SOME (TabSet.singleton Root), doc)
    in
      doc
    end

  (* ====================================================================== *)
  (* ====================================================================== *)
  (* ====================================================================== *)


  (* TODO: insert comments after the final token, too. *)

  fun insertComments debug (doc: anndoc) =
    let
      fun dbgprintln s =
        if not debug then ()
        else print (s ^ "\n")

      fun isLast tok =
        not (Option.isSome (Token.nextTokenNotCommentOrWhitespace tok))

      fun commentsToDocs atflow cs =
        Seq.map (fn c => AnnToken {at = atflow, tok=c}) cs

      fun loop doc =
        case doc of
          AnnEmpty => doc
        | AnnNewline => doc
        | AnnNoSpace => doc
        | AnnSpace => doc
        | AnnText _ => doc
        | AnnBreak {mightBeFirst, tab} => doc
        | AnnConcat (d1, d2) =>
            AnnConcat (loop d1, loop d2)
        | AnnNewTab {tab, doc} =>
            AnnNewTab {tab = tab, doc = loop doc}
        | AnnCond {tab, inactive, active} =>
            AnnCond {tab = tab, inactive = loop inactive, active = loop active}

        | AnnToken {at = NONE, tok} =>
            let
              val commentsBefore =
                commentsToDocs NONE (Token.commentsBefore tok)
              val commentsAfter =
                if not (isLast tok) then Seq.empty () else
                commentsToDocs NONE (Token.commentsAfter tok)
              val all =
                Seq.append3 (commentsBefore, Seq.singleton doc, commentsAfter)
            in
              Seq.iterate AnnConcat AnnEmpty all
            end

        | AnnToken {at = flow as SOME tabs, tok} =>
            let
              val tab =
                (* TODO: what to do when there are multiple possible tabs
                 * this token could be at? Here we just pick the first
                 * of these in the set, and usually it seems each token
                 * is only ever 'at' one possible tab...
                 *)
                List.hd (TabSet.listKeys tabs)

              val atflow = SOME (TabSet.singleton tab)

              val commentsBefore =
                commentsToDocs atflow (Token.commentsBefore tok)

              val commentsAfter =
                if not (isLast tok) then Seq.empty () else
                commentsToDocs atflow (Token.commentsAfter tok)

              fun fixflow d =
                case d of
                  AnnToken {tok, ...} => AnnToken {at=flow, tok=tok}
                | _ => raise Fail "TabbedTokenDoc.insertComments.loop.AnnToken: bug"

              fun withBreak d =
                AnnConcat (AnnBreak {mightBeFirst=false, tab=tab}, d)

              val all =
                Seq.append3 (commentsBefore, Seq.singleton doc, commentsAfter)
            in
              Seq.iterate AnnConcat
                (fixflow (Seq.nth all 0))
                (Seq.map withBreak (Seq.drop all 1))
            end

    in
      loop doc
    end

  (* ====================================================================== *)
  (* ====================================================================== *)
  (* ====================================================================== *)

  fun insertBlankLines debug (doc: anndoc) =
    let
      fun dbgprintln s =
        if not debug then ()
        else print (s ^ "\n")

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

      fun prevTokenNotWhitespace t =
        case Token.prevToken t of
          NONE => NONE
        | SOME p =>
            if Token.isWhitespace p then
              prevTokenNotWhitespace p
            else
              SOME p

      fun loop doc =
        case doc of
          AnnEmpty => doc
        | AnnNewline => doc
        | AnnNoSpace => doc
        | AnnSpace => doc
        | AnnText _ => doc
        | AnnBreak {mightBeFirst, tab} => doc
        | AnnToken {at = NONE, tok} => doc
        | AnnToken {at = SOME tabs, tok} =>
            (case prevTokenNotWhitespace tok of
              NONE => doc
            | SOME prevTok =>
                let
                  val diff = Token.lineDifference (prevTok, tok) - 1
                  val diff = Int.max (0, Int.min (2, diff))
                  val _ = dbgprintln ("line diff ('" ^ Token.toString prevTok ^ "','" ^ Token.toString tok ^ "'): " ^ Int.toString diff)
                in
                  if diff = 0 then
                    doc
                  else
                    (* TODO: what to do when there are multiple possible tabs
                     * this token could be at? Here we just pick the first
                     * of these in the set, and usually it seems each token
                     * is only ever 'at' one possible tab...
                     *)
                    breaksBefore doc (List.hd (TabSet.listKeys tabs)) diff
                end)
        | AnnConcat (d1, d2) =>
            AnnConcat (loop d1, loop d2)
        | AnnNewTab {tab, doc} =>
            AnnNewTab {tab = tab, doc = loop doc}
        | AnnCond {tab, inactive, active} =>
            AnnCond {tab = tab, inactive = loop inactive, active = loop active}
    in
      loop doc
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
      val doc = flowAts debug doc
      val doc = insertComments debug doc
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
        | AnnToken {at, tok} =>
            let
              val tab =
                case at of
                  NONE => currentTab
                | SOME tabs =>
                    TabDict.lookup tabmap (List.hd (TabSet.listKeys tabs))

              val (shouldBeRigid, doc) = tokenToStringDoc tab tabWidth tok
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