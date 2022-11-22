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

  type tab
  val root: tab
  val newTab: tab -> (tab -> doc) -> doc
  val at: tab -> doc
  val cond: tab -> {inactive: doc, active: doc} -> doc

  val toStringDoc: {tabWidth: int, debug: bool} -> doc -> TabbedStringDoc.t
end =
struct
  
  structure D = TabbedStringDoc

  (* Just need a unique name *)
  datatype tab =
    Tab of D.tab option ref * int
  | Root

  val tabCounter = ref 0

  fun mkTab () =
    let
      val c = !tabCounter
    in
      tabCounter := c+1;
      Tab (ref NONE, c)
    end

  
  val root = Root


  fun tabToString t =
    case t of
      Tab (_, c) => "[" ^ Int.toString c ^ "]"
    | Root => "[root]"

  
  fun underlyingTab t =
    case t of
      Tab (tabref, _) => Option.valOf (!tabref)
    | Root => D.root

  
  fun setUnderlyingTab t dt =
    case t of
      Tab (tabref, _) => tabref := SOME dt
    | Root => raise Fail "TabbedTokenDoc.setUnderlyingTab: Root"


  structure TabDict =
    Dict(struct
      type t = tab
      fun compare (t1: tab, t2: tab) : order =
        case (t1, t2) of
          (Root, Root) => EQUAL
        | (Tab t1, Tab t2) => Int.compare (#2 t1, #2 t2)
        | (Tab _, Root) => GREATER
        | (Root, Tab _) => LESS
    end)


  datatype doc =
    Empty
  | Space
  | NoSpace
  | Concat of doc * doc
  | Token of Token.t
  | Text of string
  | Break of tab
  | NewTab of {parent: tab, tab: tab, doc: doc}
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
    | NewTab {parent=p, tab=t, doc=d} => "NewTab(" ^ tabToString p ^ ", " ^ tabToString t ^ ", " ^ toString d ^ ")"
    | Cond {tab=t, inactive=df, active=dnf} =>
        "Cond(" ^ tabToString t ^ ", " ^ toString df ^ ", " ^ toString dnf ^ ")"

  
  fun newTab parent (genDocUsingTab: tab -> doc) =
    let
      val t = mkTab ()
      val d = genDocUsingTab t
    in
      NewTab {parent=parent, tab=t, doc=d}
    end

  (* ====================================================================== *)
  (* ====================================================================== *)
  (* ====================================================================== *)

  fun ensureSpaces debug doc =
    let
      fun dbgprintln s =
        if not debug then ()
        else print (s ^ "\n")
      val _ = dbgprintln ("ensureSpaces INPUT: " ^ toString doc)

      datatype anntab =
        AnnRoot
      | AnnTab of {parent: anntab, tab: tab}

      fun annParent t =
        case t of
          AnnRoot => NONE
        | AnnTab {parent, ...} => SOME parent

      fun removeTabAnnotation t =
        case t of
          AnnTab {tab, ...} => tab
        | AnnRoot => Root

      datatype anndoc =
        AnnEmpty
      | AnnNoSpace
      | AnnSpace
      | AnnConcat of anndoc * anndoc
      | AnnToken of Token.t
      | AnnText of string
      | AnnBreak of {mightBeFirst: bool, tab: anntab}
      | AnnNewTab of {parent: anntab, tab: anntab, doc: anndoc}
      | AnnCond of {tab: anntab, inactive: anndoc, active: anndoc}

    
      fun annTabToString t =
        case t of
          AnnRoot => "[root]"
        | AnnTab {tab, ...} => tabToString tab

      fun annToString doc =
        case doc of
          AnnEmpty => ""
        | AnnSpace => "_"
        | AnnNoSpace => "NoSpace"
        | AnnConcat (d1, d2) => annToString d1 ^ " ++ " ^ annToString d2
        | AnnToken t => "Token('" ^ Token.toString t ^ "')"
        | AnnText t => "Text('" ^ t ^ "')"
        | AnnBreak {mightBeFirst, tab} =>
            "Break" ^ (if mightBeFirst then "!!" else "") ^ "(" ^ annTabToString tab ^ ")"
        | AnnNewTab {parent=p, tab=t, doc=d} => "NewTab(" ^ annTabToString p ^ ", " ^ annTabToString t ^ ", " ^ annToString d ^ ")"
        | AnnCond {tab=t, inactive=df, active=dnf} =>
            "Cond(" ^ annTabToString t ^ ", " ^ annToString df ^ ", " ^ annToString dnf ^ ")"


      fun annotate doc =
        let
          val () = ()

          (* if tab in broken, then tab has definitely had at least one break *)
          fun loop currtab tabmap (doc, broken) =
            case doc of
              Empty => (AnnEmpty, broken)
            | Space => (AnnSpace, broken)
            | NoSpace => (AnnNoSpace, broken)
            | Token t => (AnnToken t, broken)
            | Text t => (AnnText t, broken)
            | Break tab =>
                let
                  val (mightBeFirst, broken) =
                    if TabDict.contains broken tab then
                      (false, broken)
                    else
                      (true, TabDict.insert broken (tab, ()))
                in
                  ( AnnBreak
                      { mightBeFirst = mightBeFirst
                      , tab = TabDict.lookup tabmap tab
                      }
                  , broken
                  )
                end
            | Concat (d1, d2) =>
                let
                  val (d1, broken) = loop currtab tabmap (d1, broken)
                  val (d2, broken) = loop currtab tabmap (d2, broken)
                in
                  (AnnConcat (d1, d2), broken)
                end
            | NewTab {parent, tab, doc} =>
                let
                  val annparent = TabDict.lookup tabmap parent
                  val anntab = AnnTab {parent = annparent, tab = tab}
                  val tabmap = TabDict.insert tabmap (tab, anntab)
                  val (doc, broken) = loop anntab tabmap (doc, broken)
                in
                  ( AnnNewTab 
                     { parent = annparent
                     , tab = anntab
                     , doc = doc
                     }
                   , broken
                   )
                end
            | Cond {tab, inactive, active} =>
                let
                  val anntab = TabDict.lookup tabmap tab
                  val (inactive, broken1) = loop currtab tabmap (inactive, broken)
                  val (active, broken2) = loop currtab tabmap (active, broken)
                in
                  ( AnnCond {tab=anntab, inactive=inactive, active=active}
                  , TabDict.intersectWith (fn _ => ()) (broken1, broken2)
                  )
                end
          
          val init = TabDict.singleton (Root, AnnRoot)
          val (anndoc, _) = loop AnnRoot init (doc, TabDict.empty)
        in
          anndoc
        end

      
      fun removeAnnotations anndoc =
        case anndoc of
          AnnEmpty => Empty
        | AnnSpace => Space
        | AnnNoSpace => NoSpace
        | AnnToken t => Token t
        | AnnText t => Text t
        | AnnBreak {tab, ...} => Break (removeTabAnnotation tab)
        | AnnConcat (d1, d2) =>
            Concat (removeAnnotations d1, removeAnnotations d2)
        | AnnNewTab {parent, tab, doc} =>
            NewTab 
              { parent = removeTabAnnotation parent
              , tab = removeTabAnnotation tab
              , doc = removeAnnotations doc
              }
        | AnnCond {tab, inactive, active} =>
            Cond
              { tab = removeTabAnnotation tab
              , inactive = removeAnnotations inactive
              , active = removeAnnotations active
              }


      datatype edge = Spacey | MaybeNotSpacey
      datatype tab_constraint = Active | Inactive
      type context = tab_constraint TabDict.t

      fun edgeOptToString e =
        case e of
          NONE => "NONE"
        | SOME Spacey => "Spacey"
        | SOME MaybeNotSpacey => "MaybeNotSpacey"

      fun markInactive ctx anntab =
        TabDict.insert ctx (removeTabAnnotation anntab, Inactive)

      fun markActive ctx anntab =
        case anntab of
          AnnRoot => ctx
        | AnnTab {parent, tab} =>
            markActive (TabDict.insert ctx (tab, Active)) parent

      fun edge {left: bool} ctx doc =
        let
          fun loop ctx doc =
            case doc of
              AnnEmpty => NONE
            | AnnSpace => SOME Spacey
            | AnnNoSpace => SOME Spacey (* pretends to be a space, but then actually is elided *)
            | AnnToken _ => SOME MaybeNotSpacey
            | AnnText _ => SOME MaybeNotSpacey
            | AnnBreak {mightBeFirst, tab} =>
                (case TabDict.find ctx (removeTabAnnotation tab) of
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
                    case TabDict.find ctx (removeTabAnnotation tab) of
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
              (case TabDict.find ctx (removeTabAnnotation tab) of
                SOME Inactive =>
                  ( dbgprintln ("need space at INACTIVE " ^ annToString doc)
                  ; AnnSpace
                  )
              | _ => 
                  ( dbgprintln ("need space at UNKNOWN " ^ annToString doc)
                  ; AnnConcat (AnnSpace, doc)
                  ))
        | AnnNewTab {parent, tab, doc} =>
            AnnNewTab {parent = parent, tab = tab, doc = loop ctx needSpace doc}
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
      
      val anndoc = annotate doc
      val _ = dbgprintln ("ensureSpaces ANNOTATED: " ^ annToString anndoc)

      val result = loop TabDict.empty (false, false) anndoc
      val result = removeAnnotations result
      val _ = dbgprintln ("ensureSpaces OUTPUT: " ^ toString result)
    in
      result
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
        , D.newTab currentTab (fn tab =>
            Seq.iterate
              D.concat
              D.empty
              (Seq.map (fn x => D.concat (D.at tab, x)) pieces))
        )
    end


  (* ====================================================================== *)


  fun toStringDoc (args as {tabWidth, debug}) doc =
    let
      val doc = ensureSpaces debug doc

      fun loop currentTab doc =
        case doc of
          Empty => D.empty
        | NoSpace => D.empty
        | Space => D.space
        | Concat (d1, d2) => D.concat (loop currentTab d1, loop currentTab d2)
        | Text str => D.text (TerminalColorString.fromString str)
        | Token tok =>
            let
              val (shouldBeRigid, doc) = tokenToStringDoc currentTab tabWidth tok
            in
              (* TODO: rigidity (don't allow flattening) *)
              doc
            end
        | Break tab =>
            D.at (underlyingTab tab)
        | Cond {tab, inactive, active} =>
            D.cond (underlyingTab tab)
              { inactive = loop currentTab inactive
              , active = loop currentTab active
              }
        | NewTab {parent, tab, doc} =>
            D.newTab (underlyingTab parent) (fn tab' =>
              ( setUnderlyingTab tab tab'
              ; loop tab' doc
              ))
    in
      loop D.root doc
    end

end