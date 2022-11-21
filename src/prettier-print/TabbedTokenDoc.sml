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
  val token: Token.t -> doc
  val text: string -> doc
  val concat: doc * doc -> doc

  type tab
  val newChildTab: tab -> (tab -> doc) -> doc
  val newTab: (tab -> doc) -> doc
  val break: tab -> doc
  val breakspace: tab -> doc
  val spaceIfNotFlat: tab -> doc
  val cond: tab -> {flat: doc, notflat: doc} -> doc

  val toStringDoc: {tabWidth: int} -> doc -> TabbedStringDoc.t
end =
struct

  (* Just need a unique name *)
  type tab = TabbedStringDoc.tab option ref * int

  val tabCounter = ref 0

  fun mkTab () =
    let
      val c = !tabCounter
    in
      tabCounter := c+1;
      (ref NONE, c)
    end

  fun tabToString ((_, c): tab) = "[" ^ Int.toString c ^ "]"


  structure TabDict =
    Dict(struct
      type t = tab
      fun compare (t1: tab, t2: tab) : order = Int.compare (#2 t1, #2 t2)
    end)


  datatype doc =
    Empty
  | Space
  | Concat of doc * doc
  | Token of Token.t
  | Text of string
  | Break of tab
  | NewTab of {tab: tab, doc: doc}
  | NewChildTab of {parent: tab, tab: tab, doc: doc}
  | Cond of {tab: tab, flat: doc, notflat: doc}

  type t = doc

  val empty = Empty
  val space = Space
  val token = Token
  val text = Text
  val break = Break

  fun concat (d1, d2) =
    case (d1, d2) of
      (Empty, _) => d2
    | (_, Empty) => d1
    | _ => Concat (d1, d2)

  fun cond tab {flat, notflat} = Cond {tab=tab, flat=flat, notflat=notflat}

  fun breakspace t = cond t {flat = space, notflat = break t}
  fun spaceIfNotFlat t = cond t {flat = empty, notflat = space}


  fun toString doc =
    case doc of
      Empty => ""
    | Space => "_"
    | Concat (d1, d2) => toString d1 ^ " ++ " ^ toString d2
    | Token t => "Token('" ^ Token.toString t ^ "')"
    | Text t => "Text('" ^ t ^ "')"
    | Break t => "Break(" ^ tabToString t ^ ")"
    | NewTab {tab=t, doc=d} => "NewTab(" ^ tabToString t ^ ", " ^ toString d ^ ")"
    | NewChildTab {parent=p, tab=t, doc=d} => "NewChildTab(" ^ tabToString p ^ ", " ^ tabToString t ^ ", " ^ toString d ^ ")"
    | Cond {tab=t, flat=df, notflat=dnf} =>
        "Cond(" ^ tabToString t ^ ", " ^ toString df ^ ", " ^ toString dnf ^ ")"


  fun newTab (genDocUsingTab: tab -> doc) =
    let
      val t = mkTab ()
      val d = genDocUsingTab t
    in
      NewTab {tab=t, doc=d}
    end

  
  fun newChildTab parent (genDocUsingTab: tab -> doc) =
    let
      val t = mkTab ()
      val d = genDocUsingTab t
    in
      NewChildTab {parent=parent, tab=t, doc=d}
    end

  (* ====================================================================== *)


(*
  fun ensureSpaces doc =
    let
      val _ = print ("ensureSpaces INPUT: " ^ toString doc ^ "\n")
      val () = ()

      (* Soft edge is whitespace, hard edge is token or text.
       * Goal is to ensure no two hard edges touch.
       * Have to be conservative sometimes... if we can't know an edge
       * for sure, then we assign it DefinitelyOrMaybeHard
       *
       * Some edges are neutral, i.e., they are neither soft nor hard.
       *)
      datatype edge = Soft | DefinitelyOrMaybeHard | Neutral

      fun edgeUnion (e1, e2) =
        case (e1, e2) of
          (Neutral, _) => e2
        | (_, Neutral) => e1
        | (Soft, Soft) => Soft
        | _ => DefinitelyOrMaybeHard

      datatype tab_constraint = Active | Inactive

      fun maybeConcatWithSpace (d1, r1, l2, d2) =
        case (r1, l2) of
          (DefinitelyOrMaybeHard, DefinitelyOrMaybeHard) =>
            Concat (d1, Concat (Space, d2))
        | _ => Concat (d1, d2)

      fun loop ctx doc : edge * edge * doc =
        case doc of
          Empty => (Neutral, Neutral, doc)
        | Space => (Soft, Soft, doc)
        | Token _ => (DefinitelyOrMaybeHard, DefinitelyOrMaybeHard, doc)
        | Text _ => (DefinitelyOrMaybeHard, DefinitelyOrMaybeHard, doc)
        | Concat (d1, d2) =>
            let
              val (l1, r1, d1') = loop ctx d1
              val (l2, r2, d2') = loop ctx d2
            in
              (l1, r2, maybeConcatWithSpace (d1', r1, l2, d2'))
            end
        | Break tab =>
            (Neutral, Neutral, doc)
        | NewTab {tab, doc} =>
            let
              val (l, r, doc') = loop ctx doc
            in
              (l, r, NewTab {tab=tab, doc=doc'})
            end
        | NewChildTab {parent, tab, doc} =>
            let
              val (l, r, doc') = loop ctx doc
            in
              (l, r, NewChildTab {parent=parent, tab=tab, doc=doc'})
            end
        | Cond {tab, flat=d1, notflat=d2} =>
            let
              val (l1, r1, d1') = loop (TabDict.insert ctx (tab, Inactive)) d1
              val (l2, r2, d2') = loop (TabDict.insert ctx (tab, Active)) d2
            in
              ( edgeUnion (l1, l2)
              , edgeUnion (r1, r2)
              , Cond {tab=tab, flat=d1', notflat=d2'}
              )
            end
      
      val (_, _, doc') = loop TabDict.empty doc
      val _ = print ("ensureSpaces OUTPUT: " ^ toString doc' ^ "\n")
    in
      doc'
    end
*)

(*
  fun last doc =
    let
      fun loop doc =
        case doc of
          Empty => NONE
        | Space => SOME [doc]
        | Token _ => SOME [doc]
        | Text _ => SOME [doc]
        | Break tab => NONE
        | Concat (d1, d2) =>
            (case loop d2 of
              SOME xs => SOME xs
            | NONE => loop d1)
        | NewTab {doc=d, ...} => loop d
        | NewChildTab {doc=d, ...} => loop d
        | Cond {flat, notflat, ...} =>
            let
              val r1 = loop flat
              val r2 = loop notflat
            in
              case (r1, r2) of
                (SOME xs, SOME ys) => SOME (xs @ ys)
              | (SOME _, _) => r1
              | (_, SOME _) => r2
            end
    in
      loop doc
    end
*)

  fun ensureSpaces doc =
    let
      val _ = print ("ensureSpaces INPUT: " ^ toString doc ^ "\n")

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
        | AnnRoot => raise Fail "TabbedTokenDoc.removeTabAnnotation: Root"

      datatype anndoc =
        AnnEmpty
      | AnnSpace
      | AnnConcat of anndoc * anndoc
      | AnnToken of Token.t
      | AnnText of string
      | AnnBreak of {mightBeFirst: bool, tab: anntab}
      | AnnNewTab of {tab: anntab, doc: anndoc}
      | AnnNewChildTab of {parent: anntab, tab: anntab, doc: anndoc}
      | AnnCond of {tab: anntab, flat: anndoc, notflat: anndoc}


      fun annotate doc =
        let
          val () = ()

          (* if tab in broken, then tab has definitely had at least one break *)
          fun loop currtab tabmap (doc, broken) =
            case doc of
              Empty => (AnnEmpty, broken)
            | Space => (AnnSpace, broken)
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
            | NewTab {tab, doc} =>
                let
                  val anntab = AnnTab {parent = currtab, tab = tab}
                  val tabmap = TabDict.insert tabmap (tab, anntab)
                  val (doc, broken) = loop anntab tabmap (doc, broken)
                in
                  (AnnNewTab {tab=anntab, doc=doc}, broken)
                end
            | NewChildTab {parent, tab, doc} =>
                let
                  val annparent = TabDict.lookup tabmap parent
                  val anntab = AnnTab {parent = annparent, tab = tab}
                  val tabmap = TabDict.insert tabmap (tab, anntab)
                  val (doc, broken) = loop anntab tabmap (doc, broken)
                in
                  ( AnnNewChildTab 
                     { parent = annparent
                     , tab = anntab
                     , doc = doc
                     }
                   , broken
                   )
                end
            | Cond {tab, flat, notflat} =>
                let
                  val anntab = TabDict.lookup tabmap tab
                  val (flat, broken1) = loop currtab tabmap (flat, broken)
                  val (notflat, broken2) = loop currtab tabmap (notflat, broken)
                in
                  ( AnnCond {tab=anntab, flat=flat, notflat=notflat}
                  , TabDict.intersectWith (fn _ => ()) (broken1, broken2)
                  )
                end
          
          val (anndoc, _) = loop AnnRoot TabDict.empty (doc, TabDict.empty)
        in
          anndoc
        end

      
      fun removeAnnotations anndoc =
        case anndoc of
          AnnEmpty => Empty
        | AnnSpace => Space
        | AnnToken t => Token t
        | AnnText t => Text t
        | AnnBreak {tab, ...} => Break (removeTabAnnotation tab)
        | AnnConcat (d1, d2) =>
            Concat (removeAnnotations d1, removeAnnotations d2)
        | AnnNewTab {tab, doc} =>
            NewTab {tab = removeTabAnnotation tab, doc = removeAnnotations doc}
        | AnnNewChildTab {parent, tab, doc} =>
            NewChildTab 
              { parent = removeTabAnnotation parent
              , tab = removeTabAnnotation tab
              , doc = removeAnnotations doc
              }
        | AnnCond {tab, flat, notflat} =>
            Cond
              { tab = removeTabAnnotation tab
              , flat = removeAnnotations flat
              , notflat = removeAnnotations notflat
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

      fun leftEdge ctx doc =
        let
          fun loop ctx doc =
            case doc of
              AnnEmpty => NONE
            | AnnSpace => SOME Spacey
            | AnnToken _ => SOME MaybeNotSpacey
            | AnnText _ => SOME MaybeNotSpacey
            | AnnBreak {mightBeFirst, tab} =>
                (case TabDict.find ctx (removeTabAnnotation tab) of
                  SOME Inactive => NONE
                | SOME Active => 
                    if mightBeFirst then
                      SOME MaybeNotSpacey
                    else
                      SOME Spacey
                | _ =>
                    SOME MaybeNotSpacey)
            | AnnConcat (d1, d2) =>
                (case loop ctx d1 of
                  SOME xs => SOME xs
                | NONE => loop ctx d2)
            | AnnNewTab {doc=d, ...} => loop ctx d
            | AnnNewChildTab {doc=d, ...} => loop ctx d
            | AnnCond {tab, flat, notflat} =>
                let
                  val r1 = loop (markInactive ctx tab) flat
                  val r2 = loop (markActive ctx tab) notflat
                in
                  case (r1, r2) of
                    (SOME MaybeNotSpacey, _) => SOME MaybeNotSpacey
                  | (_, SOME MaybeNotSpacey) => SOME MaybeNotSpacey
                  | (SOME Spacey, SOME Spacey) => SOME Spacey
                  | (NONE, _) => r2
                  | (_, NONE) => r1
                end
        in
          loop ctx doc
        end

      fun loop ctx (doc: anndoc, next) =
        case doc of
          AnnToken t =>
            let
              val _ = print ("Token " ^ Token.toString t ^ " next to " ^ edgeOptToString next ^ "\n")
              val doc = case next of SOME MaybeNotSpacey => AnnConcat (doc, AnnSpace) | _ => doc
            in
              doc
            end
        | AnnText t =>
            let
              val _ = print ("Text " ^ t ^ " next to " ^ edgeOptToString next ^ "\n")
              val doc = case next of SOME MaybeNotSpacey => AnnConcat (doc, AnnSpace) | _ => doc
            in
              doc
            end
        | AnnEmpty => doc
        | AnnSpace => doc
        | AnnBreak _ => doc
        | AnnConcat (d1, d2) =>
            AnnConcat (loop ctx (d1, leftEdge ctx d2), loop ctx (d2, next))
        | AnnNewTab {tab, doc} =>
            AnnNewTab {tab = tab, doc = loop ctx (doc, next)}
        | AnnNewChildTab {parent, tab, doc} =>
            AnnNewChildTab {parent = parent, tab = tab, doc = loop ctx (doc, next)}
        | AnnCond {tab, flat, notflat} =>
            let
              val flat = loop (markInactive ctx tab) (flat, next)
              val notflat = loop (markActive ctx tab) (notflat, next)
            in
              AnnCond {tab = tab, flat = flat, notflat = notflat}
            end
      
      val result = loop TabDict.empty (annotate doc, NONE)
      val result = removeAnnotations result
      val _ = print ("ensureSpaces OUTPUT: " ^ toString result ^ "\n")
    in
      result
    end


  (* ====================================================================== *)

  structure TCS = TerminalColorString
  structure D = TabbedStringDoc

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


  fun toStringDoc (args as {tabWidth}) doc =
    let
      val doc = ensureSpaces doc

      fun loop currentTab doc =
        case doc of
          Empty => D.empty
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
        | Break (tab as (tabref, _)) =>
            D.at (Option.valOf (!tabref))
        | Cond {tab = (tabref, _), flat, notflat} =>
            D.cond (Option.valOf (!tabref))
              { inactive = loop currentTab flat
              , active = loop currentTab notflat
              }
        | NewTab {tab=(tabref,_), doc} =>
            D.newTab currentTab (fn tab' =>
              ( tabref := SOME tab'
              ; loop tab' doc
              ))
        | NewChildTab {parent=(ptabref, _), tab=(tabref,_), doc} =>
            D.newTab (Option.valOf (!ptabref)) (fn tab' =>
              ( tabref := SOME tab'
              ; loop tab' doc
              ))
    in
      loop D.root doc
    end

end