(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

(** Functor argument CustomString could either be a standard string, or could
  * be a TerminalColorString, etc.
  *)
functor PrettyTabbedDoc
  (CustomString:
    sig
      type t
      val substring: t * int * int -> t

      (* should be visually distinct, e.g., color the background.
       * the integer argument is a depth; this can be ignored (in which
       * case all depths will be emphasized the same) or can be used
       * to distinguish different tab depths 
       *)
      val emphasize: int -> t -> t

      val fromString: string -> t
      val toString: t -> string
      val size: t -> int
      val concat: t list -> t
    end) :>
sig
  type doc
  type t = doc

  exception InvalidDoc

  val empty: doc
  val space: doc
  val text: CustomString.t -> doc
  val concat: doc * doc -> doc

  type tab
  val root: tab
  val newTab: tab -> (tab -> doc) -> doc
  val break: tab -> doc
  val cond: tab -> {flat: doc, notflat: doc} -> doc

  val pretty: {ribbonFrac: real, maxWidth: int, indentWidth: int, debug: bool}
           -> doc
           -> CustomString.t

  val toString: doc -> CustomString.t
end =
struct

  (* IDEA: lazily activate tabs. If size/ribbon are violated, then
   * promote the outermost tab.
   *
   * Promotion follows this progression, which improves horizontal compaction:
   *       Flattened -> ActivatedInPlace -> ActivatedIndented
   *           -----------------> (horizontal compaction)
   *)


  (* ====================================================================== *)

  structure Tab =
  struct
    datatype activation_state = Flattened | Activated of int option
    (* datatype location_state = LocUnknown | LocInPlace | LocIndented  *)
    datatype state =
      Fresh
    | Usable of activation_state
    | Completed

    datatype tab =
      Tab of {state: state ref, id: int, parent: tab}
    | Root

    type t = tab

    val tabCounter = ref 0

    fun make parent =
      let
        val c = !tabCounter
      in
        tabCounter := c+1;
        Tab {state = ref Fresh, id = c, parent = parent}
      end

    fun eq (t1, t2) =
      case (t1, t2) of
        (Tab {id=c1, ...}, Tab {id=c2, ...}) => c1 = c2
      | (Root, Root) => true
      | _ => false

    fun getState t =
      case t of
        Tab {state=r, ...} => !r
      | Root => Usable (Activated (SOME 0))

    fun setState t x =
      case t of
        Tab {state=r, ...} => r := x
      | Root => ()

    fun isActivated t =
      case t of
        Root => true
      | Tab {state=r, ...} =>
          case !r of
            Usable (Activated _) => true
          | Usable (Flattened) => false
          | _ => raise Fail "PrettyTabbedDoc.Tab.isActivated: bad tab"

    fun parent t =
      case t of
        Root => NONE
      | Tab {parent=p, ...} => SOME p

    fun oldestInactiveParent t =
      if isActivated t then NONE else
      case parent t of
        SOME p =>
          if isActivated p then
            SOME t
          else
            oldestInactiveParent p
      | NONE => SOME t

    fun infoString t =
      case t of
        Root => "[root]"
      | Tab {state=r, id=c, parent=p} =>
          let
            val pinfo =
              case p of
                Root => ""
              | Tab {id=pid, ...} => Int.toString pid ^ "<-"
            val info =
              case !r of
                Usable Flattened => "f"
              | Usable (Activated NONE) => "a?"
              | Usable (Activated (SOME _)) => "a"
              | _ => "x"
          in
            "[" ^ pinfo ^ Int.toString c ^ info ^ "]"
          end

    fun name t =
      case t of
        Root => "root"
      | Tab {id=c, ...} => Int.toString c

    
    fun compare (t1, t2) =
      case (t1, t2) of
        (Tab {id=c1, ...}, Tab {id=c2, ...}) => Int.compare (c1, c2)
      | (Root, Root) => EQUAL
      | (Root, _) => LESS
      | (_, Root) => GREATER

    fun depth t =
      case t of
        Root => 0
      | Tab {parent=p, ...} => 1 + depth p

  end

  structure TabDict = Dict(Tab)

  (* ====================================================================== *)

  exception InvalidDoc

  type tab = Tab.t

  val root = Tab.Root

  datatype doc =
    Empty
  | Space
  | Concat of doc * doc
  | Text of CustomString.t
  | Break of tab
  | NewTab of {parent: tab, tab: tab, doc: doc}
  | Cond of {tab: tab, flat: doc, notflat: doc}

  type t = doc

  val empty = Empty
  val space = Space
  val text = Text
  val break = Break

  fun hasNoBreaks doc =
    case doc of
      Break _ => false
    | Concat (d1, d2) => hasNoBreaks d1 andalso hasNoBreaks d2
    | NewTab {doc=d, ...} => hasNoBreaks d
    | Cond {flat, notflat, ...} =>
        hasNoBreaks flat andalso hasNoBreaks notflat
    | _ => true

  fun cond tab {flat, notflat} =
    Cond {tab=tab, flat=flat, notflat=notflat}

  fun concat (d1, d2) =
    case (d1, d2) of
      (Empty, _) => d2
    | (_, Empty) => d1
    | _ => Concat (d1, d2)
  
  fun newTab parent genDocUsingTab =
    let
      val t = Tab.make parent
      val d = genDocUsingTab t
    in
      NewTab {parent = parent, tab = t, doc = d}
    end

  (* ====================================================================== *)


  fun ifActivatedHasAtLeastOneBreak tab doc =
    case doc of
      Empty => false
    | Space => false
    | Concat (d1, d2) =>
        ifActivatedHasAtLeastOneBreak tab d1
        orelse ifActivatedHasAtLeastOneBreak tab d2
    | Text _ => false
    | Break tab' => Tab.eq (tab, tab')
    | NewTab {doc=d, ...} => ifActivatedHasAtLeastOneBreak tab d
    | Cond {tab=tab', notflat, flat} =>
        if Tab.eq (tab, tab') orelse Tab.isActivated tab' then
          ifActivatedHasAtLeastOneBreak tab notflat
        else
          ifActivatedHasAtLeastOneBreak tab flat
        

  fun allOuterBreaksActivated tab doc =
    let
      fun isInList xs x =
        List.exists (fn y => Tab.eq (x, y)) xs

      fun loop innerTabs doc =
        case doc of
          Empty => true
        | Space => true
        | Concat (d1, d2) =>
            loop innerTabs d1
            andalso loop innerTabs d2
        | Text _ => true
        | Break tab' => isInList innerTabs tab' orelse Tab.isActivated tab'
        | NewTab {tab=tab', doc=d, ...} => loop (tab' :: innerTabs) d
        | Cond {notflat, ...} => loop innerTabs notflat
    in
      loop [tab] doc
    end
  

  (* A tab can be activated if
   *   (1) it has at least one break, and
   *   (2) every ancestor break within scope is activated
   *
   * This function should only be called on a NewTab{tab,doc}
   *)

  fun activationOkay debug tab doc =
    let
      val x = ifActivatedHasAtLeastOneBreak tab doc
      val y = allOuterBreaksActivated tab doc
      val result = x andalso y

      val _ =
        if not debug then ()
        else if result then
          print ("PrettyTabbedDoc.debug: tab " ^ Tab.infoString tab ^ " can be activated\n")
        else 
          print ("PrettyTabbedDoc.debug: tab "
            ^ Tab.infoString tab
            ^ " CANNOT be activated: "
            ^ (if x then "has breaks" else "no breaks") ^ "; "
            ^ (if y then "all outer activated" else "outer inactive")
            ^ "\n")
    in
      result
    end



  (* fun activationOkay debug tab doc =
    allOuterBreaksActivated tab doc *)
  
  (* ====================================================================== *)

  fun allTabsInDoc d =
    let
      fun loop acc d =
        case d of
          NewTab {tab, doc, ...} => loop (tab :: acc) doc
        | Concat (d1, d2) => loop (loop acc d1) d2
        | Cond {flat, notflat, ...} => loop (loop acc flat) notflat
        | _ => acc
    in
      loop [] d
    end

  (* ====================================================================== *)


  fun spaces count =
    CustomString.fromString (CharVector.tabulate (count, fn _ => #" "))


  datatype item =
    Spaces of int
  | Newline
  | Stuff of CustomString.t
  | StartDebug of {tab: tab, col: int}
  | EndDebug of {tab: tab, info: CustomString.t, col: int}


  fun itemWidth item =
    case item of
      Spaces n => n
    | Stuff s => CustomString.size s
    | _ => raise Fail "PrettyTabbedDoc.itemWidth"


  fun itos item =
    case item of
      Spaces n => "Spaces(" ^ Int.toString n ^ ")"
    | Stuff s =>
        if itemWidth item <= 5 then
          "Stuff('" ^ CustomString.toString s ^ "')"
        else
          "Stuff('" ^ String.substring (CustomString.toString s, 0, 5) ^ "...')"
    | _ => "???"


  fun splitItem item i =
    if i < 0 orelse i+1 > itemWidth item then
      raise Fail "PrettyTabbedDoc.splitItem: size"
    else
    (* i+1 <= itemWidth item *)
    case item of  
      Spaces n =>
        (Spaces i, CustomString.fromString " ", Spaces (n-i-1))
    | Stuff s =>
        let
          val n = CustomString.size s
          val left = CustomString.substring (s, 0, i)
          val mid = CustomString.substring (s, i, 1)
          val right = CustomString.substring (s, i+1, n-i-1)
        in
          (Stuff left, mid, Stuff right)
        end
    | _ => raise Fail "PrettyTabbedDoc.splitItem: bad item"


  (* ====================================================================== *)
  (* ====================================================================== *)
  (* ====================================================================== *)


  fun implementDebugs items =
    let
      type sentry = {tab: tab, col: int}
      type eentry = {tab: tab, info: CustomString.t, col: int}

      fun sentryCmp ({tab=tab1, col=col1}, {tab=tab2, col=col2}) =
        case Int.compare (col1, col2) of
          EQUAL => Tab.compare (tab1, tab2)
        | other => other

      fun eentryCmp ({tab=tab1, col=col1, info=_}, {tab=tab2, col=col2, info=_}) =
        case Int.compare (col1, col2) of
          EQUAL => Tab.compare (tab1, tab2)
        | other => other

      fun highlightActive accCurrLine acc startDebugs =
        let
          val orderedHighlightCols =
            Mergesort.sort sentryCmp (Seq.fromList startDebugs)

          fun processItem (item, (currCol, hi, acc)) =
            let
              val () = ()
              (* val _ = print ("processItem " ^ itos item ^ "\n") *)
              val nextHighlightCol =
                if hi < Seq.length orderedHighlightCols then
                  #col (Seq.nth orderedHighlightCols hi)
                else
                  valOf Int.maxInt
              
              val n = itemWidth item
            in
              if nextHighlightCol < currCol then
                processItem (item, (currCol, hi+1, acc))
              else if currCol+n <= nextHighlightCol then
                (currCol+n, hi, item :: acc)
              else
                let
                  val tabDepth = Tab.depth (#tab (Seq.nth orderedHighlightCols hi))
                  val (left, mid, right) = splitItem item (nextHighlightCol-currCol)
                  (* 
                  val _ =
                    print ("item: " ^ itos item
                       ^ " split into (" ^ itos left ^ ", _, " ^ itos right ^ ")"
                       ^ " nextHightlightCol: " ^ Int.toString nextHighlightCol
                       ^ " currCol: " ^ Int.toString currCol
                       ^ " itemWidth: " ^ Int.toString n
                       ^ " hi: " ^ Int.toString hi
                       ^ "\n")
                  *)
                in
                  processItem (right,
                   ( nextHighlightCol + 1
                   , hi+1
                   , Stuff (CustomString.emphasize tabDepth mid)
                     :: left :: acc
                   ))
                end
            end

          val (_, _, acc) = List.foldr processItem (0, 0, acc) accCurrLine
        in
          acc
        end


      fun newlineWithEndDebugs endDebugs startDebugs acc =
        if List.null endDebugs then
          (startDebugs, Newline :: acc)
        else
        let
          val orderedStarts =
            Mergesort.sort sentryCmp (Seq.fromList startDebugs)
          val orderedEnds =
            Mergesort.sort eentryCmp (Seq.fromList endDebugs)

          (* This is a bit cumbersome, but actually is fairly straightforward:
           * for each `(info, col)` in `EE`, output `info` at column `col`.
           *
           * There's some trickiness though, because multiple `(info, col)`
           * entries might overlap. For this, we check if each entry fits,
           * and if not, we add the entry to `didntFit`, and then process
           * `didntFit` on the next line, repeating until all entries have been
           * output.
           *
           * Update: and now there's more trickiness, because we need to filter
           * starts as we go to get decent output...
           *)
          fun loop
                (i, SS: sentry Seq.t)
                (j, EE: eentry Seq.t)
                (didntFitEE: eentry list)
                ( removedSSCurrLine: sentry list
                , remainingSS: sentry list
                )
                (currCol: int)
                (accCurrLine: item list)
                (acc: item list)
            =
            if j >= Seq.length EE then
              if List.null didntFitEE then
                let
                  val remainingSS' = Seq.toList (Seq.drop SS i) @ remainingSS
                in
                  ( remainingSS'
                  , highlightActive accCurrLine acc (removedSSCurrLine @ remainingSS')
                  )
                end
              else
                loop
                  (0, Seq.append (Seq.fromRevList remainingSS, Seq.drop SS i))
                  (0, Seq.fromRevList didntFitEE)
                  []        (* didntFitEE *)
                  ([], [])  (* (removedSSCurrLine, remainingSS) *)
                  0         (* currCol *)
                  []        (* accCurrLine *)
                  (Newline :: highlightActive accCurrLine acc
                    (Seq.toList (Seq.drop SS i) @ remainingSS @ removedSSCurrLine))
            else
            let
              val sentry as {tab=st, col=scol} = Seq.nth SS i
              val eentry as {tab=et, info, col=ecol} = Seq.nth EE j

              fun sentrytos {tab=st, col=scol} =
                "{st = " ^ Tab.name st ^ ", scol = " ^ Int.toString scol ^ "}"
              fun eentrytos {tab=et, info, col=ecol} =
                "{et = " ^ Tab.name st ^ ", ecol = " ^ Int.toString scol ^ "}"
              
              val _ =
                (* check invariant *)
                if scol < ecol orelse (scol = ecol andalso Tab.eq (st, et)) then ()
                else 
                  ( print ("sentry " ^ sentrytos sentry ^ "\n"
                         ^ "eentry " ^ eentrytos eentry ^ "\n"
                         ^ "i " ^ Int.toString i ^ "\n"
                         ^ "j " ^ Int.toString j ^ "\n"
                         ^ "SS " ^ Seq.toString sentrytos SS ^ "\n"
                         ^ "EE " ^ Seq.toString eentrytos EE ^ "\n")
                  ; raise Fail "newlineWithEndDebugs.loop: invariant violated"
                  )
            in
              if scol < ecol then
                loop (i+1, SS) (j, EE) didntFitEE (removedSSCurrLine, sentry :: remainingSS) currCol accCurrLine acc
              else
              (* invariant: scol = ecol andalso Tab.eq (st, et) *)
              if ecol < currCol then
                loop (i+1, SS) (j+1, EE) (eentry :: didntFitEE) (removedSSCurrLine, sentry :: remainingSS) currCol accCurrLine acc
              else
              let
                val numSpaces = ecol - currCol
                val newCol = currCol + numSpaces + CustomString.size info
              in
                loop (i+1, SS) (j+1, EE) didntFitEE (sentry :: removedSSCurrLine, remainingSS) newCol (Stuff info :: Spaces numSpaces :: accCurrLine) acc
              end
            end
          
          val (remainingSS, acc) =
            loop (0, orderedStarts) (0, orderedEnds) [] ([], []) 0 [] (Newline :: acc)
        in
          (remainingSS, Newline :: acc)
        end


      fun processItem (item, (accCurrLine, acc, endDebugs, startDebugs)) =
        case item of
          EndDebug entry => (accCurrLine, acc, entry :: endDebugs, startDebugs)
        | StartDebug entry => (accCurrLine, acc, endDebugs, entry :: startDebugs)
        | Newline =>
            let
              val (remainingSS, acc) =
                newlineWithEndDebugs endDebugs startDebugs
                  (highlightActive accCurrLine acc startDebugs)
            in
              ([], acc, [], remainingSS)
            end
        | _ => (item :: accCurrLine, acc, endDebugs, startDebugs)


      val (accCurrLine, acc, endDebugs, startDebugs) =
        List.foldr processItem ([], [], [], []) items
    in
      if List.null endDebugs then
        accCurrLine @ acc
      else
        #2 (newlineWithEndDebugs endDebugs startDebugs
              (highlightActive accCurrLine acc startDebugs))
    end


  (* ====================================================================== *)
  (* ====================================================================== *)
  (* ====================================================================== *)


  fun revAndStripTrailingWhitespace (items: item list) =
    let
      fun loopStrip acc items =
        case items of
          [] => acc
        | Spaces _ :: items' =>
            loopStrip acc items'
        | x :: items' =>
            loopKeep (x :: acc) items'

      and loopKeep acc items =
        case items of
          [] => acc
        | Newline :: items' =>
            loopStrip (Newline :: acc) items'
        | x :: items' =>
            loopKeep (x :: acc) items'
    in
      loopStrip [] items
    end

  
  exception DoPromote of tab


  fun pretty {ribbonFrac, maxWidth, indentWidth, debug} doc =
    let
      val ribbonWidth =
        Int.max (0, Int.min (maxWidth,
          Real.round (ribbonFrac * Real.fromInt maxWidth)))

      val newline = CustomString.fromString "\n"
      val sp = CustomString.fromString " "

      val allTabs = allTabsInDoc doc
      val _ =
        if List.all (fn t => Tab.getState t = Tab.Fresh) allTabs then ()
        else raise Fail "PrettyTabbedDoc.pretty: bug: non-fresh input tab"

      (* initially, all tabs inactive, and their placement is unknown *)
      val _ = List.app (fn t => Tab.setState t (Tab.Usable Tab.Flattened)) allTabs

      (* tab -> hit first break? *)
      type debug_state = bool TabDict.t 

      (* debug state, current tab, line start, current col, accumulator *)
      type layout_state = debug_state * tab * int * int * (item list)

      fun dbgInsert tab ((dbgState, ct, s, c, a): layout_state) : layout_state =
        if not debug then (dbgState, ct, s, c, a) else
        ( TabDict.insert dbgState (tab, false)
        , ct, s, c, a
        )

      fun dbgBreak tab ((dbgState, ct, s, c, a): layout_state) : layout_state =
        if not debug then
          (dbgState, ct, s, c, a)
        else if TabDict.lookup dbgState tab then
          (dbgState, ct, s, c, a)
        else
          ( TabDict.insert dbgState (tab, true)
          , ct, s, c
          , StartDebug {tab = tab, col = c} :: a
          )

      fun isPromotable' t =
        case Tab.getState t of
          Tab.Usable Tab.Flattened => true
        | Tab.Usable (Tab.Activated NONE) => true
        | Tab.Usable (Tab.Activated (SOME ti)) =>
            (case Tab.parent t of
              NONE => false
            | SOME p =>
                case Tab.getState p of
                  Tab.Usable (Tab.Activated (SOME pi)) =>
                    ti > pi + indentWidth
                | _ => raise Fail "PrettyTabbedDoc.pretty.isPromotable: bad parent tab")
        | _ => raise Fail "PrettyTabbedDoc.pretty.isPromotable: bad tab"

      
      fun isPromotable t =
        let
          val result = isPromotable' t
        in
          if not debug then () else
          print ("PrettyTabbedDoc.debug: isPromotable " ^ Tab.infoString t ^ " = " ^ (if result then "true" else "false") ^ "\n");
          result
        end


      fun oldestPromotableParent t =
        if not (isPromotable t) then NONE else
        case Tab.parent t of
          SOME p =>
            if not (isPromotable p) then
              SOME t
            else
              oldestPromotableParent p
        | NONE => SOME t
        

      fun check (dbgState, ct, lnStart, col, acc) =
        let
          val widthOkay = col <= maxWidth
          val ribbonOkay = (col - lnStart) <= ribbonWidth
        in
          if widthOkay andalso ribbonOkay then
            (dbgState, ct, lnStart, col, acc)
          else case oldestPromotableParent ct of
            NONE =>
              ( ()
              ; if not debug then ()
                else if not widthOkay then
                  print ("PrettyTabbedDoc.debug: width violated: ct=" ^ Tab.infoString ct ^ " lnStart=" ^ Int.toString lnStart ^ " col=" ^ Int.toString col ^ "\n")
                else if not ribbonOkay then
                  print ("PrettyTabbedDoc.debug: ribbon violated: ct=" ^ Tab.infoString ct ^ " lnStart=" ^ Int.toString lnStart ^ " col=" ^ Int.toString col ^ "\n")
                else
                  print ("PrettyTabbedDoc.debug: unknown violation?? ct=" ^ Tab.infoString ct ^ " lnStart=" ^ Int.toString lnStart ^ " col=" ^ Int.toString col ^ "\n")
              ; (dbgState, ct, lnStart, col, acc)
              )
          | SOME p =>
              ( ()
              ; if not debug then ()
                else if not widthOkay then
                  print ("PrettyTabbedDoc.debug: width violated: lnStart=" ^ Int.toString lnStart ^ " col=" ^ Int.toString col ^ "\n")
                else if not ribbonOkay then
                  print ("PrettyTabbedDoc.debug: ribbon violated: lnStart=" ^ Int.toString lnStart ^ " col=" ^ Int.toString col ^ "\n")
                else
                  print ("PrettyTabbedDoc.debug: unknown violation?? lnStart=" ^ Int.toString lnStart ^ " col=" ^ Int.toString col ^ "\n")
              ; raise DoPromote p
              )
        end

      
      (* This is a little tricky, but the idea is: try to lay out the doc,
       * and keep track of whether or not there exists an ancestor tab that
       * could be promoted (ap). If we ever violate either the width or
       * ribbon condition, then promote the oldest ancestor tab and try again.
       *
       * Promotion is implemented by throwing an exception (DoPromote), which
       * is caught by the oldest ancestor.
       *)
      fun layout ((dbgState, ct, lnStart, col, acc): layout_state) doc : layout_state =
        case doc of
          Empty => 
            (dbgState, ct, lnStart, col, acc)
        
        | Space =>
            check (dbgState, ct, lnStart, col + 1, Spaces 1 :: acc)

        | Text s =>
            check (dbgState, ct, lnStart, col + CustomString.size s, Stuff s :: acc)

        | Concat (doc1, doc2) =>
            layout (layout (dbgState, ct, lnStart, col, acc) doc1) doc2

        | Break tab =>
            let in
              case Tab.getState tab of
                Tab.Usable Tab.Flattened =>
                  (dbgState, tab, lnStart, col, acc)
              | Tab.Usable (Tab.Activated NONE) =>
                  ( Tab.setState tab (Tab.Usable (Tab.Activated (SOME col)))
                  ; dbgBreak tab (dbgState, tab, lnStart, col, acc)
                  )
              | Tab.Usable (Tab.Activated (SOME i)) =>
                  if i < col then
                    dbgBreak tab (check (dbgState, tab, i, i, Spaces i :: Newline :: acc))
                  else if i = col then
                    dbgBreak tab (dbgState, tab, i, i, acc)
                  else
                    (* force this tab to promote, which should move onto
                     * a new line and indent.
                     *
                     * an alternative here would be this:
                     *   check (tab, lnStart, i, Spaces (i-col) :: acc)
                     * which prefers to jump forward on the current line to
                     * meet the tab, but IMO this can look really strange in
                     * some situations
                     *)
                    raise DoPromote tab
                    
              | _ =>
                  raise Fail "PrettyTabbedDoc.pretty.layout.Break: bad tab"
            end

        | Cond {tab, flat, notflat} =>
            let in
              case Tab.getState tab of
                Tab.Usable (Tab.Activated _) =>
                  layout (dbgState, ct, lnStart, col, acc) notflat
              | Tab.Usable Tab.Flattened =>
                  layout (dbgState, ct, lnStart, col, acc) flat
              | _ =>
                  raise Fail "PrettyTabbedDoc.pretty.layout.Cond: bad tab"
            end

        | NewTab {parent, tab, doc} =>
            let
              fun parentTabCol () =
                case Tab.getState parent of
                  Tab.Usable (Tab.Activated (SOME i)) => i
                | _ => raise Fail "PrettyTabbedDoc.pretty.layout.NewTab.parentTabCol: bad tab"  

              fun tryPromote () =
                (* try to activate first *)
                if not (Tab.isActivated tab) then
                  Tab.setState tab (Tab.Usable (Tab.Activated NONE))
                  (*if activationOkay debug tab doc then
                    Tab.setState tab (Tab.Usable (Tab.Activated NONE))
                  else
                    ()*)
                else (* if activated, try to relocate *)
                case Tab.getState tab of
                  Tab.Usable (Tab.Activated NONE) =>
                    let
                      val desired = parentTabCol () + indentWidth
                    in
                      Tab.setState tab (Tab.Usable (Tab.Activated (SOME desired)))
                    end
                | Tab.Usable (Tab.Activated (SOME i)) =>
                    let
                      val desired = Int.min (i, parentTabCol () + indentWidth)
                    in
                      Tab.setState tab (Tab.Usable (Tab.Activated (SOME desired)))
                    end
                | _ =>
                    raise Fail "PrettyTabbedDoc.pretty.layout.NewTab.tryPromote: bad tab"

              fun doit () =
                let in
                  ( ()
                  ; (layout (dbgInsert tab (dbgState, ct, lnStart, col, acc)) doc
                      handle DoPromote p =>
                      if not (Tab.eq (p, tab)) then raise DoPromote p else
                      let
                        val _ = 
                          if not debug then () else
                          print ("PrettyTabbedDoc.debug: promoting " ^ Tab.infoString tab ^ "\n")
                      in
                        tryPromote ();
                        doit ()
                      end)
                  )
                end
            
              val _ = Tab.setState tab (Tab.Usable Tab.Flattened)

              val (dbgState, _, lnStart, col, acc) : layout_state =
                doit ()

              val acc =
                if not debug then acc else
                case Tab.getState tab of
                  Tab.Usable Tab.Flattened => acc
                | Tab.Usable (Tab.Activated (SOME i)) =>
                    EndDebug
                      { tab = tab
                      , info = CustomString.emphasize (Tab.depth tab) (CustomString.fromString ("^" ^ Tab.name tab))
                      , col = i
                      } :: acc
                | _ => raise Fail "PrettyTabbedDoc.debug: error..."
            in
              if not debug then () else
              print ("PrettyTabbedDoc.debug: finishing " ^ Tab.infoString tab ^ "\n");

              Tab.setState tab Tab.Completed;

              (dbgState, parent, lnStart, col, acc)
            end


      val (_, _, _, _, items) = layout (TabDict.empty, root, 0, 0, []) doc

      val items = if not debug then items else implementDebugs items

      (* reset tabs (so that if we call `pretty` again, it will work...) *)
      val _ = List.app (fn tab => Tab.setState tab Tab.Fresh) allTabs

      val items = revAndStripTrailingWhitespace items

      fun itemToString x =
        case x of
          Newline => newline
        | Spaces n => spaces n
        | Stuff s => s
        | _ => raise Fail "impossible"
    in
      CustomString.concat (List.map itemToString items)
    end


  val toString = pretty {ribbonFrac = 0.5, maxWidth = 80, indentWidth = 2, debug = false}

end