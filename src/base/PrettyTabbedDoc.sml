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
      val fromString: string -> t
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
  val newTab: (tab -> doc) -> doc
  val break: tab -> doc

  (* Requires flat doc has no breaks; raises InvalidDoc otherwise *)
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

    datatype tab = Tab of state ref * int
    type t = tab

    val tabCounter = ref 0

    fun make () =
      let
        val c = !tabCounter
      in
        tabCounter := c+1;
        Tab (ref Fresh, c)
      end

    fun eq (Tab (_, c1), Tab (_, c2)) =
      c1 = c2

    fun getState (Tab (r, _)) = !r
    fun setState (Tab (r, _)) x = r := x

    fun isActivated (Tab (r, _)) =
      case !r of
        Usable (Activated _) => true
      | Usable (Flattened) => false
      | _ => raise Fail "PrettyTabbedDoc.Tab.isActivated: bad tab"

    fun infoString (Tab (r, c)) =
      let
        val info =
          case !r of
            Usable Flattened => "f"
          | Usable (Activated NONE) => "a?"
          | Usable (Activated (SOME _)) => "a"
          | _ => "x"
      in
        "[" ^ Int.toString c ^ info ^ "]"
      end

  end

  (* ====================================================================== *)

  exception InvalidDoc

  type tab = Tab.t

  datatype doc =
    Empty
  | Space
  | Concat of doc * doc
  | Text of CustomString.t
  | Break of tab
  | NewTab of {tab: tab, doc: doc}
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
        (* TODO: check: can omit `hasNoBreaks flat`, because previous `cond`
         * will have checked this.
         *)
        hasNoBreaks flat andalso hasNoBreaks notflat
    | _ => true

  fun cond tab {flat, notflat} =
    (* if hasNoBreaks flat then *)
      Cond {tab=tab, flat=flat, notflat=notflat}
    (* else *)
      (* raise InvalidDoc *)

  fun concat (d1, d2) =
    case (d1, d2) of
      (Empty, _) => d2
    | (_, Empty) => d1
    | _ => Concat (d1, d2)
  
  fun newTab genDocUsingTab =
    let
      val t = Tab.make ()
      val d = genDocUsingTab t
    in
      NewTab {tab = t, doc = d}
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
    | Cond {notflat, ...} =>
        ifActivatedHasAtLeastOneBreak tab notflat
        

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

  
  exception DoPromote


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

      (* inside promotable tab?, line start, current col, accumulator *)
      type layout_state = bool * int * int * (item list)

      fun check (ap, lnStart, col, acc) =
        let
          val widthOkay = col <= maxWidth
          val ribbonOkay = (col - lnStart) <= ribbonWidth
        in
          if (not ap) orelse (widthOkay andalso ribbonOkay) then
            (ap, lnStart, col, acc)
          else
            ( ()
            ; if not debug then ()
              else if not widthOkay then
                print ("PrettyTabbedDoc.debug: width violated: lnStart=" ^ Int.toString lnStart ^ " col=" ^ Int.toString col ^ "\n")
              else if not ribbonOkay then
                print ("PrettyTabbedDoc.debug: ribbon violated: lnStart=" ^ Int.toString lnStart ^ " col=" ^ Int.toString col ^ "\n")
              else
                print ("PrettyTabbedDoc.debug: unknown violation?? lnStart=" ^ Int.toString lnStart ^ " col=" ^ Int.toString col ^ "\n")
            ; raise DoPromote
            )
        end


      fun addDebugOutput tab (ap, lnStart, col, acc) =
        let
          val acc' =
            if not debug then
              acc
            else
              Stuff (CustomString.fromString (Tab.infoString tab))
              :: acc
        in
          (ap, lnStart, col, acc')
        end

      
      (* This is a little tricky, but the idea is: try to lay out the doc,
       * and keep track of whether or not there exists an ancestor tab that
       * could be promoted (ap). If we ever violate either the width or
       * ribbon condition, then promote the oldest ancestor tab and try again.
       *
       * Promotion is implemented by throwing an exception (DoPromote), which
       * is caught by the oldest ancestor.
       *)
      fun layout tabCtx ((ap, lnStart, col, acc): layout_state) doc : layout_state =
        case doc of
          Empty => 
            (ap, lnStart, col, acc)
        
        | Space =>
            check (ap, lnStart, col + 1, Spaces 1 :: acc)

        | Text s =>
            check (ap, lnStart, col + CustomString.size s, Stuff s :: acc)

        | Concat (doc1, doc2) =>
            layout tabCtx (layout tabCtx (ap, lnStart, col, acc) doc1) doc2

        | Break tab =>
            let in
              case Tab.getState tab of
                Tab.Usable Tab.Flattened =>
                  addDebugOutput tab (ap, lnStart, col, acc)
              | Tab.Usable (Tab.Activated NONE) =>
                  ( Tab.setState tab (Tab.Usable (Tab.Activated (SOME col)))
                  ; addDebugOutput tab (ap, lnStart, col, acc)
                  )
              | Tab.Usable (Tab.Activated (SOME i)) =>
                  if i < col then
                    addDebugOutput tab (check (ap, i, i, Spaces i :: Newline :: acc))
                  else
                    addDebugOutput tab (check (ap, lnStart, i, Spaces (i-col) :: acc))
              | _ =>
                  raise Fail "PrettyTabbedDoc.pretty.layout.Break: bad tab"
            end

        | Cond {tab, flat, notflat} =>
            let in
              case Tab.getState tab of
                Tab.Usable (Tab.Activated NONE) =>
                  raise Fail "PrettyTabbedDoc.pretty.layout.Cond: tab activated but not placed"
              | Tab.Usable (Tab.Activated (SOME _)) =>
                  layout tabCtx (ap, lnStart, col, acc) notflat
              | Tab.Usable Tab.Flattened =>
                  layout tabCtx (ap, lnStart, col, acc) flat
              | _ =>
                  raise Fail "PrettyTabbedDoc.pretty.layout.Cond: bad tab"
            end

        | NewTab {tab, doc} =>
            let
              fun parentTabCol () =
                case tabCtx of
                  [] => 0
                | parent :: _ =>
                case Tab.getState parent of
                  Tab.Usable (Tab.Activated (SOME i)) => i
                | _ => raise Fail "PrettyTabbedDoc.pretty.layout.NewTab.parentTabCol: bad tab"  

              fun tryPromote () =
                (* try to activate first *)
                if not (Tab.isActivated tab) then
                  if activationOkay debug tab doc then
                    ( Tab.setState tab (Tab.Usable (Tab.Activated NONE))
                    ; (true, true)
                    )
                  else
                    (false, false)
                else (* if activated, try to relocate *)
                case Tab.getState tab of
                  Tab.Usable (Tab.Activated NONE) =>
                    let
                      val desired = parentTabCol () + indentWidth
                    in
                      Tab.setState tab (Tab.Usable (Tab.Activated (SOME desired)));
                      (false, true)
                    end
                | Tab.Usable (Tab.Activated (SOME i)) =>
                    let
                      val desired = Int.min (i, parentTabCol () + indentWidth)
                    in
                      Tab.setState tab (Tab.Usable (Tab.Activated (SOME desired)));
                      (false, true)
                    end
                | _ =>
                    raise Fail "PrettyTabbedDoc.pretty.layout.NewTab.tryPromote: bad tab"

              fun doit thisTabActive =
                ( ()
                ; if not debug then () else
                  print ("PrettyTabbedDoc.debug: attempting promotable " ^ Tab.infoString tab ^ "\n")
                ; (layout
                     (if thisTabActive then tab :: tabCtx else tabCtx)
                     (true, lnStart, col, acc)
                     doc
                    handle DoPromote => 
                    let
                      val _ = 
                        if not debug then () else
                        print ("PrettyTabbedDoc.debug: promoting " ^ Tab.infoString tab ^ "\n")
                      
                      val (promotable, thisTabActive') = tryPromote ()
                    in
                      if promotable then
                        doit thisTabActive'
                      else
                        layout
                          (if thisTabActive' then tab :: tabCtx else tabCtx)
                          (false, lnStart, col, acc)
                          doc
                    end)
                )
            
              val _ = Tab.setState tab (Tab.Usable Tab.Flattened)

              val (_, lnStart', col', acc') : layout_state =
                if ap then
                  layout tabCtx (true, lnStart, col, acc) doc
                else
                  doit false
            in
              if not debug then () else
              print ("PrettyTabbedDoc.debug: finishing " ^ Tab.infoString tab ^ "\n");

              Tab.setState tab Tab.Completed;

              (ap, lnStart', col', acc')
            end


      val (_, _, _, items) = layout [] (false, 0, 0, []) doc

      (* reset tabs (so that if we call `pretty` again, it will work...) *)
      val _ = List.app (fn tab => Tab.setState tab Tab.Fresh) allTabs

      val items = revAndStripTrailingWhitespace items

      fun itemToString x =
        case x of
          Newline => newline
        | Spaces n => spaces n
        | Stuff s => s
    in
      CustomString.concat (List.map itemToString items)
    end


  val toString = pretty {ribbonFrac = 0.5, maxWidth = 80, indentWidth = 2, debug = false}

end