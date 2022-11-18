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

  end

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

      (* current tab, line start, current col, accumulator *)
      type layout_state = tab * int * int * (item list)


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
        

      fun check (ct, lnStart, col, acc) =
        let
          val widthOkay = col <= maxWidth
          val ribbonOkay = (col - lnStart) <= ribbonWidth
        in
          if widthOkay andalso ribbonOkay then
            (ct, lnStart, col, acc)
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
              ; (ct, lnStart, col, acc)
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


      fun addDebugOutput tab (ct, lnStart, col, acc) =
        let
          val acc' =
            if not debug then
              acc
            else
              Stuff (CustomString.fromString (Tab.infoString tab))
              :: acc
        in
          (ct, lnStart, col, acc')
        end

      
      (* This is a little tricky, but the idea is: try to lay out the doc,
       * and keep track of whether or not there exists an ancestor tab that
       * could be promoted (ap). If we ever violate either the width or
       * ribbon condition, then promote the oldest ancestor tab and try again.
       *
       * Promotion is implemented by throwing an exception (DoPromote), which
       * is caught by the oldest ancestor.
       *)
      fun layout ((ct, lnStart, col, acc): layout_state) doc : layout_state =
        case doc of
          Empty => 
            (ct, lnStart, col, acc)
        
        | Space =>
            check (ct, lnStart, col + 1, Spaces 1 :: acc)

        | Text s =>
            check (ct, lnStart, col + CustomString.size s, Stuff s :: acc)

        | Concat (doc1, doc2) =>
            layout (layout (ct, lnStart, col, acc) doc1) doc2

        | Break tab =>
            let in
              case Tab.getState tab of
                Tab.Usable Tab.Flattened =>
                  addDebugOutput tab (tab, lnStart, col, acc)
              | Tab.Usable (Tab.Activated NONE) =>
                  ( Tab.setState tab (Tab.Usable (Tab.Activated (SOME col)))
                  ; addDebugOutput tab (tab, lnStart, col, acc)
                  )
              | Tab.Usable (Tab.Activated (SOME i)) =>
                  if i < col then
                    addDebugOutput tab (check (tab, i, i, Spaces i :: Newline :: acc))
                  else
                    addDebugOutput tab (check (tab, lnStart, i, Spaces (i-col) :: acc))
              | _ =>
                  raise Fail "PrettyTabbedDoc.pretty.layout.Break: bad tab"
            end

        | Cond {tab, flat, notflat} =>
            let in
              case Tab.getState tab of
                Tab.Usable (Tab.Activated _) =>
                  layout (ct, lnStart, col, acc) notflat
              | Tab.Usable Tab.Flattened =>
                  layout (ct, lnStart, col, acc) flat
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
                ( ()
                ; (layout (ct, lnStart, col, acc) doc
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
            
              val _ = Tab.setState tab (Tab.Usable Tab.Flattened)

              val (_, lnStart', col', acc') : layout_state =
                doit ()
            in
              if not debug then () else
              print ("PrettyTabbedDoc.debug: finishing " ^ Tab.infoString tab ^ "\n");

              Tab.setState tab Tab.Completed;

              (parent, lnStart', col', acc')
            end


      val (_, _, _, items) = layout (root, 0, 0, []) doc

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