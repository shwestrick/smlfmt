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

  val empty: doc
  val space: doc
  val text: CustomString.t -> doc
  val concat: doc * doc -> doc

  type tab
  val newTab: (tab -> doc) -> doc
  val breakspace: tab -> doc
  val break: tab -> doc  (* TODO: might need more general version which
                          * optionally inserts something if the tab is/isn't
                          * activated? Needs a constraint though, that
                          * the unactivated version is "smaller"
                          *)
  val spaceIfNotFlat: tab -> doc

  (* "under the hood" version of newTab. On input (t, d), requires that tab t
   * is used in d and nowhere else. *)
  (* val newTab': tab * doc -> doc *)

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



  structure Tab =
  struct
    datatype state =
      Fresh
    | Flattened
    | ActivatedInPlace of int
    | ActivatedIndented of int
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

    fun getState (Tab (r, _)) = !r
    fun setState (Tab (r, _)) x = r := x

    fun infoString (Tab (r, c)) =
      let
        val info =
          case !r of
            Flattened => "f"
          | ActivatedInPlace _ => "p"
          | ActivatedIndented _ => "i"
          | _ => "?"
      in
        "[" ^ Int.toString c ^ info ^ "]"
      end

  end

  type tab = Tab.t

  datatype doc =
    Empty
  | Space
  | SpaceIfNotFlat of tab
  | Concat of doc * doc
  | Text of CustomString.t
  | Break of {keepSpace: bool, tab: tab}
  | NewTab of {tab: tab, doc: doc(*, flatsize: int*)}

  type t = doc

  val empty = Empty
  val space = Space
  val text = Text
  val break = Break
  fun break t = Break {keepSpace=false, tab=t}
  fun breakspace t = Break {keepSpace=true, tab=t}
  val spaceIfNotFlat = SpaceIfNotFlat
  (* fun newTab' (t, d) = NewTab {tab=t, doc=d} *)

  fun concat (d1, d2) =
    case (d1, d2) of
      (Empty, _) => d2
    | (_, Empty) => d1
    | _ => Concat (d1, d2)

(*
  fun flatsize doc =
    case doc of
      Empty => 0
    | Space => 1
    | Concat (d1, d2) => flatsize d1 + flatsize d2
    | Text s => CustomString.size s
    | Break _ => 0
    | NewTab {flatsize=sz, ...} => sz
*)

  
  fun newTab genDocUsingTab =
    let
      val t = Tab.make ()
      val d = genDocUsingTab t
    in
      NewTab {tab = t, doc = d(*, flatsize = flatsize d*)}
    end

  
  (* ====================================================================== *)

  fun allTabsInDoc d =
    let
      fun loop acc d =
        case d of
          NewTab {tab, doc, ...} => loop (tab :: acc) doc
        | Concat (d1, d2) => loop (loop acc d1) d2
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
      fun layout ((ap, lnStart, col, acc): layout_state) doc : layout_state =
        case doc of
          Empty => 
            (ap, lnStart, col, acc)
        
        | Space =>
            check (ap, lnStart, col + 1, Spaces 1 :: acc)

        | SpaceIfNotFlat tab =>
            let in
              case Tab.getState tab of
                Tab.Flattened =>
                  (ap, lnStart, col, acc)
              | _ =>
                  check (ap, lnStart, col + 1, Spaces 1 :: acc)
            end

        | Text s =>
            check (ap, lnStart, col + CustomString.size s, Stuff s :: acc)

        | Concat (doc1, doc2) =>
            layout (layout (ap, lnStart, col, acc) doc1) doc2

        | Break {keepSpace, tab} =>
            let in
              case Tab.getState tab of
                Tab.Flattened =>
                  if not keepSpace then
                    addDebugOutput tab (ap, lnStart, col, acc)
                  else
                    addDebugOutput tab (check (ap, lnStart, col+1, Spaces 1 :: acc))
              | Tab.ActivatedInPlace i =>
                  addDebugOutput tab (check (ap, i, i, Spaces i :: Newline :: acc))
              | Tab.ActivatedIndented i =>
                  addDebugOutput tab (check (ap, i, i, Spaces i :: Newline :: acc))
              | _ =>
                  raise Fail "PrettyTabbedDoc.pretty.layout.Break: bad tab"
            end

        | NewTab {tab, doc} =>
            let
              fun tryPromote () =
                case Tab.getState tab of
                  Tab.Fresh =>
                    ( Tab.setState tab Tab.Flattened
                    ; (true, (lnStart, col, acc))
                    )
                | Tab.Flattened =>
                    ( Tab.setState tab (Tab.ActivatedInPlace col)
                    ; (true, (lnStart, col, acc))
                    )
                | Tab.ActivatedInPlace _ =>
                    if col <= lnStart + indentWidth then
                      ( Tab.setState tab (Tab.ActivatedIndented lnStart)
                      ; (true, (lnStart, col, acc))
                      )
                    else
                      let
                        val i = lnStart + indentWidth
                      in
                        Tab.setState tab (Tab.ActivatedIndented i);
                        (true, (i, i, Spaces i :: Newline :: acc))
                      end
                | Tab.ActivatedIndented i =>
                    ( false
                    , if col <= lnStart + indentWidth then
                        (lnStart, col, acc)
                      else
                        (i, i, Spaces i :: Newline :: acc)
                    )
                | _ =>
                    raise Fail "PrettyTabbedDoc.pretty.layout.NewTab.tryPromote: bad tab"

              fun doit () =
                case tryPromote () of
                  (false, (lnStart, col, acc)) =>
                    layout (addDebugOutput tab (false, lnStart, col, acc)) doc
                | (true, (lnStart, col, acc)) => 
                    (layout (addDebugOutput tab (true, lnStart, col, acc)) doc
                     handle DoPromote => 
                       ( ()
                       ; if not debug then () else
                         print ("PrettyTabbedDoc.debug: promoting " ^ Tab.infoString tab ^ "\n")
                       ; doit ()
                       ))

              val _ = Tab.setState tab Tab.Fresh

              val (_, lnStart', col', acc') : layout_state =
                if ap then
                  ( Tab.setState tab Tab.Flattened
                  ; layout (addDebugOutput tab (ap, lnStart, col, acc)) doc
                  )
                else
                  doit ()
            in
              Tab.setState tab Tab.Completed;
              (ap, lnStart', col', acc')
            end


      val (_, _, _, items) = layout (false, 0, 0, []) doc

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