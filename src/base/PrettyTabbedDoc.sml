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

  (* "under the hood" version of newTab. On input (t, d), requires that tab t
   * is used in d and nowhere else. *)
  (* val newTab': tab * doc -> doc *)

  val pretty: {ribbonFrac: real, maxWidth: int, indentWidth: int}
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

  datatype tabstate =
    Fresh
  | Flattened
  | ActivatedInPlace of int
  | ActivatedIndented of int
  | Completed

  type tab = tabstate ref


  datatype doc =
    Empty
  | Space
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
      val t = ref Fresh
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


  fun pretty {ribbonFrac, maxWidth, indentWidth} doc =
    let
      val ribbonWidth =
        Int.max (0, Int.min (maxWidth,
          Real.round (ribbonFrac * Real.fromInt maxWidth)))

      val newline = CustomString.fromString "\n"
      val sp = CustomString.fromString " "

      val allTabs = allTabsInDoc doc
      val _ =
        if List.all (fn t => !t = Fresh) allTabs then ()
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
            raise DoPromote
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

        | Text s =>
            check (ap, lnStart, col + CustomString.size s, Stuff s :: acc)

        | Concat (doc1, doc2) =>
            layout (layout (ap, lnStart, col, acc) doc1) doc2

        | Break {keepSpace, tab} =>
            (case !tab of
              Flattened =>
                if not keepSpace then
                  (ap, lnStart, col, acc)
                else
                  check (ap, lnStart, col+1, Spaces 1 :: acc)
            | ActivatedInPlace i =>
                check (ap, i, i, Spaces i :: Newline :: acc)
            | ActivatedIndented i =>
                check (ap, i, i, Spaces i :: Newline :: acc)
            | _ =>
                raise Fail "PrettyTabbedDoc.pretty: bad tab")

        | NewTab {tab, doc} =>
            let
              fun tryPromote () =
                case !tab of
                  Flattened =>
                    ( tab := ActivatedInPlace col
                    ; SOME (lnStart, col, acc)
                    )
                | ActivatedInPlace _ =>
                    let
                      val i = lnStart + indentWidth
                    in
                      tab := ActivatedIndented i;
                      SOME (i, i, Spaces i :: Newline :: acc)
                    end
                | Fresh =>
                    raise Fail "PrettyTabbedDoc.pretty.NewTab.tryPromote: bug: fresh"
                | _ => NONE

              fun doit () =
                case tryPromote () of
                  NONE =>
                    layout (false, lnStart, col, acc) doc
                | SOME (lnStart, col, acc) => 
                    (layout (true, lnStart, col, acc) doc
                     handle DoPromote => doit ())

              val _ = tab := Flattened

              val result : layout_state =
                if ap then
                  layout (ap, lnStart, col, acc) doc
                else
                  doit ()
            in
              tab := Completed;
              result
            end


      val (_, _, _, items) = layout (false, 0, 0, []) doc

      (* reset tabs (so that if we call `pretty` again, it will work...) *)
      val _ = List.app (fn tab => tab := Fresh) allTabs

      val items = revAndStripTrailingWhitespace items

      fun itemToString x =
        case x of
          Newline => newline
        | Spaces n => spaces n
        | Stuff s => s
    in
      CustomString.concat (List.map itemToString items)
    end


  val toString = pretty {ribbonFrac = 0.5, maxWidth = 80, indentWidth = 2}

end