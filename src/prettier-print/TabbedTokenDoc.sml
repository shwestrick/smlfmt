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
  val newChildTab: 'a tab -> ('a tab -> 'a doc) -> 'a doc
  val newTab: ('a tab -> 'a doc) -> 'a doc
  val break: 'a tab -> 'a doc
  val breakspace: 'a tab -> 'a doc
  val spaceIfNotFlat: 'a tab -> 'a doc
  val cond: 'a tab -> {flat: 'a doc, notflat: 'a doc} -> 'a doc

  val toStringDoc: {tabWidth: int} -> TabbedStringDoc.tab doc -> TabbedStringDoc.t
end =
struct

  (* Just need a unique name *)
  type 'a tab = 'a option ref * int

  val tabCounter = ref 0

  fun mkTab () =
    let
      val c = !tabCounter
    in
      tabCounter := c+1;
      (ref NONE, c)
    end

  fun tabToString ((_, c): 'a tab) = "[" ^ Int.toString c ^ "]"


  datatype 'a doc =
    Empty
  | Space
  | Concat of 'a doc * 'a doc
  | Token of Token.t
  | Text of string
  | Break of 'a tab
  | NewTab of {tab: 'a tab, doc: 'a doc}
  | NewChildTab of {parent: 'a tab, tab: 'a tab, doc: 'a doc}
  | Cond of {tab: 'a tab, flat: 'a doc, notflat: 'a doc}

  type 'a t = 'a doc

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


  fun newTab (genDocUsingTab: 'a tab -> 'a doc) =
    let
      val t = mkTab ()
      val d = genDocUsingTab t
    in
      NewTab {tab=t, doc=d}
    end

  
  fun newChildTab parent (genDocUsingTab: 'a tab -> 'a doc) =
    let
      val t = mkTab ()
      val d = genDocUsingTab t
    in
      NewChildTab {parent=parent, tab=t, doc=d}
    end

  (* ====================================================================== *)

  (* TODO: Use this to automatically insert spaces, rather than inserting
   * manually in this interface. *)
  fun ensureSpacesBetweenTokens doc = raise Fail "not yet implemented"
  
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
              (Seq.map (fn x => D.concat (D.break tab, x)) pieces))
        )
    end


  (* ====================================================================== *)


  fun toStringDoc (args as {tabWidth}) doc =
    let
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
            D.break (Option.valOf (!tabref))
        | Cond {tab = (tabref, _), flat, notflat} =>
            D.cond (Option.valOf (!tabref))
              { flat = loop currentTab flat
              , notflat = loop currentTab notflat
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