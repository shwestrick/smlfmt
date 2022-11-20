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
  fun ensureSpacesBetweenTokens doc =
    let
      fun loop ctx doc =
    in
    end
  *)

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