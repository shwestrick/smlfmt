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
  val newTab: ('a tab -> 'a doc) -> 'a doc
  val break: 'a tab -> 'a doc

  val toStringDoc: TabbedStringDoc.tab doc -> TabbedStringDoc.t
end =
struct

  (* Just need a unique name *)
  type 'a tab = 'a option ref

  datatype 'a doc =
    Empty
  | Space
  | Concat of 'a doc * 'a doc
  | Token of Token.t
  | Text of string
  | Break of 'a tab
  | NewTab of {tab: 'a tab, doc: 'a doc}

  type 'a t = 'a doc

  val empty = Empty
  val space = Space
  val token = Token
  val text = Text
  val concat = Concat
  val break = Break

  fun newTab (genDocUsingTab: 'a tab -> 'a doc) =
    let
      val t = ref (NONE: 'a option)
      val d = genDocUsingTab t
    in
      NewTab {tab=t, doc=d}
    end


  fun toStringDoc doc =
    case doc of
      Empty => TabbedStringDoc.empty
    | Space => TabbedStringDoc.space
    | Concat (d1, d2) => TabbedStringDoc.concat (toStringDoc d1, toStringDoc d2)
    | Text str => TabbedStringDoc.text (TerminalColorString.fromString str)
    | Token tok => TabbedStringDoc.text (SyntaxHighlighter.highlightToken tok)
    | Break tab => TabbedStringDoc.break (Option.valOf (!tab))
    | NewTab {tab, doc} =>
        TabbedStringDoc.newTab (fn tab' =>
          ( tab := SOME tab'
          ; toStringDoc doc
          ))

end