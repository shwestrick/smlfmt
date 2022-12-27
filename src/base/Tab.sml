(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure Tab:
sig

  structure Style:
  sig
    datatype style =
      Inplace
    | Indented of {minIndent: int} option
    | RigidInplace
    | RigidIndented of {minIndent: int} option

    type t = style
  end

  type tab
  type t = tab

  val root: tab
  val new: {parent: tab, style: Style.t} -> tab

  val parent: tab -> tab option
  val style: tab -> Style.t
  val compare: tab * tab -> order

  val toString: tab -> string

end =
struct

  (* ===================================================================== *)

  structure Style =
  struct
    datatype style =
      Inplace
    | Indented of {minIndent: int} option
    | RigidInplace
    | RigidIndented of {minIndent: int} option

    type t = style
  end

  (* ===================================================================== *)


  datatype tab =
    Tab of {id: int, style: Style.t, parent: tab}
  | Root

  type t = tab

  val tabCounter = ref 0

  fun new {parent, style} =
    let
      val c = !tabCounter
    in
      tabCounter := c+1;
      Tab {id = c, style = style, parent = parent}
    end

  val root = Root

  fun parent t =
    case t of
      Root => NONE
    | Tab {parent, ...} => SOME parent

  fun style t =
    case t of
      Root => Style.Inplace
    | Tab {style=s, ...} => s

  fun toString t =
    case t of
      Tab {id=c, ...} => "[" ^ Int.toString c ^ "]"
    | Root => "[root]"

  fun compare (t1: tab, t2: tab) : order =
    case (t1, t2) of
      (Root, Root) => EQUAL
    | (Tab t1, Tab t2) => Int.compare (#id t1, #id t2)
    | (Tab _, Root) => GREATER
    | (Root, Tab _) => LESS

end