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
  val depth: tab -> int

  val eq: tab * tab -> bool
  val compare: tab * tab -> order

  val minIndent: tab -> int
  val isInplace: tab -> bool
  val isRigid: tab -> bool

  val name: tab -> string
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

  fun name t =
    case t of
      Root => "root"
    | Tab {id=c, ...} => Int.toString c

  fun toString t =
    "[" ^ name t ^ "]"

  fun compare (t1: tab, t2: tab) : order =
    case (t1, t2) of
      (Root, Root) => EQUAL
    | (Tab t1, Tab t2) => Int.compare (#id t1, #id t2)
    | (Tab _, Root) => GREATER
    | (Root, Tab _) => LESS

  fun eq (t1, t2) =
    compare (t1, t2) = EQUAL

  fun depth t =
    case t of
      Root => 0
    | Tab {parent=p, ...} => 1 + depth p

  fun isRigid t =
    case style t of
      Style.RigidInplace => true
    | Style.RigidIndented _ => true
    | _ => false

  fun isInplace t =
    case style t of
      Style.RigidInplace => true
    | Style.Inplace => true
    | _ => false

  fun minIndent t =
    case style t of
      Style.Indented (SOME {minIndent=i}) => i
    | Style.RigidIndented (SOME {minIndent=i}) => i
    | _ => 0

end