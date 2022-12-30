(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure Tab:
sig

  structure Style:
  sig
    type style
    type t = style

    val inplace: style  (* default *)
    val indented: style
    val indentedAtLeastBy: int -> style
    val rigid: style

    val combine: style * style -> style

    val isRigid: style -> bool
    val minIndent: style -> int
    val isInplace: style -> bool
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

    datatype indent_style = Inplace | Indented of {minIndent: int} option

    fun combineIndentStyles (i1, i2) =
      case (i1, i2) of
        (Inplace, Inplace) => Inplace
      | (Inplace, Indented _) => i2
      | (Indented _, Inplace) => i1
      | (Indented mio1, Indented mio2) =>
          case (mio1, mio2) of
            (NONE, _) => i2
          | (_, NONE) => i1
          | (SOME {minIndent=mi1}, SOME {minIndent=mi2}) =>
              if mi1 >= mi2 then
                i1
              else
                i2

    datatype style =
      S of {indent: indent_style, rigid: bool}
    type t = style

    fun combine (S {indent=i1, rigid=r1}, S {indent=i2, rigid=r2}) =
      S { indent = combineIndentStyles (i1, i2)
        , rigid = r1 orelse r2
        }

    val inplace =
      S {indent = Inplace, rigid = false}
    val indented =
      S {indent = Indented NONE, rigid = false}
    fun indentedAtLeastBy i =
      S {indent = Indented (SOME {minIndent=i}), rigid = false}
    val rigid =
      S {indent = Inplace, rigid = true}

    fun isRigid (S {rigid, ...}) = rigid

    fun isInplace (S {indent, ...}) =
      case indent of
        Inplace => true
      | _ => false

    fun minIndent (S {indent, ...}) =
      case indent of
        Indented (SOME {minIndent=mi}) => mi
      | _ => 0
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
      Root => Style.inplace
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

  fun isRigid t = Style.isRigid (style t)
  fun isInplace t = Style.isInplace (style t)
  fun minIndent t = Style.minIndent (style t)

end