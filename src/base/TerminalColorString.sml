(** Copyright (c) 2021 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure TerminalColorString :>
sig
  type t
  type color = TerminalColors.color

  val fromChar: char -> t
  val fromString: string -> t
  val toString: {colors: bool} -> t -> string

  val size: t -> int
  val empty: t
  val append: t * t -> t
  val concat: t list -> t
  val concatWith: t -> t list -> t

  val foreground: color -> t -> t
  val background: color -> t -> t
  val bold: t -> t
  val italic: t -> t
  val underline: t -> t
  val clear: t -> t

  (** prints with colors if stdout is a terminal,
    * and without colors otherwise *)
  val print: t -> unit
end =
struct

  structure TC = TerminalColors

  type color = TC.color

  type attributes =
    { foreground: color option
    , background: color option
    , bold: bool
    , italic: bool
    , underline: bool
    }

  datatype t =
    Append of {size: int, left: t, right: t}
  | Attributes of {size: int, attr: attributes, child: t}
  | String of string
  | Empty

  fun size t =
    case t of
      Append {size=n, ...} => n
    | Attributes {size=n, ...} => n
    | String s => String.size s
    | Empty => 0

  val empty = Empty

  fun fromChar c = String (String.str c)
  fun fromString s = String s

  fun append (t1, t2) =
    case (t1, t2) of
      (Empty, _) => t2
    | (_, Empty) => t1
    | _ => Append {size = size t1 + size t2, left = t1, right = t2}

  fun concat ts =
    List.foldl (fn (next, prev) => append (prev, next)) Empty ts

  fun concatWith t ts =
    case ts of
      [] => empty
    | first :: rest =>
        List.foldl (fn (next, prev) => concat [prev, t, next]) first rest

  val default =
    { foreground = NONE
    , background = NONE
    , bold = false
    , italic = false
    , underline = false
    }

  fun setForeground (a: attributes) color =
    { foreground = SOME color
    , background = #background a
    , bold = #bold a
    , italic = #italic a
    , underline = #underline a
    }

  fun setBackground (a: attributes) color =
    { foreground = #foreground a
    , background = SOME color
    , bold = #bold a
    , italic = #italic a
    , underline = #underline a
    }

  fun setBold (a: attributes) =
    { foreground = #foreground a
    , background = #background a
    , bold = true
    , italic = #italic a
    , underline = #underline a
    }

  fun setItalic (a: attributes) =
    { foreground = #foreground a
    , background = #background a
    , bold = #bold a
    , italic = true
    , underline = #underline a
    }

  fun setUnderline (a: attributes) =
    { foreground = #foreground a
    , background = #background a
    , bold = #bold a
    , italic = #italic a
    , underline = true
    }

  fun mergeOpt xo yo =
    case yo of
      SOME _ => yo
    | NONE => xo

  fun mergeAttributes (a1: attributes) (a2: attributes) =
    { foreground = mergeOpt (#foreground a1) (#foreground a2)
    , background = mergeOpt (#background a1) (#background a2)
    , bold = #bold a1 orelse #bold a2
    , italic = #italic a1 orelse #italic a2
    , underline = #underline a1 orelse #underline a2
    }

  fun splitAttributes t =
    case t of
      Attributes {attr=a, child, ...} => (a, child)
    | _ => (default, t)

  fun foreground color t =
    let
      val (a, t) = splitAttributes t
    in
      Attributes
        { size = size t
        , attr = setForeground a color
        , child = t
        }
    end

  fun background color t =
    let
      val (a, t) = splitAttributes t
    in
      Attributes
        { size = size t
        , attr = setBackground a color
        , child = t
        }
    end

  fun bold t =
    let
      val (a, t) = splitAttributes t
    in
      Attributes
        { size = size t
        , attr = setBold a
        , child = t
        }
    end

  fun italic t =
    let
      val (a, t) = splitAttributes t
    in
      Attributes
        { size = size t
        , attr = setItalic a
        , child = t
        }
    end

  fun underline t =
    let
      val (a, t) = splitAttributes t
    in
      Attributes
        { size = size t
        , attr = setUnderline a
        , child = t
        }
    end

  fun clear t =
    case t of
      Append {size, left, right} =>
        Append {size = size, left = clear left, right = clear right}
    | Attributes {child, ...} => child
    | _ => t

  fun toString {colors} t =
    let
      fun traverse attr acc t =
        case t of
          Append {left, right, ...} =>
            traverse attr (traverse attr acc left) right
        | Attributes {attr=attr', child, ...} =>
            traverse (mergeAttributes attr attr') acc child
        | Empty => acc
        | String s =>
            if not colors then s :: acc else
            let
              val acc =
                case #foreground attr of
                  NONE => acc
                | SOME c => TC.foreground c :: acc
              val acc =
                case #background attr of
                  NONE => acc
                | SOME c => TC.background c :: acc
              val acc = if #bold attr then TC.bold :: acc else acc
              val acc = if #underline attr then TC.underline :: acc else acc
              val acc = if #italic attr then TC.italic :: acc else acc
            in
              TC.reset :: s :: acc
            end

    in
      String.concat (List.rev (traverse default [] t))
    end


  fun print_ t =
    let
      val stdOutKind = OS.IO.kind (Posix.FileSys.fdToIOD Posix.FileSys.stdout)
    in
      if stdOutKind = OS.IO.Kind.tty then
        print (toString {colors=true} t)
      else
        print (toString {colors=false} t)
    end

  val print = print_

end
