(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettyDoc :>
sig
  type t
  type doc = t

  val pretty: doc -> string

  val text: string -> doc
  val separate: doc * doc -> doc
  val horizontal: doc * doc -> doc
  val verticalNest: doc * int * doc -> doc
  (* val chooseBest: doc list -> doc *)
end =
struct

  datatype elem = Sep | Text of string

  type line = elem list

  datatype doc =
    Doc of
      { last: line
      , prev: line list  (** in reverse order *)
      }

  type t = doc

  fun spaces count =
    CharVector.tabulate (count, fn _ => #" ")

  fun indentBy k line =
    Text (spaces k) :: line

  fun makeLine line =
    let
      fun f (next, (prevIsSep, str)) =
        case next of
          Sep => (true, str)
        | Text x => (false, str ^ (if prevIsSep then " " else "") ^ x)

      val (_, str) = List.foldl f (false, "") line
    in
      str
    end

  fun lineLength line =
    let
      fun f (next, (prevIsSep, size)) =
        case next of
          Sep =>
            (true, size)
        | Text x =>
            (false, size + String.size x + (if prevIsSep then 1 else 0))

      val (_, size) = List.foldl f (false, 0) line
    in
      size
    end

  fun last (Doc {last=x, ...}) = x
  fun prev (Doc {prev=x, ...}) = x


  (** doc -> string * (doc option) *)
  fun splitFirst (Doc {last, prev}) =
    case Util.splitLast prev of
      NONE => (last, NONE)
    | SOME (prev', first) =>
        (first, SOME (Doc {last=last, prev=prev'}))

  fun text x =
    Doc {last = [Text x], prev = []}

  fun verticalNest (d1, k, d2) =
    Doc
      { prev = List.map (indentBy k) (prev d2) @ (last d1 :: (prev d1))
      , last = indentBy k (last d2)
      }

  fun horizontal' withSep (d1, d2) =
    let
      val (first2, maybeRestOfD2) = splitFirst d2
      val d1' =
        Doc
          { last = last d1 @ (if withSep then Sep :: first2 else first2)
          , prev = prev d1
          }
      val indentSize =
        lineLength (last d1) + (if withSep then 1 else 0)
    in
      case maybeRestOfD2 of
        NONE =>
          d1'
      | SOME d2' =>
          verticalNest (d1', indentSize, d2')
    end

  val horizontal = horizontal' false
  val separate = horizontal' true

  fun pretty (Doc {last, prev}) =
    String.concatWith "\n" (List.map makeLine (List.rev (last :: prev)))

end
