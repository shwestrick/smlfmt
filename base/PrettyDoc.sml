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
  val horizontal: doc * doc -> doc
  val verticalNest: doc * int * doc -> doc
  (* val chooseBest: doc list -> doc *)
end =
struct


  fun spaces count =
    CharVector.tabulate (count, fn _ => #" ")

  fun indentBy k line =
    spaces k ^ line

  datatype doc =
    Doc of
      { last: string
      , prev: string list  (** in reverse order *)
      }

  type t = doc

  fun last (Doc {last=x, ...}) = x
  fun prev (Doc {prev=x, ...}) = x


  (** doc -> string * (doc option) *)
  fun splitFirst (Doc {last, prev}) =
    case Util.splitLast prev of
      NONE => (last, NONE)
    | SOME (prev', first) =>
        (first, SOME (Doc {last=last, prev=prev'}))


  fun text x =
    Doc {last = x, prev = []}


  fun verticalNest (d1, k, d2) =
    Doc
      { prev = List.map (indentBy k) (prev d2) @ (last d1 :: (prev d1))
      , last = indentBy k (last d2)
      }


  fun horizontal (d1, d2) =
    let
      val (first2, maybeRestOfD2) = splitFirst d2
      val d1' =
        Doc
          { last = last d1 ^ first2
          , prev = prev d1
          }
    in
      case maybeRestOfD2 of
        NONE =>
          d1
      | SOME d2' =>
          verticalNest (d1', String.size (last d1), d2')
    end

  fun pretty (Doc {last, prev}) =
    String.concatWith "\n" (List.rev (last :: prev))

end
