(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettySimpleDoc :>
sig
  type doc
  type t = doc

  val empty: doc
  val text: string -> doc

  val beside: doc * doc -> doc
  val aboveOrSpace: doc * doc -> doc
  val aboveOrBeside: doc * doc -> doc

  val space: doc
  val softspace: doc
  val group: doc -> doc

  val pretty: int -> doc -> string
  val toString: doc -> string
end =
struct

  (** for Space and Above, the boolean indicates whether or not to
    * keep space when undone by group.
    *)
  datatype doc =
    Empty
  | Space of bool
  | Text of string
  | Beside of doc * doc
  | Above of bool * doc * doc
  | Choice of {flattened: (bool * doc * int * bool), normal: doc}


  type t = doc


  val empty = Empty
  val space = Space true
  val softspace = Space false
  val text = Text


  fun beside (doc1, doc2) =
    case (doc1, doc2) of
      (Empty, _) => doc2
    | (_, Empty) => doc1
    | _ => Beside (doc1, doc2)


  fun above' withSpace (doc1, doc2) =
    case (doc1, doc2) of
      (Empty, _) => doc2
    | (_, Empty) => doc1
    | _ => Above (withSpace, doc1, doc2)

  val aboveOrSpace = above' true
  val aboveOrBeside = above' false

  fun flatten doc =
    let
      (** Returns (space-before?, flattened, flattened size, space-after?) *)
      fun loop doc =
        case doc of
          Empty =>
            (false, Empty, 0, false)
        | Space keepSpace =>
            (keepSpace, Empty, 0, keepSpace)
        | Text str =>
            (false, Text str, String.size str, false)
        | Beside (d1, d2) =>
            loopBeside (d1, d2)
        | Above (withSpace, d1, d2) =>
            if withSpace then
              loopBeside (d1, Beside (Space true, d2))
            else
              loopBeside (d1, d2)
        | Choice {flattened, ...} =>
            flattened

      and loopBeside (d1, d2) =
        let
          val (l1, flat1, sz1, r1) = loop d1
          val (l2, flat2, sz2, r2) = loop d2

          (** Beside(flat1, flat2), but put a space between if
            * necessary, and compute the size too. This might result in
            * spaces l or r on either side, if flat1 or flat2 is Empty
            *)
          val (l, m, sz, r) =
            case (flat1, r1 orelse l2, flat2) of
              (Empty, b, _) =>
                (b, flat2, sz2, false)
            | (_, b, Empty) =>
                (false, flat1, sz1, b)
            | (_, false, _) =>
                (false, Beside (flat1, flat2), sz1+sz2, false)
            | _ =>
                (false, Beside (flat1, Beside (Space true, flat2)), sz1+sz2+1, false)
        in
          ( l1 orelse l
          , m
          , sz
          , r2 orelse r
          )
        end

    in
      loop doc
    end


  fun group doc =
    Choice {flattened = flatten doc, normal = doc}


  fun spaces count =
    CharVector.tabulate (count, fn _ => #" ")


  fun pretty maxWidth inputDoc =
    let
      fun layout (col, acc) doc : int * (string list) =
        case doc of
          Empty => (col, acc)
        | Space _ => (col + 1, " " :: acc)
        | Text str => (col + String.size str, str :: acc)
        | Beside (doc1, doc2) =>
            layout (layout (col, acc) doc1) doc2
        | Above (_, doc1, doc2) =>
            let
              val (_, acc) = layout (col, acc) doc1
              val acc = spaces col :: "\n" :: acc
            in
              layout (col, acc) doc2
            end
        | Choice {flattened = (_, flat, sz, _), normal} =>
            if col + sz <= maxWidth then
              layout (col, acc) flat
            else
              layout (col, acc) normal

      val (_, strs) = layout (0, []) inputDoc
    in
      String.concat (List.rev strs)
    end


  val toString = pretty 80

end
