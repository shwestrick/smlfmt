(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettierDoc :>
sig
  type t
  type doc = t

  structure SimpleDoc:
  sig
    type t
    val toString: t -> string
  end

  val pretty: {ribbonFrac: real, maxWidth: int} -> doc -> SimpleDoc.t
  val toString: doc -> string

  val empty: doc
  val text: string -> doc

  val line: doc
  val softline: doc
  val softbreak: doc

  val cat: doc * doc -> doc
  val nest: int -> doc -> doc
  val group: doc -> doc
end =
struct

  structure SimpleDoc =
  struct
    datatype elem =
      Text of string
    | Newline of int  (** indent amount *)

    type t = elem list

    fun spaces count =
      CharVector.tabulate (count, fn _ => #" ")

    fun toString elems =
      String.concat
        (List.map (fn Text s => s | Newline k => ("\n" ^ spaces k)) elems)
  end


  datatype doc =
    Empty
  | Text of string    (** No newlines permitted! *)
  | Line              (** Becomes a space when undone by group *)
  | Break             (** Becomes empty when undone by group *)
  | Cat of doc * doc
  | Nest of int * doc
  | Union of doc * doc

  type t = doc

  val empty = Empty
  fun text s = Text s
  fun cat (d1, d2) = Cat (d1, d2)
  fun nest k d = Nest (k, d)

  val line = Line

  fun flatten doc =
    case doc of
      Empty => doc
    | Text s => doc
    | Line => Text " "
    | Break => Empty
    | Cat (d1, d2) => Cat (flatten d1, flatten d2)
    | Nest (k, d) => Nest (k, flatten d)
    | Union (d, _) => flatten d

  fun group x = Union (flatten x, x)

  val softline = group Line
  val softbreak = group Break



  fun pretty {ribbonFrac, maxWidth} inputDoc =
    let
      (** Guarantees 0 <= ribbonWidth <= maxWidth *)
      val ribbonWidth =
        Int.max (0, Int.min (maxWidth,
          Real.round (ribbonFrac * Real.fromInt maxWidth)))

      (** =============================================================
        * We're now going to define a function `simplify` that performs
        * an inorder tree traversal on a document, threading a state
        * parameter through the computation.
        *     val simplify: state -> (int * doc) -> state
        * Essentially, the state is just a partial simple doc. We could
        * get away with `type state = SimpleDoc.elem list`, but to compute
        * things about the current state faster, we also memoize some
        * info along the way.
        *
        * This makes the state essentially an ADT:
        *     type state
        *     val initialState: state
        *     val toSimpleDoc: state -> SimpleDoc.t
        *     val push: state -> SimpleDoc.elem -> state
        *     val isNice: state -> bool
        *)

      type simplify_state =
        { firstLineStartAndSize: (int * int) option
        , currLineStart: int
        , currLineSize: int
        , elems: SimpleDoc.elem list (** In reverse order! *)
        }

      fun toSimpleDoc ({elems, ...}: simplify_state) =
        List.rev elems

      val initialState =
        { currLineSize = 0
        , currLineStart = 0
        , firstLineStartAndSize = NONE
        , elems = []
        }

      fun push (s: simplify_state) elem =
        case elem of
          SimpleDoc.Text str =>
            { currLineStart = #currLineStart s
            , currLineSize = #currLineSize s + String.size str
            , elems = elem :: #elems s
            , firstLineStartAndSize = #firstLineStartAndSize s
            }
        | SimpleDoc.Newline indent =>
            { currLineStart = indent
            , currLineSize = indent
            , elems = elem :: #elems s
            , firstLineStartAndSize =
                case #firstLineStartAndSize s of
                  NONE => SOME (#currLineStart s, #currLineSize s)
                | other => other
            }

      fun isNice (s: simplify_state) =
        let
          val (firstStart, firstSize) =
            case #firstLineStartAndSize s of
              SOME x => x
            | NONE => (#currLineStart s, #currLineSize s)
        in
          (firstSize - firstStart) <= ribbonWidth
          andalso
          firstSize <= maxWidth
        end

      fun simplify state (indent, doc) =
        case doc of
          Empty =>
            state
        | Text s =>
            push state (SimpleDoc.Text s)
        | Nest (k, doc') =>
            simplify state (indent+k, doc')
        | Line =>
            push state (SimpleDoc.Newline indent)
        | Break =>
            push state (SimpleDoc.Newline indent)
        | Cat (doc1, doc2) =>
            simplify (simplify state (indent, doc1)) (indent, doc2)
        | Union (doc1, doc2) =>
            let
              (** This is correct, but slow. I adapted this code from a
                * lazy implementation that would only process as far as needed
                * in order to figure out which is nicer.
                *
                * TODO: Optimize this by refactoring the code into
                * line-generators. Essentially, the idea would be to simplify
                * until we hit a newline, and then return our progress so far,
                * as well as a continuation for the rest of the computation
                * that will only be called if needed.
                *)
              val state1 = simplify state (indent, doc1)
              val state2 = simplify state (indent, doc2)
            in
              (** Note that the first lines of state1 are longer than the
                * first lines of state2, because of how `group` works.
                * Therefore, if state1 is nice, it's preferable, because
                * it maximizes horizontal usage.
                *)
              if isNice state1 then
                state1
              else
                state2
            end

    in
      toSimpleDoc (simplify initialState (0, inputDoc))
    end


  fun toString doc =
    SimpleDoc.toString (pretty {ribbonFrac = 0.4, maxWidth = 80} doc)

end
