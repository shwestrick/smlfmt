(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

(** This is based on Daan Leijen's wl-pprint library for Haskell:
  *   https://hackage.haskell.org/package/wl-pprint-1.2.1/src/
  * which is (in turn) based on Wadler's "prettier printer":
  *   http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
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
    datatype t =
      Empty
    | Text of string * t
    | Newline of int * t

    fun spaces count =
      CharVector.tabulate (count, fn _ => #" ")

    fun toString sdoc =
      let
        fun loop acc sdoc =
          case sdoc of
            Empty => String.concat (List.rev acc)
          | Text (s, sdoc') => loop (s :: acc) sdoc'
          | Newline (k, sdoc') => loop (spaces k :: "\n" :: acc) sdoc'
      in
        loop [] sdoc
      end
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
      Empty => Empty
    | Text s => Text s
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
      val r = ribbonWidth
      val w = maxWidth

      datatype docs = Nil | Cons of int * doc * docs

      fun fits w x =
        if w < 0 then false else
        case x of
          SimpleDoc.Empty => true
        | SimpleDoc.Text (s, x') => fits (w - String.size s) x'
        | SimpleDoc.Newline (k, x') => true

      fun nicest n k x y =
        let
          val width = Int.min (w - k, r - k + n)
        in
          if fits width x then x else y
        end

      (** TODO: Needs to be optimized.
        *
        * This is "correct" but inefficient, because it is adapted from
        * lazy code. Specifically, in Union case, we only need to continue
        * as far as necessary to figure out if the first line fits. But
        * this code will fully evaluate both tails of the document.
        *)
      fun best n k Nil = SimpleDoc.Empty
        | best n k (Cons (i, d, ds)) =
            case d of
              Empty        => best n k ds
            | Text s       => SimpleDoc.Text (s, best n (k + String.size s) ds)
            | Line         => SimpleDoc.Newline (i, best i i ds)
            | Break        => SimpleDoc.Newline (i, best i i ds)
            | Cat (x, y)   => best n k (Cons (i, x, Cons (i, y, ds)))
            | Nest (j, x)  => best n k (Cons (i+j, x, ds))
            | Union (x, y) => nicest n k (best n k (Cons (i, x, ds)))
                                         (best n k (Cons (i, y, ds)))

    in
      best 0 0 (Cons (0, inputDoc, Nil))
    end


  fun toString doc =
    SimpleDoc.toString (pretty {ribbonFrac = 0.4, maxWidth = 80} doc)

end
