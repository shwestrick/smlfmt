(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

(** A source is a contiguous subsequence of a code file. We can look up its
  * position (e.g. line-and-column, or character index), take subsequences,
  * etc.
  *)
structure Source:
sig
  type source
  type t = source

  val loadFromFile: FilePath.t -> source

  val fileName: source -> FilePath.t
  val absoluteStart: source -> {line: int, col: int}
  val absoluteEnd: source -> {line: int, col: int}

  val length: source -> int
  val nth: source -> int -> char
  val subseq: source -> int * int -> source
  val take: source -> int -> source
  val drop: source -> int -> source

  val toString: source -> string

  val base: source -> source
end =
struct

  type source =
    { slice: char Seq.t
    (** underlying file *)
    , fileName: FilePath.t
    (** the byte offsets of newlines in the underlying file *)
    , newlineIdxs: int Seq.t
    }

  type t = source


  fun loadFromFile path =
    let
      val contents = ReadFile.contentsSeq (FilePath.toHostPath path)
      val n = Seq.length contents

      (** Check that we can use the slice base as the actual base. *)
      val (_, absoluteOffset, _) = ArraySlice.base contents
      val _ =
        if absoluteOffset = 0 then ()
        else raise Fail "bug in Source.loadFromFile: expected \
                        \ReadFile.contentsSeq to return slice at offset 0"

      val newlineIdxs =
        ArraySlice.full (SeqBasis.filter 2000 (0, n)
          (fn i => i)
          (fn i => Seq.nth contents i = #"\n"))
    in
      { slice = contents
      , fileName = path
      , newlineIdxs = newlineIdxs
      }
    end

  fun fileName (s: source) = #fileName s

  fun absoluteOffset ({slice, ...}: source) =
    let
      val (_, off, _) = ArraySlice.base slice
    in
      off
    end

  fun absoluteStart (s as {newlineIdxs, ...}: source) =
    if absoluteOffset s = 0 then
      {line = 1, col = 1}
    else
      let
        val lineNum =
          BinarySearch.search Int.compare newlineIdxs (absoluteOffset s)
        val lineOffset =
          if lineNum = 0 then 0 else 1 + Seq.nth newlineIdxs (lineNum - 1)
        val charNum = absoluteOffset s - lineOffset
      in
        (** Convert to 1-indexing *)
        {line = lineNum+1, col = charNum+1}
      end

  fun length ({slice, ...}: source) = Seq.length slice
  fun nth ({slice, ...}: source) k = Seq.nth slice k

  fun subseq {slice, fileName, newlineIdxs} (i, len) =
    { slice = Seq.subseq slice (i, Int.min (len, Seq.length slice - i))
    , fileName = fileName
    , newlineIdxs = newlineIdxs
    }

  fun take s k = subseq s (0, k)
  fun drop s k = subseq s (k, length s - k)

  fun absoluteEnd s =
    absoluteStart (drop s (length s))

  fun toString s =
    CharVector.tabulate (length s, nth s)

  fun base ({slice, fileName, newlineIdxs}: source) =
    let
      val (a, _, _) = ArraySlice.base slice
    in
      { slice = ArraySlice.full a
      , fileName = fileName
      , newlineIdxs = newlineIdxs
      }
    end

end
