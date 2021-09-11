(** Copyright (c) 2021 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure ParseAllSMLFromMLB:
sig
  (** Take an .mlb source and fully parse all SML by loading all filepaths
    * recursively specified by the .mlb and parsing them, etc.
    *)
  (* val parse: Source.t -> Ast.t *)

  val allSMLPaths: Source.t -> FilePath.t Seq.t
end =
struct

  fun allSMLPaths mlbSrc : FilePath.t Seq.t =
    let
      open MLBAst

      fun doBasdec basdec =
        case basdec of
          DecMultiple {elems, ...} =>
            Seq.flatten (Seq.map doBasdec elems)
        | DecPathMLB {path, ...} =>
            allSMLPaths (Source.loadFromFile path)
        | DecPathSML {path, ...} =>
            Seq.singleton path
        | DecBasis {elems, ...} =>
            Seq.flatten (Seq.map (doBasexp o #basexp) elems)
        | DecLocalInEnd {basdec1, basdec2, ...} =>
            Seq.append (doBasdec basdec1, doBasdec basdec2)
        | DecAnn {basdec, ...} =>
            doBasdec basdec
        | _ => Seq.empty ()

      and doBasexp basexp =
        case basexp of
          BasEnd {basdec, ...} => doBasdec basdec
        | LetInEnd {basdec, basexp, ...} =>
            Seq.append (doBasdec basdec, doBasexp basexp)
        | _ => Seq.empty ()

      val Ast basdec = MLBParser.parse mlbSrc
    in
      doBasdec basdec
    end

end
