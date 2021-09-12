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

  val readSMLPathsFromMLB: FilePath.t -> FilePath.t Seq.t
end =
struct

  fun readSMLPathsFromMLB mlbPath : FilePath.t Seq.t =
    let
      open MLBAst

      fun doBasdec relativeDir basdec =
        case basdec of
          DecMultiple {elems, ...} =>
            Seq.flatten (Seq.map (doBasdec relativeDir) elems)
        | DecPathMLB {path, token} =>
            (doMLB relativeDir path
            handle OS.SysErr (msg, _) =>
              let
                val fullPath =
                  FilePath.normalize (FilePath.join (relativeDir, path))
              in
                ParserUtils.error
                  { pos = MLBToken.getSource token
                  , what = ("Could not open file: " ^ FilePath.toUnixPath fullPath)
                  , explain = SOME msg
                  }
              end)
        | DecPathSML {path, ...} =>
            Seq.singleton path
        | DecBasis {elems, ...} =>
            Seq.flatten (Seq.map (doBasexp relativeDir o #basexp) elems)
        | DecLocalInEnd {basdec1, basdec2, ...} =>
            Seq.append
              (doBasdec relativeDir basdec1, doBasdec relativeDir basdec2)
        | DecAnn {basdec, ...} =>
            doBasdec relativeDir basdec
        | _ => Seq.empty ()

      and doBasexp relativeDir basexp =
        case basexp of
          BasEnd {basdec, ...} => doBasdec relativeDir basdec
        | LetInEnd {basdec, basexp, ...} =>
            Seq.append (doBasdec relativeDir basdec, doBasexp relativeDir basexp)
        | _ => Seq.empty ()

      and doMLB relativeDir mlbPath =
        let
          val fullPath =
            FilePath.normalize (FilePath.join (relativeDir, mlbPath))
          val mlbSrc = Source.loadFromFile fullPath
          val Ast basdec = MLBParser.parse mlbSrc
        in
          doBasdec (FilePath.dirname fullPath) basdec
        end

    in
      doMLB (FilePath.fromUnixPath ".") mlbPath
    end

end
