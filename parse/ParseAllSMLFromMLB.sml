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

  val readSMLPathsFromMLB: MLtonPathMap.t -> FilePath.t -> FilePath.t Seq.t
end =
struct

  fun readSMLPathsFromMLB pathmap mlbPath : FilePath.t Seq.t =
    let
      open MLBAst

      fun expandAndJoin relativeDir path =
        let
          val path = MLtonPathMap.expandPath pathmap path
        in
          if FilePath.isAbsolute path then
            path
          else
            FilePath.normalize (FilePath.join (relativeDir, path))
        end


      fun doBasdec parents relativeDir basdec =
        case basdec of
          DecMultiple {elems, ...} =>
            Seq.flatten (Seq.map (doBasdec parents relativeDir) elems)
        | DecPathMLB {path, token} =>
            (doMLB parents relativeDir path
            handle OS.SysErr (msg, _) =>
              let
                val path = expandAndJoin relativeDir path
                val backtrace =
                  "Included from: " ^ String.concatWith " -> "
                    (List.rev (List.map FilePath.toUnixPath parents))
              in
                ParserUtils.error
                  { pos = MLBToken.getSource token
                  , what = (msg ^ ": " ^ FilePath.toUnixPath path)
                  , explain = SOME backtrace
                  }
              end)
        | DecPathSML {path, ...} =>
            Seq.singleton (expandAndJoin relativeDir path)
        | DecBasis {elems, ...} =>
            Seq.flatten (Seq.map (doBasexp parents relativeDir o #basexp) elems)
        | DecLocalInEnd {basdec1, basdec2, ...} =>
            Seq.append
              (doBasdec parents relativeDir basdec1, doBasdec parents relativeDir basdec2)
        | DecAnn {basdec, ...} =>
            doBasdec parents relativeDir basdec
        | _ => Seq.empty ()

      and doBasexp parents relativeDir basexp =
        case basexp of
          BasEnd {basdec, ...} => doBasdec parents relativeDir basdec
        | LetInEnd {basdec, basexp, ...} =>
            Seq.append
              ( doBasdec parents relativeDir basdec
              , doBasexp parents relativeDir basexp
              )
        | _ => Seq.empty ()

      and doMLB parents relativeDir mlbPath =
        let
          val path = expandAndJoin relativeDir mlbPath
          val _ = print ("loading " ^ FilePath.toUnixPath path ^ "\n")
          val mlbSrc = Source.loadFromFile path
          val Ast basdec = MLBParser.parse mlbSrc
        in
          doBasdec (path :: parents) (FilePath.dirname path) basdec
        end

    in
      doMLB [] (FilePath.fromUnixPath ".") mlbPath
    end

end
