(** Copyright (c) 2021 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure ParseAllSMLFromMLB:
sig
  (** Take an .mlb source and fully parse all SML by loading all filepaths
    * recursively specified by the .mlb and parsing them, etc.
    *)
  val parse: MLtonPathMap.t -> FilePath.t -> Ast.t
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



  fun parse pathmap mlbPath : Ast.t =
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


      fun doBasdec infdict parents relativeDir basdec =
        case basdec of

          DecMultiple {elems, ...} =>
            let
              fun doElem ((infdict, ast), basdec) =
                let
                  val (infdict', ast') =
                    doBasdec infdict parents relativeDir basdec
                in
                  (infdict', Ast.join (ast, ast'))
                end
            in
              Seq.iterate doElem (infdict, Ast.empty) elems
            end

        | DecPathMLB {path, token} =>
            (doMLB parents relativeDir path
            handle OS.SysErr (msg, _) =>
              let
                (* val extra =
                  ( "relative = " ^ FilePath.toUnixPath relativeDir ^ "\n"
                  ^ "original path = " ^ FilePath.toUnixPath path ^ "\n"
                  ) *)
                val path = expandAndJoin relativeDir path
                val backtrace =
                  "Included from: " ^ String.concatWith " -> "
                    (List.rev (List.map FilePath.toUnixPath parents))
              in
                ParserUtils.error
                  { pos = MLBToken.getSource token
                  , what = (msg ^ ": " ^ FilePath.toUnixPath path)
                  , explain = SOME (backtrace (*^ "\n" ^ extra*))
                  }
              end)

        | DecPathSML {path, token} =>
            (let
              val path = expandAndJoin relativeDir path
              val src = Source.loadFromFile path
            in
              Parser.parseWithInfdict infdict src
            end
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

        | DecBasis {elems, ...} =>
            let
              fun doElem ((infdict, ast), {basexp, ...}) =
                let
                  val (infdict', ast') =
                    doBasexp infdict parents relativeDir basexp
                in
                  (infdict', Ast.join (ast, ast'))
                end
            in
              Seq.iterate doElem (infdict, Ast.empty) elems
            end

        | DecLocalInEnd {basdec1, basdec2, ...} =>
            let
              val (infdict, ast1) = doBasdec infdict parents relativeDir basdec1
              val (infdict, ast2) = doBasdec infdict parents relativeDir basdec2
            in
              (infdict, Ast.join (ast1, ast2))
            end

        | DecAnn {basdec, ...} =>
            doBasdec infdict parents relativeDir basdec

        | _ =>
            (infdict, Ast.empty)


      and doBasexp infdict parents relativeDir basexp =
        case basexp of
          BasEnd {basdec, ...} =>
            doBasdec infdict parents relativeDir basdec

        | LetInEnd {basdec, basexp, ...} =>
            let
              val (infdict, ast1) = doBasdec infdict parents relativeDir basdec
              val (infdict, ast2) = doBasexp infdict parents relativeDir basexp
            in
              (infdict, Ast.join (ast1, ast2))
            end

        | _ =>
            (infdict, Ast.empty)


      and doMLB parents relativeDir mlbPath =
        let
          val path = expandAndJoin relativeDir mlbPath
          val _ = print ("loading " ^ FilePath.toUnixPath path ^ "\n")
          val mlbSrc = Source.loadFromFile path
          val Ast basdec = MLBParser.parse mlbSrc
        in
          doBasdec InfixDict.empty (path :: parents) (FilePath.dirname path) basdec
        end


      val (_, ast) = doMLB [] (FilePath.fromUnixPath ".") mlbPath
    in
      ast
    end

end
