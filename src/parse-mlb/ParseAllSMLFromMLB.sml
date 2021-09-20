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
  (* val readSMLPathsFromMLB: MLtonPathMap.t -> FilePath.t -> FilePath.t Seq.t *)
end =
struct

  structure VarKey =
  struct
    type t = Token.t
    fun compare (tok1, tok2) =
      String.compare (Token.toString tok1, Token.toString tok2)
  end

  structure FilePathKey =
  struct
    type t = FilePath.t
    fun compare (fp1, fp2) =
      String.compare (FilePath.toUnixPath fp1, FilePath.toUnixPath fp2)
  end

  structure VarDict = Dict (VarKey)
  structure FilePathDict = Dict (FilePathKey)

  (** For the purposes of parsing, we only need to remember infix definitions
    * across source files.
    *
    * TODO: FIX: a basis also needs to be explicit about what it has set nonfix!
    * (When merging bases, if the second basis sets an identifier nonfix, then
    * the previous basis infix is overridden.)
    *)
  type basis =
    {fixities: InfixDict.t}

  val emptyBasis = {fixities = InfixDict.empty}

  fun mergeBases (b1: basis, b2: basis) =
    {fixities = InfixDict.merge (#fixities b1, #fixities b2)}

  type context =
    { parents: FilePath.t list
    , dir: FilePath.t
    (*, mlbs: basis FilePathDict.t
    , bases: basis VarDict.t *)
    }

  type mlb_cache = (basis * Ast.ast) FilePathDict.t


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


      fun fileErrorHandler ctx path token errorMessage =
        let
          val path = expandAndJoin (#dir ctx) path
          val backtrace =
            "Included from: " ^ String.concatWith " -> "
              (List.rev (List.map FilePath.toUnixPath (#parents ctx)))
        in
          ParserUtils.error
            { pos = MLBToken.getSource token
            , what = (errorMessage ^ ": " ^ FilePath.toUnixPath path)
            , explain = SOME backtrace
            }
        end


      fun doSML (ctx: context) (basis, path, errFun) =
        let
          val path = expandAndJoin (#dir ctx) path

          val _ = print ("loading " ^ FilePath.toUnixPath path ^ "\n")
          val src =
            Source.loadFromFile path
            handle OS.SysErr (msg, _) => errFun msg

          val (infdict, ast) = Parser.parseWithInfdict (#fixities basis) src
        in
          ({fixities = infdict}, ast)
        end


      fun doMLB (ctx: context) (mlbCache, basis, path, errFun) =
        let
          val path = expandAndJoin (#dir ctx) path

          val mlbCache =
            if FilePathDict.contains mlbCache path then mlbCache else
            let
              val _ = print ("loading " ^ FilePath.toUnixPath path ^ "\n")
              val mlbSrc =
                Source.loadFromFile path
                handle OS.SysErr (msg, _) => errFun msg

              val Ast basdec = MLBParser.parse mlbSrc

              val ctx' =
                { parents = path :: #parents ctx
                , dir = FilePath.dirname path
                }

              val (mlbCache, b, a) =
                doBasdec ctx' (mlbCache, emptyBasis, basdec)
            in
              FilePathDict.insert mlbCache (path, (b, a))
            end

          val (basis', ast) = FilePathDict.lookup mlbCache path
        in
          (mlbCache, mergeBases (basis, basis'), ast)
        end


      and doBasdec
        (ctx: context)
        (mlbCache: mlb_cache, basis: basis, basdec: MLBAst.basdec)
        : (mlb_cache * basis * Ast.ast)
      = case basdec of

          DecPathMLB {path, token} =>
            doMLB ctx (mlbCache, basis, path, fileErrorHandler ctx path token)

        | DecPathSML {path, token} =>
            let
              val (basis, ast) =
                doSML ctx (basis, path, fileErrorHandler ctx path token)
            in
              (mlbCache, basis, ast)
            end

        | DecMultiple {elems, ...} =>
            let
              fun doElem ((mlbCache, basis, ast), basdec) =
                let
                  val (mlbCache, basis', ast') =
                    doBasdec ctx (mlbCache, basis, basdec)
                in
                  (mlbCache, basis', Ast.join (ast, ast'))
                end
            in
              Seq.iterate doElem (mlbCache, basis, Ast.empty) elems
            end

        | DecBasis {elems, ...} =>
            let
              fun doElem ((mlbCache, basis, ast), {basexp, ...}) =
                let
                  val (mlbCache, basis', ast') =
                    doBasexp ctx (mlbCache, basis, basexp)
                in
                  (mlbCache, basis', Ast.join (ast, ast'))
                end
            in
              Seq.iterate doElem (mlbCache, basis, Ast.empty) elems
            end

        | DecLocalInEnd {basdec1, basdec2, ...} =>
            let
              (** TODO: FIX: this is not quite right; stuff exported by
                * basdec1 should not be visible in the overall basis.
                *)
              val (mlbCache, basis, ast1) =
                doBasdec ctx (mlbCache, basis, basdec1)
              val (mlbCache, basis, ast2) =
                doBasdec ctx (mlbCache, basis, basdec2)
            in
              (mlbCache, basis, Ast.join (ast1, ast2))
            end

        | DecAnn {basdec, ...} =>
            doBasdec ctx (mlbCache, basis, basdec)

        | _ =>
            (mlbCache, basis, Ast.empty)


      and doBasexp ctx (mlbCache, basis, basexp) =
        case basexp of

          BasEnd {basdec, ...} =>
            doBasdec ctx (mlbCache, basis, basdec)

        | LetInEnd {basdec, basexp, ...} =>
            let
              (** TODO: FIX: this is not quite right; stuff exported by
                * basdec should not be visible in the overall basis.
                *)
              val (mlbCache, basis, ast1) =
                doBasdec ctx (mlbCache, basis, basdec)
              val (mlbCache, basis, ast2) =
                doBasexp ctx (mlbCache, basis, basexp)
            in
              (mlbCache, basis, Ast.join (ast1, ast2))
            end

        | _ =>
            (mlbCache, basis, Ast.empty)


      fun topLevelError msg =
        raise Error.Error (Error.ErrorReport
          { header = "FILE ERROR"
          , content =
              [ ErrorReport.Paragraph
                  (msg ^ ": " ^ FilePath.toUnixPath mlbPath)
              ]
          })

      val emptyCache = FilePathDict.empty

      val (_, _, ast) =
        doMLB {parents = [], dir = FilePath.fromUnixPath "."}
          (emptyCache, emptyBasis, mlbPath, topLevelError)
    in
      ast
    end

end
