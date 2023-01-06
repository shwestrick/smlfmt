(** Copyright (c) 2021-2023 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure ParseAllSMLFromMLB:
sig
  (** Take an .mlb source and fully parse all SML by loading all filepaths
    * recursively specified by the .mlb and parsing them, etc.
    *)
  val parse:
    { pathmap: MLtonPathMap.t
    , skipBasis: bool
    , allowTopExp: bool
    , allowOptBar: bool
    }
    -> FilePath.t
    -> (FilePath.t * Parser.parser_output) Seq.t
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

  structure VarDict = Dict(VarKey)
  structure FilePathDict = Dict(FilePathKey)

  (** For the purposes of parsing, we only need to remember infix definitions
    * across source files.
    *
    * TODO: FIX: a basis also needs to be explicit about what it has set nonfix!
    * (When merging bases, if the second basis sets an identifier nonfix, then
    * the previous basis infix is overridden.)
    *)
  type basis = {fixities: InfixDict.t}
  fun newScope ({fixities}: basis) = {fixities = InfixDict.newScope fixities}
  fun popScope ({fixities}: basis) : {old: basis, popped: basis} =
    let val {old, popped} = InfixDict.popScope fixities
    in {old = {fixities = old}, popped = {fixities = popped}}
    end

  val emptyBasis = {fixities = InfixDict.empty}
  val initialTopLevelBasis = {fixities = InfixDict.initialTopLevel}

  fun mergeBases (b1: basis, b2: basis) =
    {fixities = InfixDict.merge (#fixities b1, #fixities b2)}

  type context =
    { parents: FilePath.t list
    , dir: FilePath.t
    (*, mlbs: basis FilePathDict.t
    , bases: basis VarDict.t *)
    }

  type mlb_cache = basis FilePathDict.t

  fun printErr m = TextIO.output (TextIO.stdErr, m)

  (** when skipBasis = true, we ignore paths containing $(SML_LIB) *)
  fun parse {skipBasis, pathmap, allowTopExp, allowOptBar} mlbPath :
    (FilePath.t * Parser.parser_output) Seq.t =
    let
      open MLBAst

      type asts = (FilePath.t * Parser.parser_output) list

      fun expandAndJoin relativeDir path =
        let
          val {result = path, used} = MLtonPathMap.expandPath pathmap path
        in
          { used = used
          , result =
              if FilePath.isAbsolute path then path
              else FilePath.normalize (FilePath.join (relativeDir, path))
          }
        end


      fun fileErrorHandler ctx path token errorMessage =
        let
          val {result = path, ...} = expandAndJoin (#dir ctx) path
          val backtrace =
            "Included from: "
            ^
            String.concatWith " -> " (List.rev
              (List.map FilePath.toUnixPath (#parents ctx)))
        in
          ParserUtils.error
            { pos = MLBToken.getSource token
            , what = (errorMessage ^ ": " ^ FilePath.toUnixPath path)
            , explain = SOME backtrace
            }
        end


      fun doSML (ctx: context) (basis, path, errFun) =
        let
          val {result = path, used} = expandAndJoin (#dir ctx) path
        in
          if skipBasis andalso List.exists (fn k => k = "SML_LIB") used then
            ( printErr ("skipping " ^ FilePath.toUnixPath path ^ "\n")
            ; printErr ("(basis files are skipped)\n")
            ; (basis, [])
            )
          else
            let
              val _ = printErr ("loading  " ^ FilePath.toUnixPath path ^ "\n")
              val src = Source.loadFromFile path
                        handle OS.SysErr (msg, _) => errFun msg

              val (infdict, ast) =
                Parser.parseWithInfdict
                  {allowTopExp = allowTopExp, allowOptBar = allowOptBar}
                  (#fixities basis) src
            in
              ({fixities = infdict}, [(path, ast)])
            end
        end


      fun doMLB (ctx: context) (mlbCache, basis, path, errFun) =
        let
          val {result = path, used} = expandAndJoin (#dir ctx) path
        in
          if skipBasis andalso List.exists (fn k => k = "SML_LIB") used then
            ( printErr ("skipping " ^ FilePath.toUnixPath path ^ "\n")
            ; printErr ("(basis files are skipped)\n")
            ; (mlbCache, mergeBases (basis, initialTopLevelBasis), [])
            )
          else
            let
              val (mlbCache, basis', asts) =
                if FilePathDict.contains mlbCache path then
                  (mlbCache, FilePathDict.lookup mlbCache path, [])
                else
                  let
                    val _ = printErr
                      ("loading  " ^ FilePath.toUnixPath path ^ "\n")
                    val mlbSrc = Source.loadFromFile path
                                 handle OS.SysErr (msg, _) => errFun msg

                    val Ast basdec = MLBParser.parse mlbSrc

                    val ctx' =
                      { parents = path :: #parents ctx
                      , dir = FilePath.dirname path
                      }

                    val (mlbCache, b, a) =
                      doBasdec ctx' (mlbCache, emptyBasis, basdec)
                  in
                    (FilePathDict.insert mlbCache (path, b), b, a)
                  end
            in
              (mlbCache, mergeBases (basis, basis'), asts)
            end
        end


      and doBasdec (ctx: context)
        (mlbCache: mlb_cache, basis: basis, basdec: MLBAst.basdec) :
        (mlb_cache * basis * asts) =
        case basdec of
          DecPathMLB {path, token} =>
            doMLB ctx (mlbCache, basis, path, fileErrorHandler ctx path token)

        | DecPathSML {path, token} =>
            let
              val (basis, asts) = doSML ctx
                (basis, path, fileErrorHandler ctx path token)
            in
              (mlbCache, basis, asts)
            end

        | DecMultiple {elems, ...} =>
            let
              fun doElem ((mlbCache, basis, asts), basdec) =
                let
                  val (mlbCache, basis', asts') =
                    doBasdec ctx (mlbCache, basis, basdec)
                in
                  (mlbCache, basis', asts' @ asts)
                end
            in
              Seq.iterate doElem (mlbCache, basis, []) elems
            end

        | DecBasis {elems, ...} =>
            let
              fun doElem ((mlbCache, basis, asts), {basexp, ...}) =
                let
                  val (mlbCache, basis', asts') =
                    doBasexp ctx (mlbCache, basis, basexp)
                in
                  (mlbCache, basis', asts' @ asts)
                end
            in
              Seq.iterate doElem (mlbCache, basis, []) elems
            end

        | DecLocalInEnd {basdec1, basdec2, ...} =>
            let
              (** create new scopes to then later pop and reconstruct, which
                * effectively hides any exports of basdec1
                *)
              val basis = newScope basis
              val (mlbCache, basis, asts1) =
                doBasdec ctx (mlbCache, basis, basdec1)
              val basis = newScope basis
              val (mlbCache, basis, asts2) =
                doBasdec ctx (mlbCache, basis, basdec2)
              val {old = basis, popped = newBasis} = popScope basis
              val {old = basis, ...} = popScope basis
            in
              (mlbCache, mergeBases (basis, newBasis), asts2 @ asts1)
            end

        | DecAnn {basdec, ...} => doBasdec ctx (mlbCache, basis, basdec)

        | _ => (mlbCache, basis, [])


      and doBasexp ctx (mlbCache, basis, basexp) =
        case basexp of
          BasEnd {basdec, ...} => doBasdec ctx (mlbCache, basis, basdec)

        | LetInEnd {basdec, basexp, ...} =>
            let
              (** create new scopes to then later pop and reconstruct, which
                * effectively hides any exports of basdec
                *)
              val basis = newScope basis
              val (mlbCache, basis, asts1) =
                doBasdec ctx (mlbCache, basis, basdec)
              val basis = newScope basis
              val (mlbCache, basis, asts2) =
                doBasexp ctx (mlbCache, basis, basexp)
              val {old = basis, popped = newBasis} = popScope basis
              val {old = basis, ...} = popScope basis
            in
              (mlbCache, mergeBases (basis, newBasis), asts2 @ asts1)
            end

        | _ => (mlbCache, basis, [])


      fun topLevelError msg =
        raise Error.Error
          { header = "FILE ERROR"
          , content =
              [Error.Paragraph (msg ^ ": " ^ FilePath.toUnixPath mlbPath)]
          }

      val emptyCache = FilePathDict.empty

      val initialBasis = if skipBasis then initialTopLevelBasis else emptyBasis

      val (_, _, asts) =
        doMLB {parents = [], dir = FilePath.fromUnixPath "."}
          (emptyCache, initialBasis, mlbPath, topLevelError)
    in
      Seq.fromRevList asts
    end

end
