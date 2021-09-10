(** Copyright (c) 2021 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure MLBParser:
sig
  type ('a, 'b) parser = ('a, 'b) ParserCombinators.parser
  type tokens = MLBToken.t Seq.t

  val basdec: tokens -> (int, MLBAst.basdec) parser
  val basexp: tokens -> (int, MLBAst.basexp) parser

  val parse: Source.t -> MLBAst.t
end =
struct

  type ('a, 'b) parser = ('a, 'b) ParserCombinators.parser
  type tokens = MLBToken.t Seq.t

  fun check_ toks f i =
    i < Seq.length toks andalso f (Seq.nth toks i)

  fun isReserved_ toks rc i =
    check_ toks (fn t =>
      case MLBToken.getClass t of
        MLBToken.Reserved rc' => rc = rc'
      | _ => false)
    i

  fun isSMLReserved_ toks rc i =
    check_ toks (fn t =>
      case MLBToken.getClass t of
        MLBToken.SML (Token.Reserved rc') => rc = rc'
      | _ => false)
    i


  fun nyi_ toks fname i =
    if i >= Seq.length toks then
      raise ParserUtils.Error
        { header = "ERROR: NOT YET IMPLEMENTED"
        , pos = MLBToken.getSource (Seq.nth toks (Seq.length toks - 1))
        , what = "Unexpected EOF after token."
        , explain = SOME ("(TODO: see parser " ^ fname ^ ")")
        }
    else if i >= 0 then
      raise ParserUtils.Error
        { header = "ERROR: NOT YET IMPLEMENTED"
        , pos = MLBToken.getSource (Seq.nth toks i)
        , what = "Unexpected token."
        , explain = SOME ("(TODO: see parser " ^ fname ^ ")")
        }
    else
      raise Fail ("Bug in parser " ^ fname ^ ": position out of bounds??")


  fun parse_SMLReserved toks rc i =
    if isSMLReserved_ toks rc i then
      (i+1, Seq.nth toks i)
    else
      ParserUtils.error
        { pos = MLBToken.getSource (Seq.nth toks i)
        , what =
            "Unexpected token. Expected to see "
            ^ "'" ^ Token.reservedToString rc ^ "'"
        , explain = NONE
        }


  fun basexp toks start =
    nyi_ toks "basexp" start


  fun basdec toks start =
    let
      val numToks = Seq.length toks
      fun tok i = Seq.nth toks i
      fun check f i = check_ toks f i
      fun isReserved rc i = isReserved_ toks rc i
      fun isSMLReserved rc i = isSMLReserved_ toks rc i


      (** not yet implemented *)
      fun nyi fname i = nyi_ toks fname i


      fun makeSMLPath pathtok pathstr =
        MLBAst.DecPathSML
          { token = pathtok
          , path = FilePath.fromUnixPath pathstr
          }

      fun makeMLBPath pathtok pathstr =
        MLBAst.DecPathMLB
          { token = pathtok
          , path = FilePath.fromUnixPath pathstr
          }


      (**  "path.{sml,mlb,...}"
        * ^
        *
        * TODO: this is a bit of a mess. Some repeated but slightly different
        * behavior from the lexer, which already distinguishes between SML
        * and MLB paths for unquoted path strings. Perhaps the lexer should be
        * simplified, and the quoted/unquoted path handling logic should be
        * unified?
        *)
      fun parse_decPathFromString i =
        let
          val thisSrc = MLBToken.getSource (tok i)
          val n = Source.length thisSrc
          val _ =
            if
              n >= 2 andalso
              Source.nth thisSrc 0 = #"\"" andalso
              Source.nth thisSrc (n - 1) = #"\""
            then
              ()
            else
              raise Fail "MLBParser bug! see parse_decPathFromString: fail 1"

          val pathstr = Source.toString (Source.slice thisSrc (1, n-2))
        in
          case MLBToken.makePathFromSourceString thisSrc pathstr of
            NONE =>
              ParserUtils.error
                { pos = MLBToken.getSource (tok i)
                , what = "Missing or invalid file extension in path."
                , explain = SOME "Valid extensions are: .mlb .sml .sig .fun"
                }
          | SOME pathtok =>
              if MLBToken.isSMLPath pathtok then
                (i+1, makeSMLPath pathtok pathstr)
              else if MLBToken.isMLBPath pathtok then
                (i+1, makeMLBPath pathtok pathstr)
              else
                raise Fail "MLBParser bug! see parse_decPathFromString: fail 2"
        end


      (** basis basid = basexp [and ...]
        *      ^
        *)
      fun parse_decBasis basiss i =
        nyi "parse_decBasis" i


      (** ann "annotation" [...] in basdec end
        *    ^
        *)
      fun parse_decAnn annn i =
        nyi "parse_decAnn" i


      (** open basid ... basid
        *     ^
        *)
      fun parse_decOpen annn i =
        nyi "parse_decOpen" i


      fun parse_decStructure structuree i =
        nyi "parse_decStructure" i
      fun parse_decSignature signaturee i =
        nyi "parse_decSignature" i
      fun parse_decFunctor functorr i =
        nyi "parse_decFunctor" i


      (** local basdec in basdec end
        *      ^
        *)
      fun parse_decLocal locall i =
        let
          val (i, basdec1) = parse_dec i
          val (i, inn) = parse_SMLReserved toks Token.In i
          val (i, basdec2) = parse_dec i
          val (i, endd) = parse_SMLReserved toks Token.End i
        in
          ( i
          , MLBAst.DecLocalInEnd
              { locall = locall
              , basdec1 = basdec1
              , inn = inn
              , basdec2 = basdec2
              , endd = endd
              }
          )
        end


      and parse_exactlyOneDec i =
        if check MLBToken.isSMLPath i then
          ( i+1
          , makeSMLPath (tok i) (Source.toString (Token.getSource (tok i)))
          )
        else if check MLBToken.isMLBPath i then
          ( i+1
          , makeMLBPath (tok i) (Source.toString (Token.getSource (tok i)))
          )
        else if check MLBToken.isStringConstant i then
          parse_decPathFromString i
        else if isReserved MLBToken.Basis i then
          parse_decBasis (tok i) (i+1)
        else if isReserved MLBToken.Ann i then
          parse_decAnn (tok i) (i+1)
        else if isSMLReserved Token.Open i then
          parse_decOpen (tok i) (i+1)
        else if isSMLReserved Token.Local i then
          parse_decLocal (tok i) (i+1)
        else if isSMLReserved Token.Structure i then
          parse_decStructure (tok i) (i+1)
        else if isSMLReserved Token.Signature i then
          parse_decSignature (tok i) (i+1)
        else if isSMLReserved Token.Functor i then
          parse_decFunctor (tok i) (i+1)
        else
          ParserUtils.error
            { pos = MLBToken.getSource (tok i)
            , what = "Unexpected token."
            , explain = SOME "Expected beginning of basis declaration."
            }


      and parse_dec i =
        let
          fun parse_maybeSemicolon i =
            if isSMLReserved Token.Semicolon i then
              (i+1, SOME (tok i))
            else
              (i, NONE)

          fun continue i =
            check MLBToken.isBasDecStartToken i

          (** While we see a basdec start-token, parse pairs of
            *   (dec, semicolon option)
            *)
          val (i, basdecs) =
            ParserCombinators.zeroOrMoreWhile
              continue
              (ParserCombinators.two
                ( parse_exactlyOneDec
                , parse_maybeSemicolon
                ))
              i

          fun makeDecMultiple () =
            MLBAst.DecMultiple
              { elems = Seq.map #1 basdecs
              , delims = Seq.map #2 basdecs
              }

          val result =
            case Seq.length basdecs of
              0 =>
                MLBAst.DecEmpty
            | 1 =>
                let
                  val (dec, semicolon) = Seq.nth basdecs 0
                in
                  if isSome semicolon then
                    makeDecMultiple ()
                  else
                    dec
                end
            | _ =>
                makeDecMultiple ()
        in
          (i, result)
        end

    in
      parse_dec start
    end


  fun parse src =
    let
      val toksWithComments = MLBLexer.tokens src
      val toks = Seq.filter (not o MLBToken.isComment) toksWithComments

      val (i, basdec) = basdec toks 0

      val _ =
        if i >= Seq.length toks then () else
        ParserUtils.error
          { pos = MLBToken.getSource (Seq.nth toks i)
          , what = "Unexpected token."
          , explain = SOME "Invalid start of basis declaration!"
          }
    in
      MLBAst.Ast basdec
    end

end
