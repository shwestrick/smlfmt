structure ParserCombinators:
sig
  type ('state, 'result) parser = 'state -> ('state * 'result)
  type 'state peeker = 'state -> bool

  val zeroOrMoreDelimitedByReserved:
    Token.t Seq.t
    -> { parseElem: (int, 'a) parser
       , delim: Token.reserved
       , shouldStop: int peeker
       }
    -> (int, {elems: 'a Seq.t, delims: Token.t Seq.t}) parser

  val oneOrMoreDelimitedByReserved:
    Token.t Seq.t
    -> { parseElem: (int, 'a) parser
       , delim: Token.reserved
       }
    -> (int, {elems: 'a Seq.t, delims: Token.t Seq.t}) parser

end =
struct

  type ('state, 'result) parser = 'state -> ('state * 'result)
  type 'state peeker = 'state -> bool

  fun zeroOrMoreDelimitedByReserved
    toks {parseElem, delim, shouldStop} i =
    let
      val numToks = Seq.length toks
      fun tok i = Seq.nth toks i
      fun check f i = i < numToks andalso f (tok i)
      fun isReserved rc = check (fn t => Token.Reserved rc = Token.getClass t)

      fun loop elems delims i =
        if shouldStop i then
          (i, elems, delims)
        else
          let
            val (i, elem) = parseElem i
            val elems = elem :: elems
          in
            if isReserved delim i then
              loop elems (tok i :: delims) (i+1)
            else
              (i, elems, delims)
          end

      val (i, elems, delims) = loop [] [] i
    in
      ( i
      , { elems = Seq.fromRevList elems
        , delims = Seq.fromRevList delims
        }
      )
    end


    fun oneOrMoreDelimitedByReserved toks {parseElem, delim} i =
      let
        val numToks = Seq.length toks
        fun tok i = Seq.nth toks i
        fun check f i = i < numToks andalso f (tok i)
        fun isReserved rc = check (fn t => Token.Reserved rc = Token.getClass t)

        fun loop elems delims i =
          let
            val (i, elem) = parseElem i
            val elems = elem :: elems
          in
            if isReserved delim i then
              loop elems (tok i :: delims) (i+1)
            else
              (i, elems, delims)
          end

        val (i, elems, delims) = loop [] [] i
      in
        ( i
        , { elems = Seq.fromRevList elems
          , delims = Seq.fromRevList delims
          }
        )
      end

end
