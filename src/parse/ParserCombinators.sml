(** Copyright (c) 2020-2021 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure ParserCombinators:
sig
  type ('state, 'result) parser = 'state -> ('state * 'result)
  type 'state peeker = 'state -> bool

  val extra: Token.t Seq.t -> (int, Ast.extra) parser

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

  val oneOrMoreDelimitedByReservedWithExtra:
    Token.t Seq.t
    -> { parseElem: (int, 'a) parser
       , delim: Token.reserved
       }
    -> (int, { elems: 'a Seq.t
             , delims: Token.t Seq.t
             , extra_between: Ast.extra Seq.t}) parser

  val two:
    ('s, 'a) parser * ('s, 'b) parser
    -> ('s, ('a * 'b)) parser

  val zeroOrMoreWhile:
    's peeker
    -> ('s, 'a) parser
    -> ('s, 'a Seq.t) parser

  val oneOrMoreWhile:
    's peeker
    -> ('s, 'a) parser
    -> ('s, 'a Seq.t) parser

end =
struct

  type ('state, 'result) parser = 'state -> ('state * 'result)
  type 'state peeker = 'state -> bool


  fun extra toks start =
    let
      fun loop acc i =
        if
          i >= Seq.length toks
          orelse
          not (Token.isCommentOrWhitespace (Seq.nth toks i))
        then
          (i, Seq.fromRevList acc)
        else
          loop (Seq.nth toks i :: acc) (i+1)
    in
      loop [] start
    end


  fun zeroOrMoreDelimitedByReserved toks {parseElem, delim, shouldStop} i =
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


  fun oneOrMoreDelimitedByReservedWithExtra toks {parseElem, delim} i =
    let
      val numToks = Seq.length toks
      fun tok i = Seq.nth toks i
      fun check f i = i < numToks andalso f (tok i)
      fun isReserved rc = check (fn t => Token.Reserved rc = Token.getClass t)

      fun loop elems delims (extras: Ast.extra list) i =
        let
          val (i, elem) = parseElem i
          val elems = elem :: elems
          val (iMaybe, extra1) = extra toks i
        in
          if isReserved delim iMaybe then
            let
              val (i, delim) = (iMaybe+1, tok iMaybe)
              val (i, extra2) = extra toks i
            in
              loop elems (delim :: delims) (extra2 :: extra1 :: extras) i
            end
          else
            (** if we don't find a delim, back out on the extra items too. *)
            (i, elems, delims, extras)
        end

      val (i, elems, delims, extras) = loop [] [] [] i
    in
      ( i
      , { elems = Seq.fromRevList elems
        , delims = Seq.fromRevList delims
        , extra_between = Seq.fromRevList extras
        }
      )
    end


  fun oneOrMoreDelimitedByReserved toks stuff i =
    let
      val (i, {elems, delims, ...}) =
        oneOrMoreDelimitedByReservedWithExtra toks stuff i
    in
      (i, {elems=elems, delims=delims})
    end


  fun two (p1, p2) state =
    let
      val (state, elem1) = p1 state
      val (state, elem2) = p2 state
    in
      (state, (elem1, elem2))
    end


  fun zeroOrMoreWhile continue parse state =
    let
      fun loop elems state =
        if not (continue state) then (state, elems) else
        let
          val (state, elem) = parse state
          val elems = elem :: elems
        in
          loop elems state
        end

      val (state, elems) = loop [] state
    in
      (state, Seq.fromRevList elems)
    end


  fun oneOrMoreWhile continue parse state =
    let
      fun loop elems state =
        let
          val (state, elem) = parse state
          val elems = elem :: elems
        in
          if not (continue state) then
            (state, elems)
          else
            loop elems state
        end

      val (state, elems) = loop [] state
    in
      (state, Seq.fromRevList elems)
    end

end
