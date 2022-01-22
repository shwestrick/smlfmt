(** A simple implementation of URIs, as defined by
  *   https://datatracker.ietf.org/doc/html/rfc3986
  *
  * Copyright (c) 2022 Sam Westrick
  * See LICENSE file at the project root.
  *)
structure URI :>
sig
  type t
  type uri = t

  val fromString: string -> uri
  val toString: uri -> string

  val scheme: uri -> string
  val authority: uri -> string option
  val path: uri -> string
  val query: uri -> string option
  val fragment: uri -> string option

  val make:
    { scheme: string
    , authority: string option
    , path: string
    , query: string option
    , fragment: string option
    }
    -> uri
end =
struct

  fun isUnreserved c =
    Char.isHexDigit c orelse Char.contains "-._~" c

  fun isValidSchemeChar c =
    Char.isAlpha c orelse Char.isDigit c orelse Char.contains "+-." c

  datatype t =
    T of
      { scheme: string
      , authority: string option
      , path: string
      , query: string option
      , fragment: string option
      }

  type uri = t

  fun make x = T x

  fun scheme (T {scheme=x, ...}) = x
  fun authority (T {authority=x, ...}) = x
  fun path (T {path=x, ...}) = x
  fun query (T {query=x, ...}) = x
  fun fragment (T {fragment=x, ...}) = x


  fun fromString s =
    let
      val n = String.size s
      fun slice (i, j) = String.substring (s, i, j-i)
      fun char i =
        if i < n then SOME (String.sub (s, i)) else NONE
      fun str i len =
        if len < 0 orelse i+len > n then
          NONE
        else
          SOME (slice (i, i+len))

      (* i is index immediately after the % *)
      (* fun skip_percent_encoding i =
        case Option.map String.explode (str i 2) of
          SOME [a,b] =>
            if Char.isHexDigit a andalso Char.isHexDigit b then
              i+2
            else
              raise Fail "URI.fromString: percent encoding must only \
                         \contain hexadecimal digits"
        | _ =>
            raise Fail "URI.fromString: invalid percent encoding" *)


      fun parseScheme start =
        let
          fun unexpectedEnd () =
            raise Fail "URI.fromString: unexpected end of uri"

          fun loop i =
            case char i of
              SOME #":" => (slice (start, i), i+1)
            | SOME c =>
                if isValidSchemeChar c then
                  loop (i+1)
                else
                  raise Fail ("URI.fromString: scheme: contains invalid \
                              \character '" ^ Char.toString c ^ "'")
            | _ =>
                unexpectedEnd ()
        in
          (** The first character must be alpha *)
          case char start of
            SOME c =>
              if Char.isAlpha c then
                loop (start+1)
              else
                raise Fail "URI.fromString: scheme must begin with an \
                           \alpha character [a-zA-Z]"
          | _ =>
              unexpectedEnd ()
        end


      fun parseMaybeAuthority slashSlashIdx =
        let
          val start = slashSlashIdx+2   (* need to see "//" first *)

          fun endOfAuthority i =
            case char i of
              SOME c => Char.contains "/?#" c
            | NONE => true

          fun loop i =
            if endOfAuthority i then
              (slice (start, i), i)
            else
              loop (i+1)
        in
          case str slashSlashIdx 2 of
            SOME "//" =>
              let val (auth, i) = loop start
              in (SOME auth, i)
              end
          | _ =>
              (NONE, slashSlashIdx)
        end



      fun parsePath {hasAuthority: bool} start =
        let
          fun endOfPath i =
            case char i of
              SOME c => Char.contains "?#" c
            | NONE => true

          fun loop i =
            if endOfPath i then
              (slice (start, i), i)
            else
              loop (i+1)

          val (path, i) = loop start
        in
          if
            hasAuthority andalso path <> "" andalso String.sub (path, 0) <> #"/"
          then
            raise Fail ("URI.fromString: path '" ^ path ^ "' is invalid \
                        \(uri has authority, but \
                        \path starts with character other than '/'")
          else
            (path, i)
        end


      fun parseMaybeQuery qmarkIdx =
        let
          val start = qmarkIdx+1

          fun endOfQuery i =
            case char i of
              SOME c => c = #"#"
            | NONE => true

          fun loop i =
            if endOfQuery i then
              (slice (start, i), i)
            else
              loop (i+1)
        in
          case char qmarkIdx of
            SOME #"?" =>
              let val (q, i) = loop start
              in (SOME q, i)
              end

          | _ => (NONE, qmarkIdx)
        end


      fun parseMaybeFragment hashIdx =
        let
          val start = hashIdx+1

          fun endOfQuery i =
            not (Option.isSome (char i))

          fun loop i =
            if endOfQuery i then
              (slice (start, i), i)
            else
              loop (i+1)
        in
          case char hashIdx of
            SOME #"#" =>
              let val (frag, i) = loop start
              in (SOME frag, i)
              end

          | _ => (NONE, hashIdx)
        end


      val (scheme, i) = parseScheme 0
      val (authority, i) = parseMaybeAuthority i
      val (path, i) = parsePath {hasAuthority = Option.isSome authority} i
      val (query, i) = parseMaybeQuery i
      val (fragment, i) = parseMaybeFragment i
    in
      T { scheme = scheme
        , authority = authority
        , path = path
        , query = query
        , fragment = fragment
        }
    end


  fun toString (T {scheme, authority, path, query, fragment}) =
    scheme ^ ":"
    ^ (case authority of SOME a => "//" ^ a | NONE => "")
    ^ path
    ^ (case query of SOME q => "?" ^ q | NONE => "")
    ^ (case fragment of SOME f => "#" ^ f | NONE => "")


  val examples =
    [ "ftp://ftp.is.co.za/rfc/rfc1808.txt"
    , "http://www.ietf.org/rfc/rfc2396.txt"
    , "ldap://[2001:db8::7]/c=GB?objectClass?one#foobar"
    , "yes:?"
    , "mailto:John.Doe@example.com"
    , "news:comp.infosystems.www.servers.unix"
    , "tel:+1-816-555-1212"
    , "telnet://192.0.2.16:80/"
    , "urn:oasis:names:specification:docbook:dtd:xml:4.1.2"
    ]

end
