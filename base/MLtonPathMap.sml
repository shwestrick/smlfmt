(** Copyright (c) 2021 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure MLtonPathMap :>
sig
  type pathmap = (string * FilePath.t) list
  type t = pathmap

  val getPathMap: unit -> pathmap
  val fromString: string -> pathmap
  val lookup: pathmap -> string -> FilePath.t option
  val expandPath: pathmap -> FilePath.t -> FilePath.t
end =
struct

  type pathmap = (string * FilePath.t) list
  type t = pathmap

  (** A bit of a hack... *)


  fun fromString str =
    let
      val lines = String.tokens (fn c => c = #"\n") str
      fun parseLine ln =
        case String.tokens Char.isSpace ln of
          [key, value] => SOME (key, FilePath.fromUnixPath value)
        | _ => NONE
    in
      List.mapPartial parseLine lines
    end


  fun getPathMap () =
    let
      open MLton.Process

      val p = create
        { path = "/usr/local/bin/mlton"
        , env = NONE
        , args = ["-show", "path-map"]
        , stderr = Param.self
        , stdin = Param.null
        , stdout = Param.pipe
        }

      val output = TextIO.inputAll (Child.textIn (getStdout p))
    in
      fromString output
    end
    handle _ => []


  fun lookup pathmap key =
    case pathmap of
      [] => NONE
    | (k, v) :: pathmap' =>
        if key = k then SOME v else lookup pathmap' key


  fun expandPath pathmap path =
    let
      fun toKey field =
        if String.isPrefix "$(" field andalso String.isSuffix ")" field then
          SOME (String.substring (field, 2, String.size field - 3))
        else
          NONE

      fun expandField field =
        case toKey field of
          NONE => [field]
        | SOME key =>
            case lookup pathmap key of
              NONE => [field]
            | SOME newpath => expand (FilePath.toFields newpath)

      and expand (fields: string list) : string list =
        List.concat (List.map expandField fields)

    in
      FilePath.fromFields (expand (FilePath.toFields path))
    end

end
