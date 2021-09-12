(** Copyright (c) 2021 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure MLtonPathMap :>
sig
  type pathmap = (string * string) list
  type t = pathmap

  val getPathMap: unit -> pathmap
  val fromString: string -> pathmap
  val lookup: pathmap -> string -> string option
  val expandPath: pathmap -> FilePath.t -> FilePath.t
end =
struct

  type pathmap = (string * string) list
  type t = pathmap

  (** A bit of a hack... *)


  fun fromString str =
    let
      val lines = String.tokens (fn c => c = #"\n") str
      fun parseLine ln =
        case String.tokens Char.isSpace ln of
          [key, value] => SOME (key, value)
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


  fun tryExpandField (pathmap: pathmap) (field: string) =
    let
      val n = String.size field
      fun c i = String.sub (field, i)
      fun slice (i, j) = String.substring (field, i, j-i)


      fun findNextKeyStart (i: int) =
        if i >= n then
          NONE
        else if i+1 < n andalso c i = #"$" andalso c (i+1) = #"(" then
          SOME i
        else
          findNextKeyStart (i+1)


      fun findNextKeyEnd (i: int) =
        if i >= n then
          NONE
        else if c i = #")" then
          SOME (i+1)
        else
          findNextKeyEnd (i+1)


      (** Example:
        *   abc$(foo)def$(bar)...
        *                     ^
        *                     i
        *   acc is ["abc", X, "def", Y], but in reverse order,
        *   where X = replacement for foo and similarly Y for bar.
        *)
      fun loop acc i =
        if i >= n then acc else
        case findNextKeyStart i of
          NONE => slice (i, n) :: acc
        | SOME j =>
            case findNextKeyEnd (j+2) of
              NONE => slice (i, n) :: acc
            | SOME k =>
                let
                  val key = slice (j+2, k-1)
                in
                  case lookup pathmap key of
                    NONE => loop (slice (j, k) :: acc) k
                  | SOME v => loop (v :: acc) k
                end

      val expanded = String.concat (List.rev (loop [] 0))
    in
      if expanded = field then
        NONE
      else
        SOME expanded
    end


  fun expandPath pathmap path =
    let
      fun expandField field =
        case tryExpandField pathmap field of
          NONE => [field]
        | SOME field' =>
            expand (FilePath.toFields (FilePath.fromUnixPath field'))

      and expand (fields: string list) : string list =
        List.concat (List.map expandField fields)

    in
      FilePath.fromFields (expand (FilePath.toFields path))
    end

end
