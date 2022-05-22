structure FindAllMLB:
sig
  val find: {root: FilePath.t} -> FilePath.t Seq.t
end =
struct

  fun find {root} =
    let
      fun loop acc hostpath =
        if OS.FileSys.isDir hostpath then
          (loopDir acc (hostpath, OS.FileSys.openDir hostpath)
            handle OS.SysErr _ => acc)
        else case OS.Path.ext hostpath of
          SOME "mlb" => FilePath.fromHostPath hostpath :: acc
        | _ => acc

      and loopDir acc (dirpath, dirstream) =
        case OS.FileSys.readDir dirstream of
          SOME name =>
            let
              val acc = loop acc (OS.Path.joinDirFile {dir=dirpath, file=name})
            in
              loopDir acc (dirpath, dirstream)
            end
        | NONE => acc

      val mlbList = loop [] (FilePath.toHostPath root)
    in
      Seq.fromRevList mlbList
    end

end
