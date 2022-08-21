(* already provided by the compiler *)
structure ForkJoin = ForkJoin

structure Concurrency =
struct
  val numberOfProcessors = MLton.Parallel.numberOfProcessors
  val cas = MLton.Parallel.compareAndSwap
  val casArray = MLton.Parallel.arrayCompareAndSwap
end

structure VectorExtra:
sig
  val unsafeFromArray: 'a array -> 'a vector
end =
struct
  open VectorExtra
end

structure ReadFile =
struct

  fun contentsSeq' reader filename =
    let
      val file = MPL.File.openFile filename
      val n = MPL.File.size file
      val arr = ForkJoin.alloc n
      val k = 10000
      val m = 1 + (n-1) div k
    in
      ForkJoin.parfor 1 (0, m) (fn i =>
        let
          val lo = i*k
          val hi = Int.min ((i+1)*k, n)
        in
          reader file lo (ArraySlice.slice (arr, lo, SOME (hi-lo)))
        end);
      MPL.File.closeFile file;
      ArraySlice.full arr
    end

  fun contentsSeq filename =
    contentsSeq' MPL.File.readChars filename

  fun contentsBinSeq filename =
    contentsSeq' MPL.File.readWord8s filename

  fun contents filename =
    let
      val chars = contentsSeq filename
    in
      CharVector.tabulate (ArraySlice.length chars,
        fn i => ArraySlice.sub (chars, i))
    end

end

structure GCStats:
sig
  val report: unit -> unit
end =
struct

  fun p name thing =
    print (name ^ ": " ^ thing () ^ "\n")

  fun report () =
    let
    in
      print ("======== GC Stats ========\n");
      p "local reclaimed" (LargeInt.toString o MPL.GC.localBytesReclaimed);
      p "num local" (LargeInt.toString o MPL.GC.numLocalGCs);
      p "local gc time" (LargeInt.toString o Time.toMilliseconds o MPL.GC.localGCTime);
      p "promo time" (LargeInt.toString o Time.toMilliseconds o MPL.GC.promoTime);
      p "internal reclaimed" (LargeInt.toString o MPL.GC.internalBytesReclaimed)
    end

end
