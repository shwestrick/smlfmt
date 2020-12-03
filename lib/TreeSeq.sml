structure TreeSeq =
struct
  structure Seq = ArraySequence

  datatype 'a t =
    Leaf
  | Elem of 'a
  | Flat of 'a Seq.t
  | Node of int * 'a t * 'a t

  type 'a seq = 'a t
  type 'a ord = 'a * 'a -> order
  datatype 'a listview = NIL | CONS of 'a * 'a seq
  datatype 'a treeview = EMPTY | ONE of 'a | PAIR of 'a seq * 'a seq

  exception Range
  exception Size
  exception NYI

  fun length Leaf = 0
    | length (Elem _) = 1
    | length (Flat s) = Seq.length s
    | length (Node (n, _, _)) = n

  fun append (t1, t2) = Node (length t1 + length t2, t1, t2)

  fun toArraySeq t =
    let
      val a = ForkJoin.alloc (length t)
      fun put offset t =
        case t of
          Leaf => ()
        | Elem x => Array.update (a, offset, x)
        | Flat s => Seq.applyIdx s (fn (i, x) => Array.update (a, offset+i, x))
        | Node (n, l, r) =>
            let
              fun left () = put offset l
              fun right () = put (offset + length l) r
            in
              if n <= 4096 then
                (left (); right ())
              else
                (ForkJoin.par (left, right); ())
            end
    in
      put 0 t;
      ArraySlice.full a
    end

  fun fromArraySeq a = Flat a

  fun empty () = Leaf
  fun singleton x = Elem x
  val $ = singleton

  fun nth _ = raise NYI
  fun toList _ = raise NYI
  fun toString _ = raise NYI
  fun equal _ = raise NYI

  fun empty _ = raise NYI
  fun tabulate _ = raise NYI
  fun fromList _ = raise NYI

  fun rev _ = raise NYI
  fun flatten _ = raise NYI

  fun map _ = raise NYI
  fun zip _ = raise NYI
  fun zipWith _ = raise NYI
  fun zipWith3 _ = raise NYI

  fun filter _ = raise NYI
  fun filterSome _ = raise NYI
  fun filterIdx _ = raise NYI

  fun enum _ = raise NYI
  fun mapIdx _ = raise NYI
  fun update _ = raise NYI
  fun inject _ = raise NYI

  fun subseq _ = raise NYI
  fun take _ = raise NYI
  fun drop _ = raise NYI
  fun splitHead _ = raise NYI
  fun splitMid _ = raise NYI

  fun iterate _ = raise NYI
  fun iteratePrefixes _ = raise NYI
  fun iteratePrefixesIncl _ = raise NYI
  fun reduce _ = raise NYI
  fun scan _ = raise NYI
  fun scanIncl _ = raise NYI

  fun sort _ = raise NYI
  fun merge _ = raise NYI
  fun collect _ = raise NYI
  fun collate _ = raise NYI
  fun argmax _ = raise NYI

  fun % _ = raise NYI

  fun force _ = raise NYI

  fun foreach _ = raise NYI
  fun foreachG _ = raise NYI

end
