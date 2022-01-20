fun pr s = print ("Json." ^ s ^ "\n")

fun itest s expected n =
    if n=expected then pr (s ^ ": OK")
    else pr(s ^ ": ERR - expected " ^ Int.toString expected ^ " but got " ^ Int.toString n)

fun stest s expected n =
    if n=expected then pr (s ^ ": OK")
    else pr(s ^ ": ERR - expected " ^ expected ^ " but got " ^ n)

fun rtest s expected n =
    if Real.toString n = Real.toString expected then pr (s ^ ": OK")
    else pr(s ^ ": ERR - expected " ^ Real.toString expected ^ " but got " ^ Real.toString n)

structure J  = Json

val j1 = "{\"a\":5, \"b\":true, \"c\":\"dfdf\", \"d\":[5, 34, 34, 2, \"sdffs\", 2], \"eee\":232.32}"
val () = stest "j1" j1 (J.toString (J.fromString j1))
val () = stest "j2" "dfdf" (J.getString (J.fromString j1) "c")

val () = stest "j3" "232.32"
               (case J.fromString j1 of J.OBJECT obj =>
                                        (case J.objLook obj "eee" of
                                             SOME (J.NUMBER n) => n
                                           | _ => "err1")
                                      | _ => "err2")

fun ppList ls = "[" ^ String.concatWith "," (map Int.toString ls) ^ "]"

val a1 = [1,34,2,4,4,5,45,45,45,34,23,23,32,3,23]
val () = stest "j4" (Int.toString (foldl op+ 0 a1))
               (Int.toString(J.foldlArray (fn (J.NUMBER x, a) =>
                                              (case Int.fromString x of
                                                   SOME i => i+a
                                                 | NONE => a)
                                            | (_,a) => a) 0 (J.fromString (ppList a1))))

fun js_sub (J.NUMBER x, a) =
    (case Int.fromString x of
         SOME i => i-a
       | NONE => a)
  | js_sub (_,a) = a

val () = stest "j5" (Int.toString (foldl op- 0 a1))
               (Int.toString(J.foldlArray js_sub 0 (J.fromString (ppList a1))))

val () = stest "j6" (Int.toString (foldr op- 0 a1))
               (Int.toString(J.foldrArray js_sub 0 (J.fromString (ppList a1))))

val () = stest "j7" (Int.toString (foldl op- 0 a1))
               (Int.toString(J.foldlArrayJson js_sub 0 (ppList a1)))

fun mkObj x = "{\"a\":1, \"b\":" ^ x ^ ", \"c\":\"3\"}"
fun mkArr xs = "[" ^ String.concatWith ", " xs ^ "]"
val j20 = mkArr [mkObj (mkArr ["2","4"]), mkObj (mkArr ["\"true\"","40.2"])]

val () = stest "j20" j20 (J.toString (J.fromString j20))

val j21 = mkObj(mkArr [mkObj (mkArr ["2","4"]), mkObj (mkArr ["\"true\"","40.2"])])

val () = stest "j21" j21 (J.toString (J.fromString j21))
