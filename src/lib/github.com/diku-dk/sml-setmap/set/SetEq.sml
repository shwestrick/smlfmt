structure SetEq :> SET_COMPARE where type cmp = bool =
struct

type cmp = bool

type 'a t = 'a list

fun empty () : 'a t = nil

fun singleton e = [e]

fun size (s: 'a t) =
    List.length s

fun isEmpty (s: 'a t) =
    List.null s

fun member eq (e, s: 'a t) =
    List.exists (fn v => eq (v,e)) s

fun fold f e (s: 'a t) =
    List.foldl f e s

fun eq eqe (a, b) =
    fold (fn (x,acc) => acc andalso member eqe (x,a))
         (size a = size b) b

fun list (s: 'a t) = s

fun insert eq (e, s: 'a t) =
    if member eq (e,s) then s else e::s

fun fromList eq (es: 'a list) =
    List.foldl (fn (e,s) => insert eq (e,s)) (empty ()) es

fun remove eq (e, s: 'a t) =
    if member eq (e,s) then
      let fun rem (nil,acc) = rev acc
            | rem (x::xs,acc) = if eq (e,x) then (rev acc)@xs
                                else rem(xs,x::acc)
      in rem(s,nil)
      end
    else s

fun difference eq (a: 'a t, b: 'a t) =
    if isEmpty b then a
    else fold (fn (x,s) => remove eq(x,s)) a b

fun intersect eq (a: 'a t, b: 'a t) =
    if isEmpty a then a
    else fold (fn (x,s) => if member eq (x,b) then
                             insert eq (x,s)
                           else s)
              (empty ()) a

fun union eq (a: 'a t, b: 'a t) =
    if isEmpty b then a
    else fold (fn (x,s) => insert eq (x,s)) a b

fun partition p (s: 'a t) =
    List.partition p s

fun app f (s: 'a t) = List.app f s

end
