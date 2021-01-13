val a = 5 : int
val b = Int.toString : int -> string
val c = List.nil : int list
val d = List.foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
val e = Array.update : 'a array * int * 'a -> unit
val f = Substring.splitl : (char -> bool)
                        -> Substring.substring
                        -> Substring.substring * Substring.substring
