val x =
  ( 1: int
  , Array.modify (~ : int -> int) (Array.array (10, 100))
  , Int.toString 1
  , 5 - 6 * 7 + 8 div 9
  , op+ (100, 1000)
  , List.filter (fn x => x mod 2 = 0) (List.tabulate (100, fn i => i))
  , fn 0 => 1 | 1 => 2 | 2 => 3 | 3 => 4 | 4 => 5 | _ => ~1
  )
