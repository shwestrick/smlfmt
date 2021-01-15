val x =
  ( 1: int
  , Array.modify (~ : int -> int) (Array.array (10, 100))
  , Int.toString 1
  , 5 - 6 * 7 + 8 div 9
  , op+ (100, 1000)
  )
