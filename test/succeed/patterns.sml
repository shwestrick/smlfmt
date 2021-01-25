val ((x)) = "hello"
val () = ()
val "hello" = "hello"
val ("hello") = "hello"
val (_, ("world")) = ("hello", "world")
val ( (a, b, c)
    , d
    , (e, f)
    , whyIsThisParameterNamedSoBig
    , (h, i, j, k, l, m, n, oo, p)
    ) =
  someLongFunctionName ()
val [(x), (1,2,3), "hello", Option.NONE] =
  [ 0, (1,2,3), "hello", NONE ]
