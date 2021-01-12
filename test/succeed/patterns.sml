val ((x)) = "hello"
val () = ()
val "hello" = "hello"
val ("hello") = "hello"
val (_, ("world")) = ("hello", "world")
val ( (a, b, c)
    , d
    , (e, f)
    , whyIsThisParameterNamedSoBig
    , (h, i, j, k, l, m, n, o, p)
    ) =
  someLongFunctionName ()
