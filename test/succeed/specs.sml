signature FOO =
  sig
    val x : int
    and y : string

    type foo
    and bar
    and 'a baz
    and ('a, 'b) bat

    eqtype 'a functions
    and are
    and ('a, 'b) values

    datatype 'a foo = A
    and 'a bar = A of int
    and 'a bat = A of int | B of string
    and karl = Crary
    and bob = Harper of int
    and umut = Acar of string | Diderot of bool
    and ('a, 'b) abstract = Nonsense
    and ('a, 'b) morphism = Commuting of 'a * 'b
    and ('a, 'b) category = Endofunctor of monad | Adjunction of 'a * 'b

    datatype foo = datatype foo2
    datatype foo = datatype A.B.foo
    datatype foo = datatype A.B.LongNameHereGoesToSomething.foo
  end
