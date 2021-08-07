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

    structure Module : MODULE
    and ModuleNameAgain : REALLYLONGSIGNATURENAME
    and Stack :
      sig
        type 'a t

        val empty : 'a t
        val push : 'a -> 'a t -> 'a t
        val pop : 'a t -> ('a * 'a t) option
      end
    and Stack :
      sig
        type 'a t
      end where type 'a t = int

    include FOO
    include
      sig
        type 'a t

        val instance : 'a t
      end
    include
      sig
        type 'a t
      end where type 'a t = string
  end
