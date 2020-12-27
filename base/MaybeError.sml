(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure MaybeError:
sig
  datatype ('a, 'err) t =
    Success of 'a
  | Error of 'err

  val andThen: ('a, 'err) t
            -> ('a -> ('b, 'err) t)
            -> ('b, 'err) t

end =
struct
  datatype ('a, 'err) t =
    Success of 'a
  | Error of 'err

  fun andThen maybeErr f =
    case maybeErr of
      Success x => f x
    | Error err => Error err
end
