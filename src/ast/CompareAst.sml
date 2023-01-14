(** Copyright (c) 2023 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure CompareAst:
sig
  datatype result = Identical | Different of {description: string}
  val compare: Ast.t * Ast.t -> result
end =
struct

  datatype result = Identical | Different of {description: string}

  fun compare (ast1, ast2) =
    raise Fail "CompareAst not yet implemented..."

end
