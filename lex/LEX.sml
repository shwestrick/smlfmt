signature LEX =
sig
  val tokens: Source.t -> Token.t Seq.t
end
