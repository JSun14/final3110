module type Movable = sig
  type t
  val move: t -> t
end
