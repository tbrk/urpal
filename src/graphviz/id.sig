(* $Id$ *)

signature ID =
sig
  type t

  val fromString : string -> t option
  val toString   : t -> string
  val compare    : t * t -> order
end

