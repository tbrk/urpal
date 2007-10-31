(* $Id$

   20070822 T. Bourke
   Interface for attributes as seen from dot.
 *)

signature ATTRIBUTE =
sig
  type t

  val name     : t -> string
  val hasValue : t -> bool
  val value    : t -> string
end

