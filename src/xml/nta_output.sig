(* $Id$ *)

signature NTA_OUTPUT =
sig
  include NTA
  type outstream

  val output         : outstream -> nta -> unit
  val outputTemplate : outstream -> template -> unit
end

