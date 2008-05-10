(* $Id$ *)
signature PARSED_NTA = sig
  include NTA_OUTPUT

  val freeTransitionNames : transition -> AtomSet.set
end
