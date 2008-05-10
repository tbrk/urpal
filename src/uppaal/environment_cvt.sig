(* $Id$ *)

signature ENVIRONMENT_CVT =
sig
  type env = Environment.env
  type outstream = TextIO.StreamIO.outstream

  val toStream         : outstream * env -> unit
  val templateToStream : outstream * env -> unit
end

