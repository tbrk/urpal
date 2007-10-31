(* $Id$ *)

signature ENVIRONMENT_PP =
sig
  type pp_desc
   and stream

  type symbol = Atom.atom

  type env
   and scopetag

  val fromEnv    : env  -> pp_desc
  val fromEnv'   : env * scopetag  -> pp_desc

  val print    : stream -> pp_desc -> unit
end

