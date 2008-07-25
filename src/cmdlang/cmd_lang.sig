(* $Id$ *)

signature CMD_LANG = sig

  exception Failure

  val parse : (string, 'strm) StringCvt.reader
              -> CmdEnv.t * 'strm -> CmdEnv.t option

end

