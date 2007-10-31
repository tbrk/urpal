(* $Id$ *)

signature CMD_LANG = sig

  val parse : (string, 'strm) StringCvt.reader
              -> CmdEnv.t * 'strm -> CmdEnv.t option

end

