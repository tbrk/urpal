(* $Id$ *)

structure CmdLoop = struct
  datatype t = Continue
             | Stop
             | Abort
end
