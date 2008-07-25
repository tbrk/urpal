(* $Id$ *)

structure CmdLoop = struct
  datatype t = Continue
             | Fail       (* command failed, may still continue *)
             | Stop
             | Abort
end
