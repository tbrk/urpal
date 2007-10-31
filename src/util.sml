(* $Id$
 *)

structure Util :
sig
  val warn : string list -> unit
  val abort : string list -> 'a

  val debug        : (Settings.debug_priority * (unit -> string list)) -> unit

  val debugAll          : (unit -> string list) -> unit
  val debugVeryDetailed : (unit -> string list) -> unit
  val debugDetailed     : (unit -> string list) -> unit
  val debugOutline      : (unit -> string list) -> unit

  (* show borders *)
  val debugSect    : (Settings.debug_priority * (unit -> string list)) -> unit
  val debugSubsect : (Settings.debug_priority * (unit -> string list)) -> unit

  val debugIndent  : (Settings.debug_priority * (unit -> string list)) -> unit
  val debugOutdent : (Settings.debug_priority * (unit -> string list)) -> unit

end
=
let structure TIO = TextIO
in
struct
  val indent = ref 0
  val indentAmount = 3;

  val sect    = StringCvt.padLeft #"*" 71 "\n"
  val subsect = StringCvt.padLeft #"-" 71 "\n"

  fun warn msg = (TIO.output (TIO.stdErr,
                              String.concat (Settings.progName::":"::msg));
                  TIO.output (TIO.stdErr, "\n"))

  fun abort msg = (TIO.output (TIO.stdErr,
                              String.concat (Settings.progName::":"::msg));
                  TIO.output (TIO.stdErr, "\n");
                  OS.Process.exit (OS.Process.failure))

  fun print (indent, lazystrs) = case lazystrs ()
      of []    => ()
       | strs  => (TIO.output (TIO.stdErr, StringCvt.padLeft #" " indent "");
                   app (fn s=> TIO.output (TIO.stdErr, s)) strs;
                   TIO.output (TIO.stdErr, "\n"))

  fun debug (priority, lazystrs) =
      if Settings.showDebug (priority)
      then print (!indent, lazystrs) else ()

  fun debugSect (priority, lazystrs) =
      if Settings.showDebug (priority)
      then (TIO.output (TIO.stdErr, sect);
            print (!indent, lazystrs))
      else ()

  fun debugSubsect (priority, lazystrs) =
      if Settings.showDebug (priority)
      then (TIO.output (TIO.stdErr, subsect);
            print (!indent, lazystrs))
      else ()

  fun debugIndent (priority, lazystrs) =
      if Settings.showDebug (priority)
      then (print (!indent, lazystrs);
            indent := (!indent) + indentAmount)
      else ()

  fun debugOutdent (priority, lazystrs) =
      if Settings.showDebug (priority)
      then (indent := (!indent) - indentAmount;
            print (!indent, lazystrs))
      else ()

  fun debugAll ls          = debug (Settings.All, ls)
  fun debugVeryDetailed ls = debug (Settings.VeryDetailed, ls)
  fun debugDetailed ls     = debug (Settings.Detailed, ls)
  fun debugOutline ls      = debug (Settings.Outline, ls)
end
end

