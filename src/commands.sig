(* $Id$

   20070815 T. Bourke
     Original code. Process options from the command line.

 *)

signature COMMANDS =
sig
  type options = {inputfile: string option,
                  outputfile: string option}

  datatype command = ScriptFile of string
                   | ScriptText of string
                   | ScriptTerminal
                   | ConfigFile of string
                   | ConfigText of string
                   | ShowConfig
                   | TestFlip

  val processCommands : string list -> command list * options
end

