(* $Id$
 *
 * Copyright (c) 2008 Timothy Bourke (University of NSW and NICTA)
 * All rights reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the "BSD License" which is distributed with the
 * software in the file LICENSE.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the BSD
 * License for more details.
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

