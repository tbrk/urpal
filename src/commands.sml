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

structure Commands : COMMANDS
=
let structure GO  = GetOpt
          and O   = Option
          and TIO = TextIO
in
struct
  type options = {inputfile: string option, outputfile: string option}
  val defaults = {inputfile=NONE, outputfile=NONE}

  datatype command = ScriptFile of string
                   | ScriptText of string
                   | ScriptTerminal
                   | ShowConfig
                   | ConfigFile of string
                   | ConfigText of string
                   | TestFlip

  datatype args = ShowUsage
                | ShowVersion
                | InputFile of string
                | OutputFile of string
                | Command of command
                | Ignore

  fun showError msg = TIO.output (TIO.stdErr,
        String.concat [Settings.progName, ": ", msg, "\n"])

  fun filterCommand (Command cmd) = SOME cmd
    | filterCommand _             = NONE

  val optionList = [
      {short="h", long=["help"], desc=GO.NoArg (fn ()=>ShowUsage),
       help="show this summary of command line options"},

      {short="v", long=["version"],
       desc=GO.NoArg (fn ()=>ShowVersion),
       help="show the version number"},

      {short="i", long=["input"],
       desc=GO.ReqArg (InputFile, "path"),
       help="Uppaal xml file to use for input"},

      {short="o", long=["output"],
       desc=GO.ReqArg (OutputFile, "path"),
       help="Path to file which should be overwritten with results"},

      {short="e", long=["eval"],
       desc=GO.ReqArg (fn t=>Command (ScriptText t), "script"),
       help="Evaluate script expressions directly."},

      {short="f", long=["scriptfile"],
       desc=GO.ReqArg (fn f=>Command (ScriptFile f), "path"),
       help="Evaluate script commands from a file."},

      {short="t", long=["terminal"],
       desc=GO.NoArg (fn ()=>Command (ScriptTerminal)),
       help="Invoke interactive terminal."},

      {short="c", long=["config"],
       desc=GO.ReqArg (fn f=>Command (ConfigFile f), "path"),
       help="Path to a configuration file."},

      {short="s", long=["set"],
       desc=GO.ReqArg (fn t=>Command (ConfigText t), "configtext"),
       help="Specify configuration settings directly."},

      {short="", long=["showconfig"],
       desc=GO.NoArg (fn ()=> Command ShowConfig),
       help="Write all settings to standard output"},

      {short="", long=["testflip"],
       desc=GO.NoArg (fn ()=>Command TestFlip),
       help="Test template flipping."}
    ]

  fun warningIfUsed NONE     = ()
    | warningIfUsed (SOME f) = showError ("file ignored '"^f^"'")

  fun processOptions (arg, opt as {inputfile=inf, outputfile=outf}) =
      case arg of
        ShowUsage        => opt before (
                              TextIO.print (GO.usageInfo {
                                header=(Settings.progName^" "^
                                        Settings.version),
                                options=optionList});
                              TextIO.print "\n")
      | ShowVersion      => opt before TextIO.print (Settings.progName ^ " " ^
                                                     Settings.version  ^ "\n")
      | InputFile f      => {inputfile=SOME f,outputfile=outf}
                            before warningIfUsed inf
      | OutputFile f     => {outputfile=SOME f, inputfile=inf}
                            before warningIfUsed outf
      | Command com      => opt
      | Ignore           => opt

  fun wrapFiles []       = []
    | wrapFiles (f::fs)  = InputFile f::wrapFiles' fs
  and wrapFiles' []      = []
    | wrapFiles' (f::fs) = OutputFile f::wrapFiles fs

  fun showRequest ShowUsage   = true
    | showRequest ShowVersion = true
    | showRequest _           = false

  fun processCommands args = let
      val (ops, files) = GO.getOpt {argOrder=GO.Permute,
                                    options=optionList,
                                    errFn=showError} args
      val options = foldl processOptions defaults (ops @ wrapFiles files)
      val commands = List.mapPartial filterCommand ops
    in
      if null commands andalso not (List.exists showRequest ops)
      then ([ScriptTerminal], options) else (commands, options)
    end

end
end

