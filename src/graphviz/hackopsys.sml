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

structure HackOpSys = struct

  (* Assumes the following caller behaviour:
   *    1. call execute
   *    2. call textOutstreamOf
   *    3. write all data to process
   *    4. call textInstreamOf
   *    5. read all data from process
   *    6. close both streams
   *    7. call reap
   *)

  datatype ('a, 'b) proc =
      Proc of { exec : string, infile : string, outfile : string }

  fun tmpName () = let
      val utn = OS.FileSys.tmpName ()
      val _  = Util.debugVeryDetailed (fn ()=>["unix tempname: ", utn])
      val wtn = OS.Path.fromUnixPath (OS.FileSys.tmpName ())
      val _  = Util.debugVeryDetailed (fn ()=>["win  tempname: ", wtn])
    in OS.Path.file utn end

  fun execute (prog, args) = let
      fun f #" " = "\\ " | f c = Char.toString c
      val exepath = String.translate f (OS.Path.toUnixPath prog)
    in
      Proc {exec    = String.concatWith " " (exepath::args),
            infile  = tmpName (),
            outfile = tmpName () }
    end

  fun textOutstreamOf (Proc {outfile, ...}) = TextIO.openOut outfile
  fun textInstreamOf  (Proc {exec, infile, outfile}) = let
      val exe = concat [exec, " -o ", infile, " ", outfile]
      val _   = Util.debugDetailed (fn ()=>["--system(", exe, ")"])
      val _   = OS.Process.system exe
    in TextIO.openIn infile end

  fun reap (Proc {infile, outfile, ...}) = (OS.FileSys.remove infile;
                                            OS.FileSys.remove outfile;
                                            OS.Process.success)

end

