(* $Id$ *)

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

  fun execute (prog, args) =
      Proc {exec    = String.concatWith " " (prog::args),
            infile  = OS.FileSys.tmpName (),
            outfile = OS.FileSys.tmpName () }

  fun textOutstreamOf (Proc {outfile, ...}) = TextIO.openOut outfile
  fun textInstreamOf  (Proc {exec, infile, outfile}) = let
      val _ = OS.Process.system (concat [exec, " -o ", infile, " ", outfile])
    in TextIO.openIn infile end

  fun reap (Proc {infile, outfile, ...}) = (OS.FileSys.remove infile;
                                            OS.FileSys.remove outfile;
                                            OS.Process.success)

end

