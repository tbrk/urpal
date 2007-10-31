(* $Id$ *)

structure Urpal :
sig
  val main : string * string list -> OS.Process.status
  val run: string -> OS.Process.status
end
=
let structure UP  = UppaalParse
          and UXML = UppaalXML
          and TIO  = TextIO
          and CMD  = Commands
in
  struct
  exception FailedCommand
        and NoInputFile

  fun abort ss = let in
      TIO.output (TIO.stdErr, String.concat (Settings.progName::":"::ss));
      TIO.output (TIO.stdErr, "\n");
      raise FailedCommand
    end

  fun readNta filename = let
      val uri = Uri.String2Uri filename
    in case UXML.parse uri
       of NONE      => abort [filename,
                          " is not a valid XML file or the dtd is invalid."]
        | SOME xnta => UP.parse (xnta, filename)
    end

  fun writeNta (filename, nta) = let
      fun makeStream (s, c) = (TIO.getOutstream s, c)
      val (strm, close) = makeStream (case filename of
                             NONE   => (TIO.stdOut, fn _=>())
                           | SOME n => (TIO.openOut n, TIO.StreamIO.closeOut))
    in ParsedNta.output strm nta; close strm end
    handle IO.Io {cause,...} => (Util.warn ["failed to write nta (",
                                            General.exnMessage cause, ")"])
                                before (raise FailedCommand)

  fun tryConfigFiles () = let
      fun tryDir (NONE, _, _) = NONE
        | tryDir (SOME d, subs, f) = let
              val dir=foldl (fn(f,d)=>OS.Path.joinDirFile{dir=d,file=f}) d subs
            in SOME (OS.Path.joinDirFile {dir=dir, file=f}) end

      fun loadFile NONE = ()
        | loadFile (SOME path) = let
          val ()= Util.debugOutline (fn()=>["seeking config file ",path,"..."])

          val inso = SOME (TextIO.getInstream (TextIO.openIn path))
                     before Util.debugOutline (fn()=>["found"])
                     handle IO.Io {cause, ...} => NONE before
                       Util.debugOutline (fn()=>["not found (",
                                                 General.exnMessage cause,")"])
        in
          ignore (Option.map
                    (SettingsRW.loadConfigFile TextIO.StreamIO.inputLine) inso)
        end

    in
      app loadFile [tryDir (Settings.prefix (), ["etc"], "urpalrc"),
                    tryDir (OS.Process.getEnv "HOME", [], ".urpalrc"),
                    SOME "urpalrc"]
    end

  fun testFlip (NONE, _) = abort ["no input file specified"]
    | testFlip (SOME inf,NONE) = TestTransFlip.runTest {input=inf,
                                                        output=TextIO.stdOut}
    | testFlip (SOME inf,SOME outf) = (let
           val ostrm = TextIO.openOut outf
         in
           TestTransFlip.runTest {input=inf, output=ostrm};
           TextIO.closeOut ostrm;
           OS.Process.success
         end
         handle IO.Io {cause, ...} => (Util.warn
           ["cannot write to '", outf, "': ",
            General.exnMessage cause]; OS.Process.failure))

  fun main (name, args) = let
      val _ = tryConfigFiles ()
      val (cmds, {inputfile, outputfile}) = CMD.processCommands args
      val _ = Settings.validate ()

      fun doCmd (CMD.ScriptFile filename, NONE)     = raise NoInputFile
        | doCmd (CMD.ScriptFile filename, SOME env) = let
              val ins = TextIO.openIn filename
            in
              CmdLang.parse TextIO.StreamIO.inputLine
                            (env, TextIO.getInstream ins)
            end

        | doCmd (CMD.ScriptText script, NONE)       = raise NoInputFile
        | doCmd (CMD.ScriptText script, SOME env)   =
              CmdLang.parse (fn""=>NONE|s=>SOME(s,"")) (env, script)

        | doCmd (CMD.ScriptTerminal, NONE)          = raise NoInputFile
        | doCmd (CMD.ScriptTerminal, SOME env)      = let
              fun getLine ins = (TextIO.print "> ";
                                 TextIO.StreamIO.inputLine ins)
            in
              CmdLang.parse getLine (env, TextIO.getInstream TextIO.stdIn)
            end

        | doCmd (CMD.ConfigFile p, envo) = (let
              val ins = TextIO.getInstream (TextIO.openIn p)
            in SettingsRW.loadConfigFile TextIO.StreamIO.inputLine ins;
               TextIO.StreamIO.closeIn ins;
               envo
            end
            handle IO.Io {cause, ...} => (Util.warn ["reading ",p," failed (",
                                            General.exnMessage cause, ")"];
                                          raise FailedCommand))

        | doCmd (CMD.ConfigText t, envo) = envo before
                SettingsRW.loadConfigFile (fn""=>NONE|s=>SOME(s,"")) t

        | doCmd (CMD.ShowConfig, envo)   = envo before
                                           Settings.saveConfigFile TextIO.stdOut
        | doCmd (CMD.TestFlip, _) = (Util.warn
                      ["The test flip command must be used by itself."];
                      raise FailedCommand)

    in
      case cmds of
        CMD.TestFlip::_ => testFlip (inputfile, outputfile)

      | _ => let
               val ntao      = Option.mapPartial readNta inputfile
               val inputEnvo = Option.map CmdEnv.fromNta ntao
               val outputfile= if isSome outputfile then outputfile
                                                    else inputfile
             in
               case foldl doCmd inputEnvo cmds of
                 NONE     => OS.Process.success
               | SOME env => (writeNta (outputfile, CmdEnv.toNta env);
                                        OS.Process.success)
             end
    end
    handle FailedCommand => OS.Process.failure
         | e => Util.abort ["uncaught exception:", General.exnMessage e]
    
    fun run args = main (Settings.progName, String.tokens Char.isSpace args)

  end
end

