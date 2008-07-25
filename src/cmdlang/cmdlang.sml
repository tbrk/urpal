(* $Id$ *)

structure CmdLang : CMD_LANG = struct

  structure LrVals = CmdLangLrValsFn (structure FilePos = FilePos
                                      structure Token = LrParser.Token
                                      structure CmdEnv = CmdEnv)
  structure Lex    = CmdLangLexFn (structure FilePos = FilePos
                                   structure Tokens = LrVals.Tokens)
  structure Parser = JoinWithArg(structure LrParser = LrParser
                                 structure ParserData = LrVals.ParserData
                                 structure Lex = Lex)

  exception Failure;

  fun parse rdr (env, ostrm) = let
      val prError   = FilePos.error (Settings.progName^":")

      val strm = ref ostrm
      fun read _ = case rdr (!strm) of
                     NONE => ""
                   | SOME (s, strm') => s before strm := strm'

      val lexstream = Parser.makeLexer read (FilePos.newstate ())

      val tokenEOF = LrVals.Tokens.EOF (FilePos.zero, FilePos.zero)
      val tokenEOL = LrVals.Tokens.SEMICOLON (FilePos.zero, FilePos.zero)

      fun skipLine strm = let
            val (next, strm') = Parser.Stream.get strm
          in
            if Parser.sameToken (next, tokenEOF)
               orelse Parser.sameToken (next, tokenEOL)
            then strm' else skipLine strm'
          end


      fun doLoop (env, strm) = let
            val (next, strm') = Parser.Stream.get strm
          in
            if Parser.sameToken (next, tokenEOF) then SOME env
            else if Parser.sameToken (next, tokenEOL)
                 then loop ((CmdLoop.Continue, env), strm')
                 else loop (Parser.parse (0, strm, prError, env)
                         handle Parser.ParseError =>
                            ((CmdLoop.Continue, env), skipLine strm))
          end

      and loop ((CmdLoop.Stop, env), _) = SOME env
        | loop ((CmdLoop.Abort, _), _) = NONE
        | loop ((CmdLoop.Continue, env), strm) = doLoop (env, strm)
        | loop ((CmdLoop.Fail, env), strm) = if Settings.exitOnFail ()
                                             then raise Failure
                                             else doLoop (env, strm)

    in loop ((CmdLoop.Continue, env), lexstream) end

end

