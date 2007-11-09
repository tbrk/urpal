(* $Id$ *)

structure MCS51 :> MCS51 = struct
  
  structure Instruction = MCS51Instruction
  type program = (string option * Instruction.t) list

  structure LrVals = MCS51LrValsFn (structure FilePos = FilePos
                                    structure Token = LrParser.Token
                                    structure ASM = Instruction)
  structure Lex    = MCS51LexFn (structure FilePos = FilePos
                                 structure Tokens = LrVals.Tokens
                                 structure ASM = Instruction)
  structure Parser = JoinWithArg(structure LrParser = LrParser
                                 structure ParserData = LrVals.ParserData
                                 structure Lex = Lex)

  fun parse rdr ostrm = let
      val prError   = FilePos.error (Settings.progName^":")

      val strm = ref ostrm
      fun read _ = case rdr (!strm) of
                     NONE => ""
                   | SOME (s, strm') => s before strm := strm'

      val lexstrm = Parser.makeLexer read (FilePos.newstate ())

      val (is, _) = Parser.parse (0, lexstrm, prError, ())
                      (*handle Parser.ParseError => ([],strm)*)
    in is end

  fun toTemplate args = MakeTimed.makeTimed args

end

