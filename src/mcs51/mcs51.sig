(* $Id$
 *
 * 20071107 T. Bourke
 *   Taken from Intel MCS-51 Instruction Set Summary
 *
 *)

signature MCS51 = sig
  
  structure Instruction : MCS51_INSTRUCTION
  type program = (string option * Instruction.t) list

  val parse             : (string,'strm) StringCvt.reader -> 'strm -> program
  val toTemplate        : program * {showinstrs : bool,
                                     position:    bool,
                                     maxrows:     int} -> ParsedNta.template

end

