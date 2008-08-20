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
 *
 * Derived from the Intel MCS-51 Instruction Set Summary
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

