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

signature CONFIG_LEXER = sig

  datatype lexresult = Id of Atom.atom
                     | Int of int
                     | Bool of bool
                     | Color of string
                     | Real of real
                     | OpenSection
                     | CloseSection
                     | Assign
                     | StringStart
                     | StringLine of string
                     | StringEnd
                     | Eof

  val makeLexer : (int -> string) -> unit -> lexresult
  val linenum   : unit -> int

end

