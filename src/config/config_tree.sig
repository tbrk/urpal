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

signature CONFIG_TREE = sig

  type symbol = Atom.atom

  exception ParseError of int

  datatype entry = Entry   of symbol * entry_type
                 | Section of symbol * entry list

       and entry_type = Id     of symbol
                      | Int    of int
                      | Bool   of bool
                      | Color  of string
                      | Real   of real
                      | String of string

  type t = entry list

  val entryToString : entry_type -> string

  val getEntry  : t * symbol list -> entry_type option
  val getSymbol : t * symbol list -> symbol option (* String -> symbol *)
  val getString : t * symbol list -> string option (* Id -> string,
                                                      Int -> string,
                                                      Color -> string  *)
  val getInt    : t * symbol list -> int option
  val getReal   : t * symbol list -> real option   (* Int -> Real      *)
  val getColor  : t * symbol list -> string option
  val getBool   : t * symbol list -> bool option   (* give default value *)

  val parse : (string, 'strm) StringCvt.reader -> 'strm -> t
  val output : TextIO.outstream * t -> unit

end
    
