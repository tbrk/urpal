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
 * Shortcuts over Atom and AtomSet.
 *    infix <+ <- ++ <\ \ =:=  (*`*); open Symbol
 *)

signature SYMBOL = sig

  type symbol = Atom.atom
  type symbolset = AtomSet.set

  val emptyset   : symbolset

  val <+         : symbolset * symbol -> symbolset    (* add    *)
  val <-         : symbol * symbolset -> bool         (* member *)
  val ++         : symbolset * symbolset -> symbolset (* union  *)
  val <\         : symbolset * symbol -> symbolset    (* difference *)
  val \          : symbolset * symbolset -> symbolset (* difference *)
  val =:=        : symbol * symbol -> bool            (* equality *)
  val `          : string -> symbol                   (* atom *)

  val getNewName : symbol * symbolset -> symbol

end

