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

signature LAYOUT =
sig
  type symbol = Atom.atom
  type pos = ParsedNta.pos

  val toDot               : bool -> ParsedNta.template -> TypedDot.graph

  val addCoordsFromPlain  : ParsedNta.template * TextPlain.graph
                            -> ParsedNta.template
  val tabulateTransLabels : (int * int) * (ParsedNta.transition -> bool)
                                        * ParsedNta.template
                            -> ParsedNta.template
  val tabulateAll         : (ParsedNta.locId -> bool) * ParsedNta.template
                            -> ParsedNta.template
      (* lay labels out in tabular format given either a position and predicate
       * on transitions, or a predicate on destination ids. *)

  val positionLabels      : (ParsedNta.locId -> pos option)
                            -> ParsedNta.transition -> ParsedNta.transition
  val matrixTrans         : (ParsedNta.locId -> pos option)
                              * ParsedNta.transition list
                            -> ParsedNta.transition list
  
  val joinColumns         : pos * pos -> pos list
end

