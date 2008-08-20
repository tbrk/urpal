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

signature UPPAAL_DTD =
sig
  include Dtd

  datatype tag = Nta | Imports | Declaration | Template | Name |
                 Parameter | Location | Init | Urgent | Committed |
                 Transition | Source | Target | Label | Nail |
                 Instantiation | System | UnknownTag

  datatype attribute = X | Y | Id | Color | Ref | Kind | UnknownAtt

  datatype labelkind = Invariant | Comments | Synchronisation |
                       Update | Guard | Select | UnknownKind

  val idxToTag  : int -> tag
  val idxToAtt  : int -> attribute
  val vecToKind : UniChar.Vector -> labelkind

  val tagToStr  : tag -> string
end

