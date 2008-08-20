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

signature EXPRESSION_PP =
sig
  type pp_desc
   and stream

  type symbol = Atom.atom

  type expr
   and ty
   and var
   and boundid
   and direction

  val fromExpr          : expr -> pp_desc
  val fromType          : ty   -> pp_desc
  val fromTypeWithId    : ty * symbol -> pp_desc
  val fromRefTypeWithId : ty * symbol -> pp_desc
  val fromVar           : var  -> pp_desc

  val fromSelects  : boundid list -> pp_desc
  val fromSync     : symbol * direction * expr list -> pp_desc
  val fromExprList : expr list -> pp_desc

  val print    : stream -> pp_desc -> unit
end

