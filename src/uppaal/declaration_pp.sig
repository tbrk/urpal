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

signature DECLARATION_PP =
sig
  type pp_desc
   and stream

  type symbol = Atom.atom

  type decl
   and stmt
   and param
   and initialiser

  val fromDecl       : decl  -> pp_desc
  val fromStmt       : stmt  -> pp_desc
  val fromParams     : param list  -> pp_desc
  val fromInitialiser : initialiser -> pp_desc

  val print    : stream -> pp_desc -> unit
end

