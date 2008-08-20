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

signature RESULT =
sig
  type expr
  type boundid
  type direction
  type decl
  type param
  type symbol = Atom.atom

  datatype t = Expr of expr
             | Decls of decl list
             | Select of boundid list
             | Sync of symbol * direction * expr list
             | ExprList of expr list
             | Params of param list
end

