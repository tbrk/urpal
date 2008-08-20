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

structure Result : RESULT =
struct
  type expr      = Expression.expr
  type boundid   = Expression.boundid
  type direction = Expression.direction
  type decl      = Declaration.decl
  type param     = Declaration.param
  type symbol    = Atom.atom

  datatype t = Expr of expr
             | Decls of decl list
             | Select of boundid list
             | Sync of symbol * direction * expr list
             | ExprList of expr list
             | Params of param list
end

