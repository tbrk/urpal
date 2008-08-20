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

structure ActionSet =
  RedBlackSetFn (
    struct
      type ord_key = Atom.atom * Expression.direction
      fun compare ((n1, d1), (n2, d2)) = case Atom.compare (n1, n2) of
                                           EQUAL => dirCompare (d1, d2)
                                         | v => v
      and dirCompare (Expression.Input, Expression.Input)   = EQUAL
        | dirCompare (Expression.Input, _)                  = GREATER
        | dirCompare (Expression.Output, Expression.Output) = EQUAL
        | dirCompare _                                      = LESS
    end)

