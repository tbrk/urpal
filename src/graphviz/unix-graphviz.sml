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

structure Graphviz = Graphviz (
          type t = TypedDot.graph
          val output = TypedDotIO.output
          val warn = fn msgs => TextIO.output (TextIO.stdErr,concat msgs ^"\n")

          val statusToString = Signal32.statusToString

          structure Plain = TextPlain
          structure OpSys = Unix
        )

