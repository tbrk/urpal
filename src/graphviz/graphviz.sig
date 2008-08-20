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
 * Pipe a graph (in dot format) through one of the GraphViz utilities
 * and return the result (in plain format).
 *)

signature GRAPHVIZ =
sig
  type t
  type plain_graph

  datatype output = PS | SVG
  datatype graph  = Dot | Neato | Fdp | Twopi | Circo

  val stringToGraph : string -> graph option
  val graphToString : graph -> string

  val makePlain  : graph -> t -> plain_graph option
  val makeFile   : graph * output -> (TextIO.outstream * t) -> OS.Process.status
end

