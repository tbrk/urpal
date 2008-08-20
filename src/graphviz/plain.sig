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

signature PLAIN =
sig
  type id
  type label
  type style
  type shape
  type color

  type node  = {name      : id,
                x         : real,
                y         : real,
                width     : real,
                height    : real,
                label     : label,
                style     : style,
                shape     : shape,
                color     : color,
                fillcolor : color}

  type edge  = {head      : id,
                tail      : id,
                points    : (real * real) list,
                label     : (label * real * real) option,
                style     : style,
                color     : color}

  type graph = {scale     : real,
                width     : real,
                height    : real,
                nodes     : node list,
                edges     : edge list}

  val scan   : (char, 'a) StringCvt.reader -> (graph, 'a) StringCvt.reader
  val output : TextIO.outstream * graph -> unit

end

