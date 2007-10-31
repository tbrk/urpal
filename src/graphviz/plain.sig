(* $Id$ *)

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

