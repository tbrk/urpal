(* $Id$
 *
 * Pipe a graph (in dot format) through one of the GraphViz utilities
 * and return the result (in plain format).
 *
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

