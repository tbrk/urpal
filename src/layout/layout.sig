(* $Id$ *)

signature LAYOUT =
sig
  type symbol = Atom.atom
  type pos = ParsedNta.pos

  val toDot               : bool -> ParsedNta.template -> TypedDot.graph

  val addCoordsFromPlain  : ParsedNta.template * TextPlain.graph
                            -> ParsedNta.template
  val tabulateTransLabels : (int * int) * (ParsedNta.transition -> bool)
                                        * ParsedNta.template
                            -> ParsedNta.template
  val tabulateAll         : (ParsedNta.locId -> bool) * ParsedNta.template
                            -> ParsedNta.template
      (* lay labels out in tabular format given either a position and predicate
       * on transitions, or a predicate on destination ids. *)

  val positionLabels      : (ParsedNta.locId -> pos option)
                            -> ParsedNta.transition -> ParsedNta.transition
  val matrixTrans         : (ParsedNta.locId -> pos option)
                              * ParsedNta.transition list
                            -> ParsedNta.transition list
  
  val joinColumns         : pos * pos -> pos list
end

