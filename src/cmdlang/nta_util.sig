(* $Id$ *)

signature NTA_UTIL =
sig
  type symbol = Atom.atom

  val warnIfCommitted     : ParsedNta.template -> unit
  val warnOnChannels      : ParsedNta.template -> unit

  val expandUrgentLocs    : ParsedNta.template -> ParsedNta.template

  val actions             : ParsedNta.template
                            -> (symbol * Expression.direction) list
  val channels            : ParsedNta.template -> symbol list
  val channelSet          : ParsedNta.template -> AtomSet.set

  exception CannotSplitChannelArrays
  val split               : ParsedNta.template *
                              (symbol * Expression.direction) ActionMap.map
                            -> ParsedNta.template

  val toDot               : bool -> ParsedNta.template -> TypedDot.graph

  val addCoordsFromPlain  : ParsedNta.template * TextPlain.graph
                            -> ParsedNta.template
  val stripTransitionIds  : ParsedNta.template -> ParsedNta.template
  val addTransitionIds    : (ParsedNta.transition -> bool) ->
                            ParsedNta.template -> ParsedNta.template
  val tabulateTransLabels : (int * int) * (ParsedNta.transition -> bool)
                                        * ParsedNta.template
                            -> ParsedNta.template
  val tabulateAll         : (ParsedNta.locId -> bool) * ParsedNta.template
                            -> ParsedNta.template
      (* lay labels out in tabular format given either a position and predicate
       * on transitions, or a predicate on destination ids. *)

  val addTransitions      : ParsedNta.template * ParsedNta.transition list
                            -> ParsedNta.template

  val makeInputEnabler    : ((symbol * Expression.direction) list
                                                        * Environment.env)
                            -> ParsedNta.template

  val namesetToLocset     : AtomSet.set * ParsedNta.template
                            -> IntBinarySet.set
end

