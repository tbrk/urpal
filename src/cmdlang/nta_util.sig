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

  val stripTransitionIds  : ParsedNta.template -> ParsedNta.template
  val addTransitionIds    : (ParsedNta.transition -> bool) ->
                            ParsedNta.template -> ParsedNta.template

  val addTransitions      : ParsedNta.template * ParsedNta.transition list
                            -> ParsedNta.template

  val makeInputEnabler    : ((symbol * Expression.direction) list
                                                        * Environment.env)
                            -> ParsedNta.template

  val namesetToLocset     : AtomSet.set * ParsedNta.template
                            -> IntBinarySet.set
end

