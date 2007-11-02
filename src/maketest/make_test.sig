(* $Id$ *)

signature MAKE_TEST =
sig
  type symbol = Atom.atom

  exception InvalidChannelId of Atom.atom
  exception MissingChannelIds of Atom.atom list
  exception NoChannels
  exception SilentTransition

  val maketest : symbol list * ParsedNta.template
                -> ParsedNta.locId * ParsedNta.template

  val makeVarCheckTrans : symbol list -> ParsedNta.template
                          -> ParsedNta.template * 
                             (ParsedNta.locId -> ParsedNta.transition list)
    (* Duplicate certain shared variables, preferring the new copies in
     * expressions. Returns a function from the error location to a
     * list of transitions that compare the duplicate variables with the
     * originals (for inclusion after transflip/other manipulations). *)

  val addReadyChecks : symbol list -> ParsedNta.locId * ParsedNta.template
                       -> ParsedNta.template
    (* addReadyChecks channels (t, errLoc)
     * If none of channels is urgent then return t unchanged.
     * Otherwise, effect the Jensen construction, extended to handle
     * arrays of channels, by adding a state for each channel,
     * splitting other locations, and adding transitions appropriately. *)
end

