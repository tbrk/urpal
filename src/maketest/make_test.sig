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

signature MAKE_TEST =
sig
  type symbol = Atom.atom

  exception InvalidChannelId of symbol
  exception NoChannels
  exception SilentTransition
  exception BadBroadcastChannel of symbol

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

