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
 * Tie the various PP and DotPP functors together to write TypedDot graphs
 * to TextIO outstreams.
 *)

structure TypedDotIO :
sig
  val output : TextIO.outstream * TypedDot.graph -> unit
end
=
struct
  structure PPStrm = PPStreamFn (structure Token = StringToken
                                       and Device = SimpleTextIODev)
  structure TypedDotPP = DotPPFn (structure PPStream = PPStrm and Dot = TypedDot)

  fun output (ostrm, graph) = let
      val dev = SimpleTextIODev.openDev {dst=ostrm, wid=78}
      val ppstrm = PPStrm.openStream dev
    in
      TypedDotPP.output (ppstrm, graph);
      PPStrm.closeStream ppstrm
    end
end

