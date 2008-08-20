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
 *   Tie the various PP and DotPP functors together to write TextDot graphs to
 *   TextIO outstreams.
 *)

structure TextDotIO :
sig
  val output : TextIO.outstream * TextDot.graph -> unit
end
=
struct
  structure PPStrm = PPStreamFn (structure Token = StringToken
                                       and Device = SimpleTextIODev)
  structure TextDotPP = DotPPFn (structure PPStream = PPStrm and Dot = TextDot)

  fun output (ostrm, graph) = let
      val dev = SimpleTextIODev.openDev {dst=ostrm, wid=78}
      val ppstrm = PPStrm.openStream dev
    in
      TextDotPP.output (ppstrm, graph);
      PPStrm.closeStream ppstrm
    end
end

