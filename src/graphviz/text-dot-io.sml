(* $Id$

   20070822 T. Bourke
     Tie the various PP and DotPP functors together to write TextDot graphs to
     TextIO outstreams.
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

