(* $Id$

   20070823 T. Bourke
     Tie the various PP and DotPP functors together to write TypedDot graphs
     to TextIO outstreams.
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

