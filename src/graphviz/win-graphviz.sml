(* $Id$ *)

structure Graphviz = Graphviz (
          type t = TypedDot.graph
          val output = TypedDotIO.output
          val warn = fn msgs => TextIO.output (TextIO.stdErr,concat msgs ^"\n")

          fun statusToString st = let
              val wst = Windows.fromStatus st
            in SysWord.toString wst end

          structure Plain = TextPlain

          structure OpSys = HackOpSys
          (*structure OpSys = Windows (* Not included with SML/NJ *)*)
        )

