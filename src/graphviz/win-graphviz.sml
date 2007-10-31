(* $Id$ *)

structure Graphviz = Graphviz (
          type t = TypedDot.graph
          val output = TypedDotIO.output
          val warn = fn msgs => TextIO.output (TextIO.stdErr,concat msgs ^"\n")

          structure Plain = TextPlain

          structure OpSys = HackOpSys
          (*structure OpSys = Windows (* Not included with SML/NJ *)*)
        )

