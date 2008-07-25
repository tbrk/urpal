(* $Id$ *)

structure Graphviz = Graphviz (
          type t = TypedDot.graph
          val output = TypedDotIO.output
          val warn = fn msgs => TextIO.output (TextIO.stdErr,concat msgs ^"\n")

          val statusToString = Signal64.statusToString

          structure Plain = TextPlain
          structure OpSys = Unix
        )

