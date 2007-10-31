(* $Id$ *)

structure Graphviz = Graphviz (
          type t = TypedDot.graph
          val output = TypedDotIO.output
          val warn = fn msgs => TextIO.output (TextIO.stdErr,concat msgs ^"\n")

          structure Plain = TextPlain
          structure OpSys = Unix
        )

