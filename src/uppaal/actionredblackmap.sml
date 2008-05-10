(* $Id$ *)

structure ActionRedBlackMap =
  RedBlackMapFn (
    struct
      type ord_key = Atom.atom * Expression.direction
      fun compare ((n1, d1), (n2, d2)) = case Atom.compare (n1, n2) of
                                           EQUAL => dirCompare (d1, d2)
                                         | v => v
      and dirCompare (Expression.Input, Expression.Input)   = EQUAL
        | dirCompare (Expression.Input, _)                  = GREATER
        | dirCompare (Expression.Output, Expression.Output) = EQUAL
        | dirCompare _                                      = LESS
    end)

