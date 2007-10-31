(* $Id$ *)

structure TypedDot = DotFn (
                       structure Id = struct open String; type t = string end
                       structure EdgeAtt  = TypedAttributes.Edge
                       structure NodeAtt  = TypedAttributes.Node
                       structure GraphAtt = TypedAttributes.Graph
                     ) : DOT

