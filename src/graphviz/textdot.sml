(* $Id$ *)

structure TextDot = DotFn (
                      structure Id = struct open String; type t = string end
                      structure EdgeAtt  = TextAttribute
                      structure NodeAtt  = TextAttribute
                      structure GraphAtt = TextAttribute
                    )

