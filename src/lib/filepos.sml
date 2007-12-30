(* $Id$ *)

structure FilePos :> FILE_POS =
struct
  type state = {line: int, pos: int, depth: int} ref
  type pos = int * int

  val zero = (0, 0)

  fun newstate () = ref {line=1, pos= ~1, depth=0}
  fun nextline (st as ref {line, depth,...}, yypos) = st := {line=line + 1,
                                                             pos=yypos - 1,
                                                             depth=depth}
  
  fun currpos (ref {line, pos, depth}, yypos)  = (line, yypos - pos)

  fun error prefix (str, (line, pos), (_, _)) = let
    fun pr s = TextIO.output (TextIO.stdOut, s)
  in
    pr (prefix); pr ":";
    pr (Int.toString line); pr ":";
    pr (Int.toString pos); pr ":";
    pr str; pr "\n"
  end
  
  fun incCommentDepth (st as ref {line, pos, depth}) = st := {line=line,
                                                              pos=pos,
                                                              depth=depth+1}
  fun decCommentDepth (st as ref {line, pos, depth=0}) = true
    | decCommentDepth (st as ref {line, pos, depth}) = let in
          st := {line=line, pos=pos, depth=depth-1};
          depth = 0
        end
  
end

