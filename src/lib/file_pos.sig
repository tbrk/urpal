(* $Id$

   Record positions for error reporting. Based on idea from ML-Yacc manual.
   Positions are line (from 1), character (from 0) pairs.
*)
signature FILE_POS =
sig
  type state
  type pos

  val zero      : pos

  val newstate  : unit -> state
  val nextline  : state * int -> unit    (* pass (yyarg, yypos) *)
  val currpos   : state * int -> pos     (* pass (yyarg, yypos) *)

  val incCommentDepth : state -> unit
  val decCommentDepth : state -> bool
    (* returns true when the comment depth is zero. *)

  val error     : string -> string * pos * pos -> unit
end

