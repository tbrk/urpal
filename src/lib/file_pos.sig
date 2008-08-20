(* $Id$
 *
 * Copyright (c) 2008 Timothy Bourke (University of NSW and NICTA)
 * All rights reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the "BSD License" which is distributed with the
 * software in the file LICENSE.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the BSD
 * License for more details.
 *
 * Record positions for error reporting. Based on idea from ML-Yacc manual.
 * Positions are line (from 1), character (from 0) pairs.
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

