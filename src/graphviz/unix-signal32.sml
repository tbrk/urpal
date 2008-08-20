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
 *)

structure Signal32 :
  sig
    val statusToString : OS.Process.status -> string
  end
=
struct

  structure S = Posix.Signal

  val sigs = [(S.abrt, "abrt"),
              (S.alrm, "alrm"),
              (S.bus,  "bus"),
              (S.fpe,  "fpe"),
              (S.hup,  "hup"),
              (S.ill,  "ill"),
              (S.int,  "int"),
              (S.kill, "kill"),
              (S.pipe, "pipe"),
              (S.quit, "quit"),
              (S.segv, "segv"),
              (S.term, "term"),
              (S.usr1, "usr1"),
              (S.usr2, "usr2"),
              (S.chld, "chld"),
              (S.cont, "cont"),
              (S.stop, "stop"),
              (S.tstp, "tstp"),
              (S.ttin, "ttin"),
              (S.ttou, "ttou")]

  fun statusToString st = let
        val estr = Posix.Error.errorMsg o Posix.Error.fromWord
                   o Word32.fromLargeWord o Word8.toLargeWord
        fun sigName s = let
                          val w= Word32.toString (Posix.Signal.toWord s)
                        in case List.find (fn(sg,_)=>sg=s) sigs of
                             NONE          => "0x" ^ w
                           | SOME (_, str) => str ^ " (0x" ^ w ^ ")"
                        end
      in
        case Unix.fromStatus st of
          Unix.W_EXITED       => "EXIT: no status"
        | Unix.W_EXITSTATUS w => "EXIT: 0x" ^ Word8.toString w
                                          ^ "(" ^ estr w ^ ")"
        | Unix.W_SIGNALED s   => "SIGNALED: " ^ sigName s
        | Unix.W_STOPPED s    => "STOPPED: "  ^ sigName s
      end
end
