(* $Id$ *)

structure Graphviz = Graphviz (
          type t = TypedDot.graph
          val output = TypedDotIO.output
          val warn = fn msgs => TextIO.output (TextIO.stdErr,concat msgs ^"\n")

          (* TODO: shift this somewhere else *)
          local
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
          in
          fun statusToString st = let
             (* SML/NJ: *)
                val estr = Posix.Error.errorMsg o Posix.Error.fromWord
                           o Word32.fromLargeWord o Word8.toLargeWord
             (* MLton: *)
             (* val estr = Posix.Error.errorMsg o Posix.Error.fromWord
                           o Word8.toLargeWord *)
                fun sigName s = let
                                  (* SML/NJ: *)
                                  val w= Word32.toString (Posix.Signal.toWord s)
                                  (* MLton: *)
                               (* val w= Word64.toString (Posix.Signal.toWord s) *)
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

          structure Plain = TextPlain
          structure OpSys = Unix
        )

