(* $Id$
 *
 * 20070827 T. Bourke
 *   Original code.
 *
 *)

functor Graphviz (
          type t
          val output : TextIO.outstream * t -> unit
          val warn   : string list -> unit

          structure Plain : PLAIN
          structure OpSys :
            sig
              type ('a, 'b) proc
              val execute : string * string list -> ('a, 'b) proc
              val textInstreamOf : (TextIO.instream, 'a) proc
                                     -> TextIO.instream
              val textOutstreamOf : ('a, TextIO.outstream) proc
                                     -> TextIO.outstream
              val reap : ('a, 'b) proc -> OS.Process.status
            end
        ) : GRAPHVIZ
=
struct
  type t = t
  type plain_graph = Plain.graph

  datatype output = PS | SVG
  datatype graph  = Dot | Neato | Fdp | Twopi | Circo

  val graphvizPath = ref "/usr/local"

  fun langToOption PS  = "-Tps"
    | langToOption SVG = "-Tsvg"

  fun stringToGraph "dot"   = SOME Dot
    | stringToGraph "neato" = SOME Neato
    | stringToGraph "fdp"   = SOME Fdp
    | stringToGraph "twopi" = SOME Twopi
    | stringToGraph "circo" = SOME Circo
    | stringToGraph _       = NONE

  fun graphToString Dot   = "dot"
    | graphToString Neato = "neato"
    | graphToString Fdp   = "fdp"
    | graphToString Twopi = "twopi"
    | graphToString Circo = "circo"

  fun exePath exe = let
      val d = OS.Path.concat (!graphvizPath, "bin")
    in OS.Path.joinDirFile {dir=d, file=exe} end

  fun graphToPath Dot   = exePath "dot"
    | graphToPath Neato = exePath "neato"
    | graphToPath Fdp   = exePath "fdp"
    | graphToPath Twopi = exePath "twopi"
    | graphToPath Circo = exePath "circo"

  (* Relies on the fact that the graphviz utilities read an entire dot file
   * on stdin before writing any output to stdout (otherwise deadlock is
   * possible). *)
  fun makePlain g v = let
    val proc  = OpSys.execute (graphToPath g, ["-Tplain"])
    val ostrm = OpSys.textOutstreamOf proc
    val istrm = TextIO.getInstream (OpSys.textInstreamOf proc)
    val _     = output (ostrm, v)
    val _     = TextIO.closeOut ostrm

    val (plain, istrm') = case Plain.scan TextIO.StreamIO.input1 istrm of
                            NONE                 => (NONE, istrm)
                          | SOME (plain, istrm') => (SOME plain, istrm')

    val st    = OpSys.reap proc
    val _     = TextIO.StreamIO.closeIn istrm

  in if OS.Process.isSuccess st then plain else NONE end
  handle IO.Io {cause,...} => NONE before
    warn ["failed invoking ",graphToPath g," (", General.exnMessage cause, ")"]
       | e => NONE before
    warn ["failed invoking ",graphToPath g," (", General.exnMessage e, ")"]

  fun copyFile (istrm, ostrm) = let
      fun loop data = if CharVector.length data = 0 then ()
                      else (TextIO.output (ostrm, data);
                            loop (TextIO.input istrm))
    in loop (TextIO.input istrm) end

  fun makeFile (g, lang) (fstrm, v) = let
    val proc  = OpSys.execute (graphToPath g, [langToOption lang])
    val ostrm = OpSys.textOutstreamOf proc
    val istrm = OpSys.textInstreamOf proc

    val _     = output (ostrm, v)
    val _     = TextIO.closeOut ostrm

    val _     = copyFile (istrm, fstrm) before TextIO.closeIn istrm
  in OpSys.reap proc end
  handle IO.Io {cause,...} => OS.Process.failure before
    warn ["failed invoking ",graphToPath g," (", General.exnMessage cause, ")"]
       | e => OS.Process.failure before
    warn ["failed invoking ",graphToPath g," (", General.exnMessage e, ")"]
  
end

