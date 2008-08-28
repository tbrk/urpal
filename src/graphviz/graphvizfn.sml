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

functor Graphviz (
          type t
          val output : TextIO.outstream * t -> unit
          val warn   : string list -> unit
          val statusToString : OS.Process.status -> string

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
      val d = OS.Path.concat (Settings.graphvizPath (), "bin")
              handle Path => OS.Path.currentArc
    in OS.Path.joinDirFile {dir=d, file=Settings.adjustExe exe} end

  fun graphToPath Dot   = exePath "dot"
    | graphToPath Neato = exePath "neato"
    | graphToPath Fdp   = exePath "fdp"
    | graphToPath Twopi = exePath "twopi"
    | graphToPath Circo = exePath "circo"

  (* Relies on the fact that the graphviz utilities read an entire dot file
   * on stdin before writing any output to stdout (otherwise deadlock is
   * possible). *)
  fun makePlain g v = let
    val _     = Util.debugOutline (fn ()=>["executing ", graphToPath g,
                                           " -Tplain"])
    val proc  = OpSys.execute (graphToPath g, ["-Tplain"])
    val ostrm = OpSys.textOutstreamOf proc
    val _     = output (ostrm, v)
    val _     = TextIO.closeOut ostrm
    val _     = Util.debugDetailed (fn ()=>["--output written."])
    val istrm = TextIO.getInstream (OpSys.textInstreamOf proc)

    (*
    fun dscan istrm = let (* for debugging: *)
            val r = TextIO.StreamIO.input1 istrm
            val _ = case r of
                      NONE        => TextIO.print "[DONE]"
                    | SOME (c, _) => TextIO.print ("·" ^ Char.toString c)
        in r end
    val (plain, istrm') = case Plain.scan dscan istrm of
    *)

    val (plain, istrm') = case Plain.scan TextIO.StreamIO.input1 istrm of
                            NONE                 => (NONE, istrm)
                          | SOME (plain, istrm') => (SOME plain, istrm')

    val _     = Util.debugDetailed (fn ()=>["--reaping..."])
    val st    = OpSys.reap proc
    val _     = Util.debugDetailed (fn ()=>["--done (",
                                            if OS.Process.isSuccess st
                                            then "success" else "failure", ")"])
    val _     = if not (OS.Process.isSuccess st)
                then warn ["reap failed: ", statusToString st]
                else ()
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
    val _     = Util.debugOutline (fn ()=>["executing ", graphToPath g,
                                           " ", langToOption lang])
    val proc  = OpSys.execute (graphToPath g, [langToOption lang])
    val ostrm = OpSys.textOutstreamOf proc
    val istrm = OpSys.textInstreamOf proc

    val _     = Util.debugDetailed (fn ()=>["--writing output..."])
    val _     = output (ostrm, v)
    val _     = TextIO.closeOut ostrm
    val _     = Util.debugDetailed (fn ()=>["--done."])

    val _     = copyFile (istrm, fstrm) before TextIO.closeIn istrm
  in OpSys.reap proc end
  handle IO.Io {cause,...} => OS.Process.failure before
    warn ["failed invoking ",graphToPath g," (", General.exnMessage cause, ")"]
       | e => OS.Process.failure before
    warn ["failed invoking ",graphToPath g," (", General.exnMessage e, ")"]
  
end

