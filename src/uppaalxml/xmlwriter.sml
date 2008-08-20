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

  (* May be set to the same character. *)
  val startData = #"\^B"  (* ASCII STX *)
  val endData   = #"\^C"  (* ASCII ETX *)

  functor Translator (type slice
                      val startData : char
                      val endData   : char

                      val full    : string -> slice
                      val size    : slice -> int
                      val splitl  : (char -> bool) -> slice -> slice * slice
                      val sub     : slice * int -> char
                      val triml   : int -> slice -> slice
                      val isEmpty : slice -> bool
                     )
  : sig
    val mkWriter : (bool ref) -> (slice -> int) -> (slice -> int)
  end
  =
  struct (*{{{2*)
    fun atSpecialText c = (c=endData orelse c=startData orelse
                           (Char.isPunct c andalso
                             (c= #"&" orelse c= #"<" orelse c= #">"
                                      orelse c= #"\"" orelse c= #"'")))
    fun atXmlEnd c = (c=startData orelse c=endData)

    local
      val ent_amp  = full "&amp;"
      val ent_lt   = full "&lt;"
      val ent_gt   = full "&gt;"
      val ent_quot = full "&quot;"
      val ent_apos = full "&apos;"
    in
    fun toEntity #"&"  = ent_amp
      | toEntity #"<"  = ent_lt
      | toEntity #">"  = ent_gt
      | toEntity #"\"" = ent_quot
      | toEntity #"'"  = ent_apos
      | toEntity c     = raise Fail ("non-mappable character (" ^
                                     Char.toString c ^ ")")
    end
    
    fun mkWriter inXmlMode writeVec data = let

        (* This function does not give up until the entity expansion is 
           written in full. Non-blocking calls would be trickier to
           implement; the writer would have to buffer the expansion. *)
        fun writeEntity c = let
            fun write slice = let val (l, w) = (size slice, writeVec slice)
                              in if w < l then write (triml w slice) else ()
                              end
          in write (toEntity c) end

        fun processXml (slice, i) = if isEmpty slice then i else
          let
            val (left, right) = splitl atXmlEnd slice
            val (length, written) = (size left, writeVec left)
          in
            if written < length orelse isEmpty right
            then i + written
            else let val r = triml 1 right
                 in if sub (right, 0) = startData
                    then (inXmlMode := false; processText (r, i + written + 1))
                    else processXml (r, i + written + 1)
                 end
          end

        and processText (slice, i) = if isEmpty slice then i else
          let
            val (left, right) = splitl atSpecialText slice
            val (length, written) = (size left, writeVec left)
          in
            if written < length orelse isEmpty right
            then i + written
            else let val (c, r) = (sub (right, 0), triml 1 right)
                     val t = i + written + 1
                 in if c = endData
                    then (inXmlMode := true; processXml (r, t))
                    else (if c <> startData then writeEntity c else ();
                          processText (r, t))
                 end
          end

      in if !inXmlMode then processXml (data, 0) else processText (data, 0) end
  end (*}}}2*)

local (* TODO: should also wrap the functor and values above, but currently
               this breaks compilation under mlton*)
  (*{{{1*)

  structure VectorTrans = Translator (open Substring
                                      type slice = substring
                                      val startData = startData
                                      val endData = endData)

  structure SubstringArray =
  struct (*{{{2*)
    open CharArraySlice

    fun strToArray s = CharArray.tabulate
                          (String.size s, fn i=>String.sub (s,i))
    val full = full o strToArray
    val size = length

    val empty = full ""
    fun splitl p a = case findi (fn (_, e)=> p e) a
                     of NONE        => (a, empty)
                      | SOME (i, _) => (subslice (a, 0, SOME i),
                                        subslice (a, i, NONE))
    fun triml n a = subslice (a, n, NONE)
  end (*}}}2*)

  structure ArrayTrans = Translator (open SubstringArray
                                     val startData = startData
                                     val endData = endData)
  (*}}}1*)
in
structure XMLWriter :> XML_WRITER
                       where type outstream = TextIO.StreamIO.outstream
=
struct
  type outstream = TextIO.StreamIO.outstream

  fun mkWriter (TextPrimIO.WR {name, chunkSize, writeVec, writeArr, writeVecNB,
                               writeArrNB, block, canOutput, getPos, setPos,
                               endPos, verifyPos, close, ioDesc}) =
    let val mode = ref true
    in TextPrimIO.WR {name=name ^ "+XML", chunkSize=chunkSize,
                      writeVec=Option.map (VectorTrans.mkWriter mode) writeVec,
                      writeArr=Option.map (ArrayTrans.mkWriter mode) writeArr,
                      writeVecNB=NONE, writeArrNB=NONE, block=NONE,
                      canOutput=NONE, getPos=getPos,
                      setPos=setPos, endPos=endPos, verifyPos=verifyPos,
                      close=close, ioDesc=NONE}
    end

  fun mkOutstream outstream = let
      val _ = TextIO.StreamIO.flushOut outstream
      val (owriter, bufmode) = TextIO.StreamIO.getWriter outstream
      val nwriter = mkWriter owriter
    in TextIO.StreamIO.mkOutstream (nwriter, bufmode) end

  val data = String.str startData
  val xml  = String.str endData
end
end (* local *)

(* To test:
    val strm = XMLWriter.mkOutstream (TextIO.getOutstream TextIO.stdOut);
    TextIO.StreamIO.output (strm, String.concat ["<test val=\"&quot;\">",
                                                 XMLWriter.data,
                                                 "x > 2 && y < 3",
                                                 XMLWriter.xml,
                                                 "</test>\n"]);
 *)

