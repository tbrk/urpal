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

signature XML_WRITER =
sig
  type outstream

  (* Augment the given writer so that it can translate characters
     into the five predefined character entities:
        <  to  &lt;           >  to  &gt;
        &  to  &amp;          "  to  &quot;        '  to  &apos;
     when the mode is set to Data.                                 *)
  val mkWriter : TextPrimIO.writer -> TextPrimIO.writer
  val mkOutstream : outstream -> outstream

  (* Printing these strings through the writer changes the mode. In data mode,
     characters are translated. In xml mode, no translation occurs. *)
  val data : string
  val xml  : string
end

