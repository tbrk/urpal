(* $Id$ *)

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

