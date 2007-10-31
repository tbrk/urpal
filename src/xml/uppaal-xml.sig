(* $Id$ *)

signature UPPAAL_XML =
sig
  val parse : Uri.Uri -> TextNta.nta option
end

