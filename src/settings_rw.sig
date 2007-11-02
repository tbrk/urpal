(* $Id$ *)

signature SETTINGS_RW =
sig
  include SETTINGS

  val set_dtdPath             : string option -> unit
  val set_prefix              : string option -> unit

  (* layout *)
  val set_graphvizPath        : string option -> unit
  val set_graphvizEngine      : Graphviz.graph -> unit

  (* appearance *)
  val set_maxLabelWidth       : int -> unit
  val set_maxDeclarationWidth : int -> unit

  val set_newColor            : string option -> unit
  val set_errorColor          : string option -> unit
  val set_urgChanLocColor     : string option -> unit

  val set_splitShiftOld       : {xoff:int, yoff:int} option -> unit
  val set_splitShiftNew       : {xoff:int, yoff:int} option -> unit
  val set_tabulateShift       : {xoff:int, yoff:int} option -> unit

  val set_tabulateLabels      : bool -> unit

  (* debug *)
  val set_priority            : int -> unit

  val loadConfigFile       : (string, 'strm) StringCvt.reader -> 'strm -> unit
end

