(* $Id$ *)

signature SETTINGS =
sig
  val version             : string
  val progName            : string

  val dtdPath             : unit -> string option
  val prefix              : unit -> string option

  (* layout *)
  val graphvizPath        : unit -> string
  val graphvizEngine      : unit -> string

  (* appearance *)
  val maxLabelWidth       : unit -> int
  val maxDeclarationWidth : unit -> int

  val newColor            : unit -> string option
  val errorColor          : unit -> string option
  val urgChanLocColor     : unit -> string option

  val splitShiftOld       : unit -> {xoff: int, yoff: int} option
  val splitShiftNew       : unit -> {xoff: int, yoff: int} option
  val tabulateShift       : unit -> {xoff: int, yoff: int}

  val tabulateLabels      : unit -> bool

  (* debug *)
  datatype debug_priority = All | VeryDetailed | Detailed | Outline | NoDebug
  val showDebug           : debug_priority -> bool

  val validate            : unit -> bool
  val saveConfigFile      : TextIO.outstream -> unit
end

