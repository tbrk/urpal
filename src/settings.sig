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

signature SETTINGS =
sig
  val version             : string
  val progName            : string
  val copyright           : string

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

  (* operation *)
  val exitOnFail          : unit -> bool

  (* debug *)
  datatype debug_priority = All | VeryDetailed | Detailed | Outline | NoDebug
  val showDebug           : debug_priority -> bool

  val validate            : unit -> bool
  val saveConfigFile      : TextIO.outstream -> unit
end

