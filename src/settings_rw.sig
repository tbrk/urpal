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

signature SETTINGS_RW =
sig
  include SETTINGS

  val set_dtdPath             : string option -> unit
  val set_prefix              : string option -> unit

  (* layout *)
  val set_graphvizPath        : string option -> unit
  val set_graphvizEngine      : string -> unit

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

