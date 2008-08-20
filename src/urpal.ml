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
use "mlyacc.ml";
use "smlnj-lib.ml";
use "fxlib.ml";
use "lib/sources.ml";
use "config/sources.ml";
use "graphviz/sources.ml";
use "layout/sources.ml";
use "general.ml";
use "uppaalxml/sources.ml";
use "uppaal/sources.ml";
use "maketest/sources.ml";
use "mcs51/sources.ml";
use "cmdlang/sources.ml";
use "commands.sig";
use "commands.sml";
use "urpal.sml";
fun urpal () = ignore (Urpal.main (CommandLine.name (),
				   CommandLine.arguments ()));
PolyML.export("urpal", urpal);
