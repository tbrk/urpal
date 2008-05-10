(* $Id$
 *
 * Simple build file for Poly/ML.
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
