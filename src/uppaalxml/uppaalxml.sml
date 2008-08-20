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

structure UppaalXML : UPPAAL_XML
=
let structure Parser = Parse (structure Dtd = UppaalDtd
                              structure Hooks = UppaalHooks
                              structure Resolve = UppaalResolver
                              structure ParserOptions = ParserOptions ())
          and T = TextNta
in
struct

  fun parse uri = let
      val dtd = UppaalDtd.initDtdTables ()
    in
      SOME (Parser.parseDocument (SOME uri) (SOME dtd) UppaalHooks.initData)
    end
    handle UppaalHooks.ParseError s => NONE before (Util.warn [s])
end
end

