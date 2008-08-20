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

structure ConfigTree :> CONFIG_TREE = ConfigTree (struct
    datatype lexresult = datatype ConfigLex.UserDeclarations.lexresult
    val makeLexer = ConfigLex.makeLexer
    val linenum = (fn ()=> !ConfigLex.UserDeclarations.linenum)
  end
)

