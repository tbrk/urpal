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

structure TextNta = NtaFn (type invariant     = string option
                            and select        = string option
                            and guard         = string option
                            and sync          = string option
                            and update        = string option
                            and parameter     = string option
                            and declaration   = string option
                            and imports       = string option
                            and instantiation = string option
                            and system        = string

                            val noInvariant   = NONE
                            val noSelect      = NONE
                            val noGuard       = NONE
                            val noSync        = NONE
                            val noUpdate      = NONE

                            val noParameter     = NONE
                            val noDeclaration   = NONE
                            val noImports       = NONE
                            val noInstantiation = NONE
                            val noSystem        = ""
                          )

