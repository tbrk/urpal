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

signature NTA_TYPES =
sig
  type invariant
  type select
  type guard
  type sync
  type update

  val  noInvariant : invariant
  val  noSelect    : select
  val  noGuard     : guard
  val  noSync      : sync
  val  noUpdate    : update

  type parameter
  type declaration
  type imports
  type instantiation
  type system

  val noParameter     : parameter
  val noDeclaration   : declaration
  val noImports       : imports
  val noInstantiation : instantiation
  val noSystem        : system

end

