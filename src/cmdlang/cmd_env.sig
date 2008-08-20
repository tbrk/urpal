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

signature CMD_ENV = sig

  datatype direction = datatype Expression.direction

  type symbol = Atom.atom
  type symbolset = AtomSet.set

  datatype value = Template   of ParsedNta.template 
                 | Symbols    of AtomSet.set
                 | SymbolMap  of symbol AtomMap.map
                 | Actions    of ActionSet.set
                 | ActionMap  of (symbol * direction) ActionMap.map
                 | Fail       of string
                 | Success

  type t

  val fromNta       : ParsedNta.nta -> t
  val toNta         : t -> ParsedNta.nta

  val insert        : (symbol * value) * t -> t
  val remove        : t * symbol -> t * value
  val getValue      : t -> symbol -> value option

  val isFail        : value -> bool

  val listTemplates : t -> ParsedNta.template list
  val listItems     : t -> (symbol * value) list

  val toString      : value -> string

  val globalDecl    : t -> Environment.env

  val symbolsToActions : AtomSet.set -> ActionSet.set
end

