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

signature UPPAAL_PARSE =
sig
  structure Environment   : ENVIRONMENT
  structure Expression    : EXPRESSION
  structure Declaration   : DECLARATION
  structure ExpressionCvt : EXPRESSION_CVT

  type symbol = Atom.atom
  
  val parseExpression : string -> string -> Expression.expr option
  val parseExpressionList : string -> string -> Expression.expr list option
  val parseDeclarations : string -> string -> (Declaration.decl list) option
  val parseParameters : string -> string -> (Declaration.param list) option
  val parseSelect : string -> string -> Expression.boundid list option
  val parseSync : string -> string ->
                  (symbol * Expression.direction * Expression.expr list) option

  val parse : TextNta.nta * string -> ParsedNta.nta option

  val removeUnusedSelectIds : ParsedNta.transition -> ParsedNta.transition

end

