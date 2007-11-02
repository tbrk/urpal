(* $Id$ *)

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

