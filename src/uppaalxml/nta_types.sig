(* $Id$ *)

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

