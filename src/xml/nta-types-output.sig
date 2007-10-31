(* $Id$ *)

signature NTA_TYPES_OUTPUT =
sig
  include NTA_TYPES
  type outstream

  val hasInvariant              : invariant -> bool
  val hasSelect                 : select -> bool
  val hasGuard                  : guard -> bool
  val hasSync                   : sync -> bool
  val hasUpdate                 : update -> bool
  val hasParameter              : parameter -> bool
  val hasDeclaration            : declaration -> bool
  val hasImports                : imports -> bool
  val hasInstantiation          : instantiation -> bool

  val outputInvariant           : (outstream * invariant) -> unit
  val outputSelect              : (outstream * select) -> unit
  val outputGuard               : (outstream * guard) -> unit
  val outputSync                : (outstream * sync) -> unit
  val outputUpdate              : (outstream * update) -> unit
  val outputParameter           : (outstream * parameter) -> unit
  val outputDeclaration         : (outstream * declaration) -> unit
  val outputTemplateDeclaration : (outstream * declaration) -> unit
  val outputImports             : (outstream * imports) -> unit
  val outputInstantiation       : (outstream * instantiation) -> unit
  val outputSystem              : (outstream * system) -> unit
end

