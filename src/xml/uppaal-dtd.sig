(* $Id$ *)

signature UPPAAL_DTD =
sig
  include Dtd

  datatype tag = Nta | Imports | Declaration | Template | Name |
                 Parameter | Location | Init | Urgent | Committed |
                 Transition | Source | Target | Label | Nail |
                 Instantiation | System | UnknownTag

  datatype attribute = X | Y | Id | Color | Ref | Kind | UnknownAtt

  datatype labelkind = Invariant | Comments | Synchronisation |
                       Update | Guard | Select | UnknownKind

  val idxToTag  : int -> tag
  val idxToAtt  : int -> attribute
  val vecToKind : UniChar.Vector -> labelkind

  val tagToStr  : tag -> string
end

