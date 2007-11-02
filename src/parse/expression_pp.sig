(* $Id$ *)

signature EXPRESSION_PP =
sig
  type pp_desc
   and stream

  type symbol = Atom.atom

  type expr
   and ty
   and var
   and boundid
   and direction

  val fromExpr          : expr -> pp_desc
  val fromType          : ty   -> pp_desc
  val fromTypeWithId    : ty * symbol -> pp_desc
  val fromRefTypeWithId : ty * symbol -> pp_desc
  val fromVar           : var  -> pp_desc

  val fromSelects  : boundid list -> pp_desc
  val fromSync     : symbol * direction * expr list -> pp_desc
  val fromExprList : expr list -> pp_desc

  val print    : stream -> pp_desc -> unit
end

