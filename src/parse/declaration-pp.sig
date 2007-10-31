(* $Id$ *)

signature DECLARATION_PP =
sig
  type pp_desc
   and stream

  type symbol = Atom.atom

  type decl
   and stmt
   and param
   and initialiser

  val fromDecl       : decl  -> pp_desc
  val fromStmt       : stmt  -> pp_desc
  val fromParams     : param list  -> pp_desc
  val fromInitialiser : initialiser -> pp_desc

  val print    : stream -> pp_desc -> unit
end

