(* $Id$ *)

signature RESULT =
sig
  type expr
  type boundid
  type direction
  type decl
  type param
  type symbol = Atom.atom

  datatype t = Expr of expr
             | Decls of decl list
             | Select of boundid list
             | Sync of symbol * direction * expr list
             | ExprList of expr list
             | Params of param list
end

