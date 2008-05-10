(* $Id$ *)

structure Result : RESULT =
struct
  type expr      = Expression.expr
  type boundid   = Expression.boundid
  type direction = Expression.direction
  type decl      = Declaration.decl
  type param     = Declaration.param
  type symbol    = Atom.atom

  datatype t = Expr of expr
             | Decls of decl list
             | Select of boundid list
             | Sync of symbol * direction * expr list
             | ExprList of expr list
             | Params of param list
end

