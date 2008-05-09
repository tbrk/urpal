(* $Id$
 *
 * 20070615 T. Bourke
 *
 * Based on Uppaal Timed Automata Parser Library documentation
 * http://www.cs.auc.dk/~behrmnn/utap/syntax.html 20070424
 *
 * Adapted from `Modern compiler implementation in ML', Appel 1998.
 *
 *)

structure Declaration : DECLARATION where type pos  = Expression.pos
                                      and type ty   = Expression.ty
                                      and type expr = Expression.expr
=
struct
  structure E = Expression

  type pos = Expression.pos
  type symbol = Atom.atom
  type ty = E.ty
  type expr = E.expr

  datatype param    = ValParam of {id: symbol, ty: ty}
                    | RefParam of {id: symbol, ty: ty}
  datatype chanexpr = ChanSingle of symbol
                    | ChanArray  of chanexpr * expr
                    | ChanDefault
  datatype initialiser = SimpleInit of expr
                       | ArrayInit  of initialiser list

  datatype decl = TyDecl  of {id: symbol, ty: ty, pos: pos}
                | VarDecl of {id: symbol, ty: ty,
                              initial: initialiser option, pos: pos}
                | FunDecl of {id: symbol,
                              rty: ty,
                              params: param list,
                              body: stmt,
                              pos: pos}
                | ChanPriDecl of ((chanexpr list) list) * pos
  
  and stmt      = BlockStmt of {decls: decl list, body: stmt list}
                | ExprStmt of E.expr * pos
                | ForStmt of {init: E.expr list,
                              cond: E.expr list,
                              step: E.expr list,
                              body: stmt,
                              pos: pos}
                | IterateStmt of {id: symbol,
                                  ty: ty,
                                  body: stmt,
                                  pos: pos}
                | WhileStmt of {cond: E.expr list,
                                body: stmt,
                                pos: pos}
                | DoWhileStmt of {cond: E.expr list,
                                  body: stmt,
                                  pos: pos}
                | IfStmt of {cond: E.expr list,
                             thenb: stmt,
                             elseb: stmt,
                             pos: pos}
                | Return of expr option * pos
                | NothingStmt

  fun declToId (TyDecl {id, ...})  = SOME id
    | declToId (VarDecl {id, ...}) = SOME id
    | declToId (FunDecl {id, ...}) = SOME id
    | declToId (ChanPriDecl _)     = NONE

end

