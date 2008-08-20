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

signature DECLARATION =
sig
  structure E : EXPRESSION

  type symbol = Atom.atom
  type pos
  type expr
  type ty

  sharing type expr = E.expr
  sharing type ty = E.ty

  val nopos : pos

  datatype param    = ValParam of {id: symbol, ty: ty}
                    | RefParam of {id: symbol, ty: ty}
  datatype chanexpr = ChanSingle of symbol
                    | ChanArray  of chanexpr * expr
                      (* ChanArray should not contain ChanDefault*)
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

  val declToId : decl -> symbol option
end

