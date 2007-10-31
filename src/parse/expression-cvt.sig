(* $Id$ *)

signature EXPRESSION_CVT =
sig
  type expr = Expression.expr
   and ty   = Expression.ty
   and var  = Expression.var
   and decl = Declaration.decl
  
  type outstream = TextIO.StreamIO.outstream

  structure Expr : sig
      val toString : expr -> string
      val toStream : outstream * expr -> unit
    end

  structure Ty : sig
      val toString : ty -> string
      val toStream : outstream * ty -> unit
    end

  structure Var : sig
      val toString : var -> string
      val toStream : outstream * var -> unit
    end

  structure Decl : sig
      val toStream : outstream * decl -> unit
    end

  val selectToStream    : outstream * Expression.boundid list -> unit
  val selectToString    : Expression.boundid list -> string
  val syncToStream      : outstream * (Expression.symbol *
                            Expression.direction * expr list) -> unit
  val syncToString      : (Expression.symbol * Expression.direction
                                             * expr list) -> string
  val exprlistToStream  : outstream * Expression.expr list -> unit
  val exprlistToString  : Expression.expr list -> string
  val paramlistToStream : outstream * Declaration.param list -> unit

  val width : int ref
end

