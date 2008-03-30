(* $Id$ *)

signature CLOCK_EXPRESSION = sig

  exception NonClockTerm

  type symbol    = Atom.atom
   and symbolset = AtomSet.set

  datatype clockrel = Lt | Leq | Eq | Geq | Gt

  datatype clockval = Simple  of int
                    | Complex of Expression.expr

  datatype clockterm = NonClock of Expression.expr
                     | CRel     of Expression.var * clockrel * clockval
                     | CDiff    of Expression.var * Expression.var
                                   * clockrel * clockval

  datatype t = Term of clockterm
             | And  of t * t
             | Or   of t * t

  val trueExpr       : t
  val falseExpr      : t

  val negate         : t -> t
  val getFree        : t -> symbolset

  val fromExpr       : symbolset * Environment.env * Expression.expr
                       -> t * (symbol * Expression.ty) list * symbolset
                       (* raises NonClockTerm *)
  (* usednames * environment * expression to convert
   *    -> result * forall bindings * usednames' -- now in prenex form *)

  val toExpr         : t * (symbol * Expression.ty) list -> Expression.expr

  val rename         : {old: symbol, new: symbol} * t -> t
  val conflictExists : symbolset * symbolset * clockterm list list -> bool 

  val toDNF          : t -> clockterm list list
  val fromDNF        : clockterm list list -> t
  val andexpr        : t * t -> t

  val toString       : t -> string

end

