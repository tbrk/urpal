(* $Id$ *)

(* Notes:
 *   * Uppaal does not permit recursive records (no pointers)
 *   * Uppaal does not permit array assignment
 *   * meta and const prefixes are incompatible and may only be used for
 *     `Integers, booleans, and arrays and records over integers and booleans
 *      can be marked as meta/const variables...'
 *   * Scalar: only assignment and identity testing.
 *   * Structs are not named.
 *   * The `urgent' qualifier must precede `broadcast' (behaviour of Uppaal).
 *   * The conditional expression (?:) may appear on the LHS of an assignment.
 *)

signature EXPRESSION =
sig
  type symbol = Atom.atom

  type unique = int
  val uniqueTag: unit -> unique

  datatype tyqual = NoQual | Meta | Const

  datatype direction = Output (* c! *)
                     | Input  (* c? *)

  datatype unaryModOp = PreIncOp  (*  ++_ *)
                      | PostIncOp (*  _++ *)
                      | PreDecOp  (*  --_ *)
                      | PostDecOp (*  _-- *)

  datatype binIntOp   = PlusOp    (*  +   *)
                      | MinusOp   (*  -   *)
                      | TimesOp   (*  *   *)
                      | DivideOp  (*  /   *)
                      | ModOp     (*  %   *)
                      | BAndOp    (*  &   *)
                      | BOrOp     (*  |   *)
                      | BXorOp    (*  ^   *)
                      | ShlOp     (*  <<  *)
                      | ShrOp     (*  >>  *)
                      | MinOp     (*  <?  *)
                      | MaxOp     (*  >?  *)

  datatype rel        = LtOp      (*  <   *)
                      | LeOp      (*  <=  *)
                      | EqOp      (*  ==  *)
                      | NeOp      (*  !=  *)
                      | GeOp      (*  >=  *)
                      | GtOp      (*  >   *)

  datatype binBoolOp  = AndOp     (*  &&  *)
                      | OrOp      (*  ||  *)
                      | ImplyOp   (*  imply *)

  datatype assignOp   = AssignOp  (* =/:= *)
                      | PlusEqOp  (*  +=  *)
                      | MinusEqOp (*  -=  *)
                      | TimesEqOp (*  *=  *)
                      | DivideEqOp(*  /=  *)
                      | ModEqOp   (*  %=  *)
                      | BOrEqOp   (*  |=  *)
                      | BAndEqOp  (*  &=  *)
                      | BXorEqOp  (*  ^=  *)
                      | ShlEqOp   (*  <<= *)
                      | ShrEqOp   (*  >>= *)

  datatype var        = SimpleVar of symbol
                      | ReturnVar of {func: symbol, args: expr list}
                      | RecordVar of var * symbol
                      | SubscriptVar of var * expr

  and      ty         = VOID
                      | INT of (expr * expr) option * tyqual
                      | BOOL of tyqual
                      | CLOCK
                      | CHANNEL of {urgent: bool, broadcast: bool}
                      | SCALAR of expr * tyqual * unique
                      | RECORD of (symbol * ty) list * tyqual * unique
                      | ARRAY of ty * unresolvedty
                      | NAME of symbol * tyqual * ty option
                        (* last component of NAME is the fully expanded
                           version incorporating the tyqual. *)

  and      expr       = VarExpr      of var
                      | IntCExpr     of int
                      | BoolCExpr    of bool
                      | CallExpr     of {func: symbol, args: expr list}
                      | NegExpr      of expr
                      | NotExpr      of expr
                      | UnaryModExpr of {uop: unaryModOp, expr: expr}
                      | BinIntExpr   of {left: expr,
                                         bop: binIntOp,
                                         right: expr}
                      | BinBoolExpr  of {left: expr,
                                         bop: binBoolOp,
                                         right: expr}
                      | RelExpr      of {left: expr,
                                         rel: rel,
                                         right: expr}
                      | AssignExpr   of {var: expr,
                                         aop: assignOp,
                                         expr: expr}
                      | CondExpr     of {test: expr,
                                         trueexpr: expr,
                                         falseexpr: expr}
                      | ForAllExpr   of {id: symbol,
                                         ty: ty,
                                         expr: expr}
                      | ExistsExpr   of {id: symbol,
                                         ty: ty,
                                         expr: expr}
                      | Deadlock
                      
  and      boundid    = BoundId of symbol * ty

  and unresolvedty = Type of ty
                   | Unresolved of symbol  (* could be a typedef or
                                              a variable (INT[0, x]) *)

  val varName : var -> string
    (* return a name suitable for debugging output and error messages *)

  val getFreeNames : expr -> AtomSet.set
  val getBoundNames : boundid list -> AtomSet.set
  val renameVar  : {old : symbol, new: symbol} * expr -> expr
  val renameVars : symbol AtomMap.map -> expr -> expr
  (* simultaneous renaming *)

  val stripArray : ty -> (ty * ty list)

  val trueExpr  : expr
  val falseExpr : expr

  val conflictExists : AtomSet.set * AtomSet.set * expr -> bool
  (* Looks (with over-approximation) through Boolean formulae for terms that
     contain variables from both sets, returning true if one is found.      *)

  val ensureNoBindingConflict : (boundid list * expr) ->
                                (boundid list * expr) -> (boundid list * expr)
  (* (l', e') = ensureNoBindingConflict (rl, re) (l, e)
   * Renames bound variables (l => l') in (e => e') to ensure that combination
   * with the reference expression will not capture names improperly.
   * 
   * e.g. it would then be safe to and the reference and result expressions:
   *          (al, ae) = (rl @ l', andexpr (re, e'))
   * *)


  val otherDirection : direction -> direction

  (* Strictly syntactic equality (e.g. ignores commutativity).
     Assumes functions return the same result given the same input arguments.
      (i.e. referentially transparent and side-effect free)
     Cost proportional to expression size *)
  val equal    : expr * expr -> bool
  val varequal : var * var   -> bool
  val tyequal  : ty * ty     -> bool

  val inc      : expr -> expr (* add one to an expression *)
  val dec      : expr -> expr (* substract one from an expression *)

  (* Assumes the expression is of type BOOL, returns the logical negation. *)
  val negate : expr -> expr

  val andexpr : expr * expr -> expr
  val orexpr : expr * expr -> expr

  val mulClocks : expr * (symbol -> bool) -> expr -> expr
    (* mulClocks (m, isClockVar) e
     * Work through e, multiplying the following by m:
     *  -RHS of assignments to clocks.
     *  -Either side of a relation involving a clock.
     *)

  val isNegation : expr * expr -> bool
  (* returnval => ((e1 && e2) == false)
     Simple and conservative syntactic checks, i.e. sound but not complete
     
     guarantees:
        isNegation(e, negate e) = true
        isNegation(negate, e)   = true
    *)

  val shrinkScope     : (symbol * ty * bool) * (expr -> bool)
                        -> expr -> expr option 
  (* Given a binding, true:    forall (symbol : ty)
   *                  false:   exists (symbol : ty)
   *
   * Find the smallest subexpression of expr that contains all occurrences
   * of symbol, and, if the given predicate is true of the subexpression
   * then return (SOME newexpr) such that the symbol is bound around the
   * subexpression. Return NONE if the predicate is false.
   *
   * If expr does not contain symbol, then (SOME expr) is returned.
   * (i.e. the binding is thrown away if it is unused.)
   *)

end

