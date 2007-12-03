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
 *
 * Questions:
 *   * Are expressions statically evaluated when given as:
 *      -scalar size?
 *      -integer bounds?
 *      -array size?
 *)

signature EXPRESSION =
sig
  type pos
  val nopos : pos

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

  datatype var        = SimpleVar of symbol * pos
                      | ReturnVar of {func: symbol, args: expr list, pos: pos}
                      | RecordVar of var * symbol * pos
                      | SubscriptVar of var * expr * pos

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
                      | CallExpr     of {func: symbol,
                                         args: expr list,
                                         pos: pos}
                      | NegExpr      of {expr: expr, pos: pos}
                      | NotExpr      of {expr: expr, pos: pos}
                      | UnaryModExpr of {uop: unaryModOp,
                                         expr: expr,
                                         pos: pos}
                      | BinIntExpr   of {left: expr,
                                         bop: binIntOp,
                                         right: expr,
                                         pos: pos}
                      | BinBoolExpr  of {left: expr,
                                         bop: binBoolOp,
                                         right: expr,
                                         pos: pos}
                      | RelExpr      of {left: expr,
                                         rel: rel,
                                         right: expr,
                                         pos: pos}
                      | AssignExpr   of {var: expr,
                                         aop: assignOp,
                                         expr: expr,
                                         pos: pos}
                      | CondExpr     of {test: expr,
                                         trueexpr: expr,
                                         falseexpr: expr,
                                         pos: pos}
                      | ForAllExpr   of {id: symbol,
                                         ty: ty,
                                         expr: expr,
                                         pos: pos}
                      | ExistsExpr   of {id: symbol,
                                         ty: ty,
                                         expr: expr,
                                         pos: pos}
                      | Deadlock     of pos
                      
  and      boundid    = BoundId of symbol * ty * pos

  and unresolvedty = Type of ty
                   | Unresolved of symbol  (* could be a typedef or
                                              a variable (INT[0, x]) *)

  val varPos : var -> pos

  val filter : (expr -> bool) -> expr -> expr list

  val getFreeNames : expr -> AtomSet.set
  val renameVar  : {old : symbol, new: symbol} * expr -> expr
  val renameVars : symbol AtomMap.map -> expr -> expr

  val stripArray : ty -> (ty * ty list)

  val trueExpr  : expr
  val falseExpr : expr

  (* Looks (with over-approximation) through Boolean formulae for terms that
     contain variables from both sets, returning true if one is found.      *)
  val conflictExists : AtomSet.set * AtomSet.set * expr -> bool

  val otherDirection : direction -> direction

  (* Ignores pos values.
     Strictly syntactic equality (e.g. ignores commutativity).
     Assumes functions return the same result given the same input arguments.
      (i.e. referentially transparent and side-effect free)
     Cost proportional to expression size *)
  val equal    : expr * expr -> bool
  val varequal : var * var   -> bool
  val tyequal  : ty * ty     -> bool

  val inc      : expr -> expr (* add one to an expression *)
  val dec      : expr -> expr (* substract one from an expression *)

  (* Assumes the expression is of type BOOL, returns the logical negation.
     May alter the top-level pos value. *)
  val negate : expr -> expr

  val andexpr : expr * expr -> expr
  val orexpr : expr * expr -> expr

  val mulClocks : expr * (symbol -> bool) -> expr -> expr
    (* mulClocks (m, isClockVar) e
     * Work through e, multiplying the following by m:
     *  -RHS of assignments to clocks.
     *  -Either side of a relation involving a clock.
     *)

  (* returnval => ((e1 && e2) == false)
     Simple and conservative syntactic checks, i.e. sound but not complete
     
     guarantees:
        isNegation(e, negate e) = true
        isNegation(negate, e)   = true
    *)
  val isNegation : expr * expr -> bool

end

