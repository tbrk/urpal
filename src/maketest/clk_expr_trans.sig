(* $Id$ *)

signature CLK_EXPR_TRANS = sig
  type symbol    = Atom.atom
   and symbolset = AtomSet.set

  exception SelectIdConflictsWithForAll of symbol list * symbol list

  (* Invariants:
       - names contains all free names and those bound by select and forall
       - the sets of names of the select and forall lists are disjoint
   *)
  datatype t = CETrans of {actselect: (symbol * Expression.ty) list,
                           gselect:   (symbol * Expression.ty) list,
                           forall:    (symbol * Expression.ty) list,
                           partition: Expression.expr,
                           guard:     ClockExpression.t,
                           action:    ActionTrans.actionsub list,
                           names:     symbolset}
  (* actselect    - SelectSub ids
     gselect      - Guard selects
     forall/guard - prenex form
   *)

  val unionNames        : t list -> symbolset

  val fromATrans        : Environment.env -> ActionTrans.t -> t
    (* may raise ClockExpression.NonClockTerm *)

  val toTrans           : t -> {selectids: Expression.boundid list,
                                actionsubs: Expression.expr list,
                                guard:      Expression.expr}

  val rename            : t * {old: symbol, new: symbol} -> t
  val negate            : (symbol * Expression.ty) list * ClockExpression.t
                          -> t -> t list
    (* negate (inv_fall, inv) trans negated_trans
     * (inv_fall, inv) is the location invariant. The transition guard is
     * negated and then and-ed with inv, the inv_fall are added to the
     * transition's forall list (an exception is thrown if they are not
     * disjoint).
     * TODO: * separate negation and invariant and-ing
     *       * handle non-disjointness better?
     *)

  val formPartitionReps : t list list -> t list

  val toString          : t -> string

end

