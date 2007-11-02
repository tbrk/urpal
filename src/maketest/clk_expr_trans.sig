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
  val addDisjointForalls : (symbol * Expression.ty) list -> t -> t
    (* adds the given bindings to the forall of the given transition
     * throws an exception if the new symbols are already in names. *)

  val fromATrans        : Environment.env -> ActionTrans.t -> t
    (* may raise ClockExpression.NonClockTerm *)

  val toTrans           : t -> {selectids: Expression.boundid list,
                                actionsubs: Expression.expr list,
                                guard:      Expression.expr}

  val rename            : t * {old: symbol, new: symbol} -> t
  val negate            : ClockExpression.t -> t -> t list
    (* the given invariant expression is and-ed to the transition guard _after_
     * negation. This is admittedly ugly. *)

  val formPartitionReps : t list list -> t list

  val toString          : t -> string

end

