(* $Id$ *)

signature ACTION_TRANS = sig

  type expr      = Expression.expr
   and ty        = Expression.ty
   and boundid   = Expression.boundid
   and symbol    = Atom.atom
   and symbolset = AtomSet.set

  exception MixedSubscriptTypes of expr * expr
  exception ActSubWithNonSimpleSelect of expr
  exception ActSubWithBadType of {expr: expr, badty: ty, goodty: ty}
  exception ActionSubWithDuplicate of symbol
  exception BadSubscriptCount

  (* transition actions have been checked against restrictions *)
  datatype actionsub = SelectSub of symbol | FreeExprSub of expr
  datatype t = ActTrans of {selectids:  (symbol * ty) list,
                            actionsubs: actionsub list,
                            guard:      expr,
                            names:      symbolset}

  val addSelectSubNames : actionsub list -> symbolset
  val addFreeExprNames  : actionsub list -> symbolset
  val addActionNames    : actionsub list -> symbolset
  val actionsubToExpr   : actionsub -> expr

  val fromTrans : ty list -> {selectids:  Expression.boundid list,
                              actionsubs: Expression.expr list,
                              guard:      Expression.expr} -> t
  (* Transform the given transition by categorising each action subscript as
     either:
       * a SelectSub (index is single select identifier),
       * or a FreeExprSub (index expression contains no select identifiers).
     Ensuring that SelectSub identifiers select from the entire subscript range
     (i.e. their type matches the corresponding array index type.)

     TODO: comments about renaming select ids
   *)
  val toTrans   : t -> {selectids:  Expression.boundid list,
                        actionsubs: Expression.expr list,
                        guard:      Expression.expr}

  val ensureConsistency : t list -> t list
  (* If two transitions refer to the same action then group them together.

     Ensure that action subscripts in the same dimension have the same
     category (both are either a FreeExprSub or both are a SelectSub).
     Ensure that SelectSub subscripts in the same dimension have the same name
     (i.e. call renameSelectIds first)
   *)
  (* For the given set of transitions, ensure that all SelectSubs against the
   * same array dimension have the same name. *)

  val coverMissingChannels : ty list * t list * Expression.expr -> t list
  (* TODO: comments
   * the last expr is the location invariant. *)

  val toString             : t -> string

end

