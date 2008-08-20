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

signature ACTION_TRANS = sig

  type expr      = Expression.expr
   and ty        = Expression.ty
   and boundid   = Expression.boundid
   and symbol    = Atom.atom
   and symbolset = AtomSet.set

  exception MixedSubscriptTypes of expr * expr
  exception ActSubWithNonSimpleSelect of expr
  exception ActSubWithBadType of {expr: expr, badty: ty, goodty: ty}
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

  val reduceSelectIds : Environment.env -> t -> t
  (* Assuming: update expressions are ignored
   *
   * Each select binding that:
   *    1) is not used in a SelectSub
   *    2) is not involved in a sub-expression containing clocks
   * is turned into an exists binding.
   *
   * Condition (1) and the assumption mean that the scope of the binding is only
   * important inside the guard expression.
   *
   * Condition (2) ensures that the new guard expression does not split clock
   * zones.
   *
   * Doing this allows maketest to negate transitions that might otherwise fail
   * with a select/forall conflict, such as:
   *    {selectids:  (i : int[0,N - 1])
   *     actionsubs:
   *     guard:      get_status(i)==APPR &&
   *                 (forall (j : int[0,N - 1])
   *                    j!=i && get_status(j)!=AWAY imply status[j][i]<M
   *                 )
   *)
end

