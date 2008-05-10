(* $Id$ *)

signature SEL_TRANS = sig

  type expr      = Expression.expr
   and ty        = Expression.ty
   and symbol    = Atom.atom
   and symbolset = AtomSet.set

  exception NonSimpleSelectIndex of expr
  exception BadChannelIndex of {id: symbol, badty: ty, goodty: ty}
  exception BadSubscriptCount

  datatype t = SelTrans of {selectids:  (symbol * ty) list,
                            guard:      expr,
                            names:      symbolset}

  val makeIndexNames : int
                       -> {selectids:  Expression.boundid list,
                           actionsubs: Expression.expr list,
                           guard:      Expression.expr} list
                       -> symbol list
  (* makeIndexNames n ts
   * Return a list of n names for indexing a channel set. The new names will not
   * conflict with any non-selectids used in the guards (i.e. state variables)
   * Prefer names already used within the given list of transitions. *)

  val fromTrans : (symbol * ty) list
                  -> {selectids:  Expression.boundid list,
                      actionsubs: Expression.expr list,
                      guard:      Expression.expr}
                  -> t
  (* fromTrans asubs trans
   * Transform trans into a SelTrans whose channel set is indexed by asubs.
   *
   * The names in asubs must not conflict with non-selectids in guard and
   * actionsubs.
   *
   * First a mapping R is formed from:
   *    -first (needed to handle duplicates) selection bindings used
   *     in index to required bindings
   *        e.g.   R(s) = s_0
   *               R(t) = s_2
   *    -required bindings to fresh names (to avoid capture)
   *        e.g.   R(s_0) = s_0'
   *               R(s_1) = s_1'
   *               R(s_2) = s_2'
   * Then it is used for simultaneous renaming in the selectids and guard.
   *
   * This involves processing each index expression.
   * for a selectid i
   *    -if the type doesn't cover the whole dimension then && in a
   *     restriction clause (l <= s_i <= u) where i has the type int[l,u]
   *    -if used in an earlier index, s_j, then && the clause (s_i == s_j)
   *    -if doesn't match the current name then rename (s_i/i)
   *     and remove i from the list of selectids
   *
   *  for a state expression e:
   *    -throw an exception if it contains selectids
   *    -&& the clause (si == R'(e)) to the guard 
   *)

  val toTrans   : (symbol * ty) list
                  -> t
                  -> {selectids:  Expression.boundid list,
                      actionsubs: Expression.expr list,
                      guard:      Expression.expr}

  (* For the given set of transitions, ensure that all SelectSubs against the
   * same array dimension have the same name. *)

  val toString  : (symbol * ty) list -> t -> string

  val mergeTrans : t list -> t
  (* Merge a list of transitions that have been created for a common channel set
   * indexing list, taking care to avoid selection binding capture. *)

  (* TODO: update *)
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

