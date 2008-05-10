(* $Id$ *)

(*TODO:
  * Check whether duplicate selectids can muck things up.
  * Remove unused select ids from final transitions (or initial transitions?).
    fun removeEarlierDuplicates l = let (* there can be only one *)
        fun p s (s', _) = not (s =:= s')
        fun f []                  = []
          | f ((x as (s, _))::xs) = x :: f (List.filter (p s) xs)
      in rev (f (rev l)) end

    fun unusedId used (E.BoundId (s, ty)) = if s <- used
                                            then SOME (s, ty) else NONE

    val sellist = removeEarlierDuplicates
                    (List.mapPartial (unusedId (!usednames)) presellist)

  * What happens if we call negate(negate tr)?
    Should this be forbidden by using types?

  * Should check whether any actselect indices are used in guard terms in a way
    that will create problems (restrict how actions are selected?).
      e.g. select i . (i > 4) / a[i]
                  ||negate
                  \/
           select i . (i <= r) / a[i]
     But how would this combine with other transitions? Does it work?
     This must first be understood properly.

  * What about action selectids with non-zero lower bounds?
        chan c[6];
        select i : int[2,5] . true / c[i]

    Either forbid, or the negation set has to include:
        select i : int [0,1] . true / c[i]

  * Need a mapping back from scalar sets to their typedef-ed names (possibly via
    uniqueid lookup?, or don't expand to begin with (would this break anything
    else?). See tests/testflip13, the selectid should come from 'S', not
    scalar[5].

 *)

structure TransitionFlipper :> TRANSITION_FLIPPER = let
  structure E       = Expression
        and Env     = Environment
        and ECVT    = ExpressionCvt
        and ATrans  = ActionTrans
        and STrans  = SelTrans
        and ClkE    = ClockExpression
        and CETrans = ClkExprTrans
in struct
  exception FlipFailed of string

  (* shortcuts over Atom and AtomSet *)
  infix <+ <- ++ <\ \ =:= ; open Symbol

  type t = {selectids:  E.boundid list,
            actionsubs: E.expr list,
            guard:      E.expr}

  fun toString {selectids, actionsubs, guard} = let
      fun showActions []      = ""
        | showActions (e::es) = "[" ^ ECVT.Expr.toString e ^"]"^ showActions es

      fun boundToStr (E.BoundId (s, ty)) = Atom.toString s ^
                                              " : " ^ ECVT.Ty.toString ty
      val bindingList = (ListFormat.fmt {init="(", sep=", ",
                                         final=")", fmt=boundToStr})
    in
      "{select: "  ^ bindingList selectids    ^ "\n"  ^
      " action: " ^ showActions actionsubs   ^ "\n " ^
      " guard:  " ^ ECVT.Expr.toString guard ^ " }"  ^ "\n"
    end

  local
    fun convBind (E.BoundId (nm, ty), (xs,used)) = ((nm, ty)::xs, used <+ nm)
  in
  fun andexpr env (ce1se, e1, e2) = let
      val (ce1se', used) = foldl convBind ([], emptyset) ce1se

      val (ce1, ce1fa, used) = ClkE.fromExpr (used, env, e1)
      val (ce2, ce2fa, _   ) = ClkE.fromExpr (used, env, e2)

      val (ce2fa', ce2') =  ClkE.ensureNoBindingConflict (ce1fa @ ce1se',ce1)
                                                         (ce2fa,ce2)
    in ClkE.toExpr (ClkE.andexpr (ce1, ce2'), ce1fa @ ce2fa') end
  end (* local *)

  fun negateInvariant env invExpr = let
      val _ = Util.debugIndent (Settings.Outline,fn()=>["=negateInvariant="])

      val atr = ActionTrans.fromTrans []
                  {selectids=[], actionsubs=[], guard=invExpr}
      val ctr = (CETrans.fromATrans env) atr

      val _ = Util.debugDetailed (fn ()=>["* before:\n", CETrans.toString ctr])

      val ctrs' = CETrans.negate ([], ClkE.trueExpr) ctr

      val _ = Util.debugDetailed (fn ()=> "* after:\n"
            ::map (fn c=>CETrans.toString c ^"\n") ctrs')
    in
      map CETrans.toTrans ctrs'
      before (Util.debugOutdent (Settings.Outline, fn()=>[]))
    end
      handle ClkE.NonClockTerm => raise FlipFailed "bad clock terms in invariant"

  (* XXX: partitioning technique *)
  fun negatePartitionedTransitions env (subtypes, trans : t list, invariant) =
    let
      val _ = Util.debugIndent (Settings.Outline,fn()=>["=negatePartitioned="])
      val _ = Util.debugDetailed (fn ()=>map toString trans)

      (* 1. Group like subscripts; rename subSelectIds; make others unique. *)
      val atrans = ATrans.ensureConsistency
                     (map (ATrans.fromTrans subtypes) trans)
      val otherATrans = ATrans.coverMissingChannels (subtypes, atrans,
                                                     invariant)
      val atrans' = map (ATrans.reduceSelectIds env) atrans
      val cetrans = map (CETrans.fromATrans env) atrans'
      
      val _ = Util.debugSubsect (Settings.Outline, fn()=>
          ["* cover missing channels:",
           if null otherATrans then " nothing" else "\n"]
          @ map ATrans.toString otherATrans)

      (* 2. Paritition remaining transitions. *)
      val partitions = Partitions.makeList cetrans

      (* 3. Form partition expressions: just over freeSubscripts
            Grouping back into a single list of transitions. *)
      val cetrans = List.concat (List.map CETrans.formPartitionReps partitions)

      val _ = Util.debugDetailed (fn ()=>
          ["* with partition reps:", if null cetrans then " nothing" else "\n"]
          @ map CETrans.toString cetrans)

      (* 4. Convert the location invariant into a clock expression *)
      val (cinv, cinv_fall, _) = ClkE.fromExpr (CETrans.unionNames cetrans,
                                                env, invariant)

      (* 5. Negate each, concatenating results. *)
      val ncetrans = List.concat (List.map (CETrans.negate (cinv_fall, cinv))
                                           cetrans)

      val _ = Util.debugDetailed (fn ()=>
          ["* after negation:", if null ncetrans then " nothing" else "\n"]
          @ map CETrans.toString ncetrans)

    in map ATrans.toTrans otherATrans @ map CETrans.toTrans ncetrans
       before (Util.debugOutdent (Settings.Outline, fn()=>[]))
    end
      handle CETrans.SelectIdConflictsWithForAll (sel, forall) => let
                fun showSym n = ListFormat.fmt {init=n ^ "(", final=")",
                                                sep=", ", fmt=Atom.toString}
              in
               raise FlipFailed ("select/forall conflict, " ^
                                 showSym "select" sel       ^
                                 showSym "forall" forall    )
              end

           | ClkE.NonClockTerm => raise FlipFailed "bad clock terms in guard"

           | ATrans.ActSubWithNonSimpleSelect e      => raise FlipFailed (
                  "channel subscript contains non-simple bound variable (" ^
                  ExpressionCvt.Expr.toString e ^ ")")

           | ATrans.ActSubWithBadType {expr, badty, goodty} => raise FlipFailed
                ("channel subscript select variable " ^
                 ExpressionCvt.Expr.toString expr  ^ " has type " ^
                   ExpressionCvt.Ty.toString badty ^ ", not "     ^
                   ExpressionCvt.Ty.toString goodty)
                  
           | ATrans.BadSubscriptCount                =>
                raise FlipFailed ("wrong number of channel subscripts")

           | ATrans.MixedSubscriptTypes (e1, e2) => raise FlipFailed
                ("non-consistent channel subscript dimensions (" ^
                 ExpressionCvt.Expr.toString e1 ^ ", " ^
                 ExpressionCvt.Expr.toString e2 ^ ")")

  fun negateTransitions env (subtypes, trans : t list, invariant) = let
      val _ = Util.debugIndent (Settings.Outline,fn()=>["=negateTransitions="])
      val _ = Util.debugDetailed (fn ()=>map toString trans)

      (* 1. Group like subscripts; rename subSelectIds; make others unique. *)

      val subIdx = ListPair.zip
                     (STrans.makeIndexNames (length subtypes) trans, subtypes)
      val strans = map (STrans.fromTrans subIdx) trans
      val cetran = CETrans.fromSTrans env subIdx (STrans.mergeTrans strans)

      (* 2. Convert the location invariant into a clock expression *)
      val (cinv, cinv_fall, _) = ClkE.fromExpr (CETrans.unionNames [cetran],
                                                env, invariant)

      (* 3. Negate the transition. *)
      val ncetrans = CETrans.negate (cinv_fall, cinv) cetran

      val _ = Util.debugDetailed (fn ()=>
          ["* after negation:", if null ncetrans then " nothing" else "\n"]
           @ map CETrans.toString ncetrans)

    in map CETrans.toTrans ncetrans
       before (Util.debugOutdent (Settings.Outline, fn()=>[]))
    end
      handle CETrans.SelectIdConflictsWithForAll (sel, forall) => let
                fun showSym n = ListFormat.fmt {init=n ^ "(", final=")",
                                                sep=", ", fmt=Atom.toString}
              in
               raise FlipFailed ("select/forall conflict, " ^
                                 showSym "select" sel       ^
                                 showSym "forall" forall    )
              end

           | ClkE.NonClockTerm => raise FlipFailed "bad clock terms in guard"

           | STrans.NonSimpleSelectIndex e      => raise FlipFailed (
                  "channel subscript contains non-simple bound variable (" ^
                  ExpressionCvt.Expr.toString e ^ ")")

           | STrans.BadChannelIndex {id, badty, goodty} => raise FlipFailed
                ("channel subscript select variable " ^
                  Atom.toString id ^ " has type " ^
                   ExpressionCvt.Ty.toString badty ^ ", not "     ^
                   ExpressionCvt.Ty.toString goodty)
                  
           | STrans.BadSubscriptCount                =>
                raise FlipFailed ("wrong number of channel subscripts")

  fun chanToSubRanges (env, s) = let
      fun arrToList (E.CHANNEL _, r)              = SOME r
        | arrToList (E.ARRAY (ty, E.Type sub), r) = arrToList (ty, sub::r)
        | arrToList _                             = NONE
    in
      Option.composePartial (fn ty=> arrToList (ty, []), Env.findValType env) s
    end

end
end

