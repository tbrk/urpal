(* $Id$ *)

(* TODO:
 *   - rename `paritition' to `constraint'
 *     more generally it is used to constrain the applicability of
 *     a set of transitions and is not affected by negation.
 *)

structure ClkExprTrans :> CLK_EXPR_TRANS = let
  structure E    = Expression
        and ClkE = ClockExpression
        and AT   = ActionTrans
        and Env  = Environment
        and ECVT = ExpressionCvt
in struct

  type expr      = Expression.expr
   and ty        = Expression.ty
   and symbol    = Atom.atom
   and symbolset = AtomSet.set

  exception SelectIdConflictsWithForAll of symbol list * symbol list

  infix <+ <- ++ <\ \ =:= ; open Symbol
  
  (* Invariants:
       - names contains all free names and those bound by select and forall
       - the sets of names of the select and forall lists are disjoint
   *)
  datatype t = CETrans of {actselect: (symbol * E.ty) list,(* SelectSub ids *)
                           gselect:   (symbol * E.ty) list,(* Guard selects *)
                           forall:    (symbol * E.ty) list,(* in prenex form *)
                           partition: Expression.expr,
                           guard:     ClkE.t,
                           action:    ActionTrans.actionsub list,
                           names:     symbolset}

  fun addNameTypes xs = let
      fun add ((n, _), s) = s <+ n
    in foldl add emptyset xs end

  local (*{{{1*)
    fun showCheckedActions []      = ""
      | showCheckedActions (AT.SelectSub s::es)   = "[#" ^ Atom.toString s ^ "#]" ^
                                                 showCheckedActions es
      | showCheckedActions (AT.FreeExprSub e::es) = "[" ^ ECVT.Expr.toString e ^ "]" ^
                                                 showCheckedActions es
    fun symtyToStr (s, ty) = Atom.toString s ^ " : " ^ ECVT.Ty.toString ty
    val symtyList = (ListFormat.fmt {init="(", sep=", ",
                                     final=")", fmt=symtyToStr})
  in (*}}}1*)
  fun toString (CETrans {actselect, gselect, forall,
                         partition, guard, action, names}) = let
    in
      "{actselect: " ^ symtyList actselect          ^ "\n" ^
      " gselect: "   ^ symtyList gselect            ^ "\n" ^
      " forall: "    ^ symtyList forall             ^ "\n" ^
      " partition: " ^ ECVT.Expr.toString partition ^ "\n" ^
      " guard: "     ^ ClkE.toString guard          ^ "\n" ^
      " action: "    ^ showCheckedActions action    ^ "\n" ^
      " names: "     ^ AtomSet.foldl
                       (fn (s, str)=> str ^ " " ^ Atom.toString s) "" names ^
      " }\n"
    end
  end (* local *)

(*
(*TODO: DEBUG ONLY! *)
fun showPartitions showitem partitions = let
    fun showClass c = let in
        TextIO.print "    class[\n";
        map showitem c;
        TextIO.print "    class]\n"
      end

    fun showPart p = let in
        TextIO.print "partition[\n";
        map showClass p;
        TextIO.print "partition]\n"
      end
  in app showPart partitions end
*)

  fun unionNames cetrans = let
      fun f (CETrans {names, ...}, m) = m ++ names
    in foldl f emptyset cetrans end

  fun addDisjointForalls newforall
      (CETrans {actselect, gselect, forall, partition, guard, action, names}) =
    let
      fun addAndCheck ((n, _), s) = if n <- s
                                    then raise Fail "addDisjointForalls"
                                    else s <+ n
    in
      CETrans {actselect=actselect,
               gselect=gselect,
               forall=newforall@forall,
               partition=partition,
               guard=guard,
               action=action,
               names=foldl addAndCheck names newforall}
    end

  fun fromATrans preenv (AT.ActTrans {selectids=sellist,
                                      actionsubs, guard, names}) =
    let
      val senv = List.foldl (Env.addId Env.SelectScope) preenv sellist
      val (guard, forall, used) = ClkE.fromExpr (names, senv, guard)
        (* Note: ClkE.fromExpr ensures:
         *         intersection(selectids, forall) = emptyset
         *       as names includes selectids.
         *)
      val actionNames = AT.addSelectSubNames actionsubs

      fun categorize ([], acts, gs) = (acts, gs)
        | categorize ((s as (n, _))::ss, acts, gs) =
            if n <- actionNames then categorize (ss, s::acts, gs)
                                else categorize (ss, acts, s::gs)

      val (actsels, gsels) = categorize (sellist, [], [])
    in
       CETrans {actselect=actsels,
                gselect=gsels,
                forall=forall,
                partition=E.trueExpr,
                guard=guard,
                action=actionsubs,
                names=used}
    end


  local (**)
    fun toBoundId (s, ty) = E.BoundId (s, ty, E.nopos)
  in (**)
  fun toTrans (CETrans {actselect, gselect, forall,
                        partition, guard, action, ...}) = let
      val preguard = ClkE.toExpr (guard, forall)
      val guard = if E.equal (partition, E.trueExpr) then preguard
                  else E.BinBoolExpr {left=partition, bop=E.AndOp,
                                      right=preguard, pos=E.nopos}
    in
      {selectids=map toBoundId (actselect @ gselect),
       actionsubs=map AT.actionsubToExpr action,
       guard=guard}
    end
  end (* local *)

  fun rename (CETrans {actselect, gselect, forall, partition,
                       guard, action, names}, r as {old, new}) =
    let
      val _ = if new <- names
              then raise Fail "rename: new name is already bound"
              else ()

      fun rsel (sv, sty) = if sv =:= old then (new, sty) else (sv, sty)

      fun renameAction (a as AT.SelectSub n) = if n =:= old
                                               then AT.SelectSub new else a
        | renameAction (AT.FreeExprSub e) = AT.FreeExprSub (E.renameVar (r, e))
    in
      CETrans {actselect=map rsel actselect,
               gselect=map rsel gselect,
               forall=map rsel forall,
               partition=E.renameVar (r, partition),
               guard=ClkE.rename (r, guard),
               action=map renameAction action,
               names=(names <\ old) <+ new}
    end

  local (*{{{1*)
    fun pairwiseIntersect ([], i)    = i
      | pairwiseIntersect (x::ys, i) = let
            fun withEach (y,i) = AtomSet.union (AtomSet.intersection(x,y), i)
          in pairwiseIntersect (ys, foldl withEach i ys) end

    fun makeTrans (actselects, potselects, potforalls, part, act) andx = let
        fun combineAnd (t, e) = ClkE.And (e, ClkE.Term t)
        fun makeAnd []      = ClkE.Term (ClkE.NonClock (E.falseExpr))
          | makeAnd (x::xs) = List.foldl combineAnd (ClkE.Term x) xs

        val e=makeAnd andx
        val exprNames=ClkE.getFree e
        val names=exprNames ++ AT.addActionNames act
        
        val forAlls=List.filter (fn (nm, _)=> nm<-exprNames) potforalls

      in (CETrans {actselect=actselects,
                   gselect=List.filter (fn (nm,_)=> nm<-names) potselects,
                   forall=forAlls,
                   partition=part,
                   guard=e,
                   action=act,
                   names=names}, addNameTypes forAlls)
      end

  in (*}}}1*)
  fun negate invariant (CETrans {actselect, gselect, forall,
                                 guard, partition, action, names}) =
      if ClkE.conflictExists (addNameTypes gselect, addNameTypes forall, guard)
      then raise SelectIdConflictsWithForAll (#1 (ListPair.unzip gselect),
                                              #1 (ListPair.unzip forall))
      else let
             val g' = ClkE.negate guard
             val e  = case invariant of
                        ClkE.Term (ClkE.NonClock (E.BoolCExpr true)) => g'
                      | _ => ClkE.And (invariant, g')

              val (trans, fall)=ListPair.unzip (map
                      (makeTrans (actselect, forall, gselect,
                                  partition, action)) (ClkE.toDNF e))
                 (* NB: forall and gselect are switched! (after having ensured
                  *     names are disjoint, and `non-conflicting') *)

              val splitFAlls=pairwiseIntersect (fall, emptyset)
           in
              if AtomSet.isEmpty splitFAlls then trans
              else (Util.warn ["forall bindings ",
                               ListFormat.fmt {init="(", sep=", ", final=")",
                                               fmt=Atom.toString}
                                 (AtomSet.listItems splitFAlls),
                           " are shared across disjuncts: likely split zones"];
                    [CETrans {actselect=actselect,
                              gselect=forall,
                              forall=gselect,
                              partition=partition,
                              guard=e,
                              action=action,
                              names=names}])
              (* An improvement would be to split where possible,
                 i.e. partition on shared foralls, transitive on overlap. *)
           end

  end (* local *)

  local (*{{{1*)
    fun equateActions ([], [], e) = e
      | equateActions (AT.FreeExprSub e1::ss1, AT.FreeExprSub e2::ss2, e) =
          if E.equal (e1, e2)
          then equateActions (ss1, ss2, e)
          else let
                 val eq = E.RelExpr {left=e1, rel=E.EqOp, right=e2,
                                     pos=E.nopos}
                 val e' = if E.equal (e, E.trueExpr) then eq
                          else E.BinBoolExpr {left=e, bop=E.AndOp, right=eq,
                                              pos=E.nopos}
               in equateActions (ss1, ss2, e') end

      | equateActions ((AT.SelectSub _)::ss1, (AT.SelectSub _)::ss2, e) =
                equateActions (ss1, ss2, e)

      | equateActions _ = raise Fail "equateActions: assumptions not met"

    fun distinguishActions (ss1, ss2, e) = let
        val ne = E.negate (equateActions (ss1, ss2, E.trueExpr))
      in
        if E.equal (e, E.trueExpr) then ne
        else E.BinBoolExpr {left=e, bop=E.AndOp, right=ne, pos=E.nopos}
      end
          
    fun equateClass (x::xs, e) = let
        fun equate (x', e) = equateActions (x, x', e)
      in foldl equate e xs end

    (* Form an expression that equates all members of the same class.
       e.g. given: [ [ [e1, e2], [f1, f2], [g1, g2] ],
                     [ [h1, h2], [i1, i2] ] ]

            returns:    (e1 == f1) && (e2 == f2)
                     && (e1 == g1) && (e2 == g2)
                     && (h1 == i1) && (h2 == i2)                    *)
    val equateClasses = foldl equateClass E.trueExpr

    (* Form an expression that distinguish classes across a partition,
       e.g. given: [ [ [e1, e2], [f1, f2], [g1, g2] ],
                     [ [h1, h2], [i1, i2] ]
                     [ [j1, j2], [k1, k2] ] ]

            returns: e && !((e1 == h1) && (e2 == h2))
                       && !((e1 == j1) && (e2 == j2))
                       && !((h1 == j1) && (h2 == j2))               *)
    fun distinguishClasses ([c], e) = e
      | distinguishClasses ((cRep::_)::cs, e) = let
            fun distinguish (oRep::_, e) = distinguishActions (cRep, oRep, e)
          in
            distinguishClasses (cs, foldl distinguish e cs)
          end

    (* Given a partition of action subscripts, form an expression that
       distinguishes the partition from others. *)
    fun formPartitionExpr part = distinguishClasses (part, equateClasses part)

    fun projActions partition = let
        fun projAction (CETrans {action, ...}) = action
      in map (fn class=> map projAction class) partition end

    fun markPartition (CETrans {actselect, gselect, forall, partition,
                                guard, action, names}, partexpr) =
        CETrans {actselect=actselect, gselect=gselect, forall=forall,
               partition=partexpr, guard=guard, action=action, names=names}

  fun renameConflicts ren args = let
  (* args: names_to_check * [] * expr_to_rename_in * conflict_set * usednames *)
    fun doit ([], done, g, conflicts, used) = (rev done, g, used)
      | doit ((s as (n, ty))::ss, done, g, conflicts, used) =
      if n <- conflicts
        then let val n' = getNewName (n, used)
                 val g' = ren ({old=n, new=n'}, g)
             in
               doit (ss, (n', ty)::done, g', conflicts, used <+ n')
             end
        else doit (ss, s::done, g, conflicts, used)
    in doit args end

    fun mergeTrans partexpr
        (CETrans {actselect=asel1, gselect=gsel1, forall=fa1,
                  partition=p1, guard=g1, action=act1, names=n1},
         CETrans {actselect=asel2, gselect=gsel2, forall=fa2,
                  partition=p2, guard=g2, action=act2, names=n2}) =
      let
        val asel1names = addNameTypes asel1
        val clash = asel1names ++ addNameTypes gsel1 ++ addNameTypes fa1
        val (gsel2', g2', names') = renameConflicts ClkE.rename
                                      (gsel2, [], g2, clash, n1++n2)
        val (fa2',   g2', names') = renameConflicts ClkE.rename
                                      (fa2, [], g2', clash, names')
      in
        CETrans {actselect=asel1
                     @ List.filter (fn (n,_)=> not (n<- asel1names)) asel2,
                     (* probably unnecessary after ATrans.ensureConsistency *)
                 gselect=gsel1 @ gsel2',
                 forall=fa1 @ fa2',
                 partition=partexpr,
                 guard=ClkE.Or (g1, g2'),
                 action=act1,
                 names=names'}
      end

  in (*}}}1*)
  (* Map each class in a partition to a single transition.
     All transitions are stamped with the same partition expression *)
  fun formPartitionReps [[]] = []
    | formPartitionReps trpart = let
          val partexpr = formPartitionExpr (projActions trpart)
          val mergeT = mergeTrans partexpr
          fun mergeClass (c::cs) = foldl mergeT (markPartition (c, partexpr)) cs
        in map mergeClass trpart end
  end (* local *)

end
end

