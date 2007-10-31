(* $Id$ *)

structure ActionTrans :> ACTION_TRANS = let
  structure E    = Expression
        and ECVT = ExpressionCvt
in struct
  type expr      = Expression.expr
   and ty        = Expression.ty
   and boundid   = Expression.boundid
   and symbol    = Atom.atom
   and symbolset = AtomSet.set

  exception MixedSubscriptTypes of E.expr * E.expr
  exception ActSubWithNonSimpleSelect of E.expr
  exception ActSubWithBadType of {expr:E.expr, badty: E.ty, goodty: E.ty}
  exception ActionSubWithDuplicate of symbol
  exception BadSubscriptCount

  infix <+ <- ++ </ =:= ; open Symbol
  
  (* transition actions have been checked against restrictions *)
  datatype actionsub = SelectSub of symbol | FreeExprSub of E.expr
  datatype t = ActTrans of {selectids:  (symbol * E.ty) list,
                            actionsubs: actionsub list,
                            guard:      expr,
                            names:      symbolset}

  (* utility functions for processing names *)
  fun toBoundId (s, ty) = E.BoundId (s, ty, E.nopos)

  val addBoundIds = let
      fun add (E.BoundId (n, _, _), s) = s <+ n
    in foldl add emptyset end

  fun fromBoundId (E.BoundId (n, ty, _)) = (n, ty)

  fun addNameTypes xs = let
      fun add ((n, _), s) = s <+ n
    in foldl add emptyset xs end

  fun addSelectSubNames act = let
      fun add (SelectSub n, s) = s <+ n
        | add (FreeExprSub _, s) = s
    in foldl add emptyset act end

  fun addFreeExprNames act = let
      fun add (SelectSub _, s)   = s
        | add (FreeExprSub e, s) = s ++ E.getFreeNames e
    in foldl add emptyset act end

  fun addActionNames act = let
      fun add (SelectSub n, s)   = s <+ n
        | add (FreeExprSub e, s) = s ++ E.getFreeNames e
    in foldl add emptyset act end

  fun actionsubToExpr (SelectSub n)   = E.VarExpr (E.SimpleVar (n, E.nopos))
    | actionsubToExpr (FreeExprSub e) = e

  local (*{{{1*)
    fun showCheckedActions []      = ""
      | showCheckedActions (SelectSub s::es)   = "[#" ^ Atom.toString s ^ "#]" ^
                                                 showCheckedActions es
      | showCheckedActions (FreeExprSub e::es) = "[" ^ ECVT.Expr.toString e ^ "]" ^
                                                 showCheckedActions es

    fun showActions []      = ""
      | showActions (e::es) = "[" ^ ECVT.Expr.toString e ^ "]" ^ showActions es
  in (*}}}1*)
  fun symtyToStr (s, ty) = Atom.toString s ^ " : " ^ ECVT.Ty.toString ty
  val symtyList = (ListFormat.fmt {init="(", sep=", ",
                                   final=")", fmt=symtyToStr})

  fun toString (ActTrans {selectids, actionsubs, guard, names}) = let in
      "{selectids: " ^ symtyList selectids           ^ "\n" ^
      " guard: "     ^ ECVT.Expr.toString guard      ^ "\n" ^
      " action: "    ^ showCheckedActions actionsubs ^ "\n" ^
      " names: "     ^ AtomSet.foldl
                       (fn (s, str)=> str ^ " " ^ Atom.toString s) "" names ^
      " }\n"
    end
  end (* local *)

  fun fromTrans subtypes {selectids, actionsubs, guard} =
    let
      (*{{{1*)
      fun addBound (E.BoundId (n, ty, _), m) = AtomMap.insert (m, n, ty)
      val sids = foldl addBound AtomMap.empty selectids

      fun checksubs ([], [], _) = []

        | checksubs (ty::tys, (act as E.VarExpr (E.SimpleVar (n, _)))::acts,
                     usedsels) = let
              fun already nq = n =:= nq
            in
              case AtomMap.find (sids, n)
              of NONE       => FreeExprSub act
                               :: checksubs (tys, acts, usedsels)
               | SOME (ty') => if E.tyequal (ty, ty')
                               then if List.exists already usedsels
                                    then raise ActionSubWithDuplicate n
                                    else SelectSub n
                                         :: checksubs (tys, acts, n::usedsels)
                               else raise ActSubWithBadType
                                          {expr=act, badty=ty', goodty=ty}
            end

        | checksubs (_::tys, act::acts, usedsels) = let
              val names = E.getFreeNames act
              fun selectId n = not (isSome (AtomMap.find (sids, n)))
            in
              if AtomSet.exists selectId names
              then raise ActSubWithNonSimpleSelect act
              else FreeExprSub act :: checksubs (tys, acts, usedsels)
            end

        | checksubs _ = raise BadSubscriptCount

      val asubs = checksubs (subtypes, actionsubs, [])
      (*}}}1*)
    in
      ActTrans {selectids=map fromBoundId selectids,
                actionsubs=asubs,
                guard=guard,
                names=addBoundIds selectids
                      ++ addFreeExprNames asubs
                      ++ E.getFreeNames guard}
    end

  fun toTrans (ActTrans {selectids, actionsubs, guard, ...}) =
      {selectids=map toBoundId selectids,
       actionsubs=map actionsubToExpr actionsubs,
       guard=guard}

  fun joinTrans
      (ActTrans {selectids=sids1, actionsubs=act1, guard=g1, names=n1},
       ActTrans {selectids=sids2, guard=g2, names=n2, ...}) = let

      fun addType first ((n, ty), m) = let
          val (ty1, ty2) = case AtomMap.find (m, n) of
                             NONE       => (NONE, NONE)
                           | SOME types => types

          val newtypes = if first then (SOME ty, ty2)
                                  else (ty1    , SOME ty)
        in AtomMap.insert (m, n, newtypes) end

      val tymap1 = List.foldl (addType true)  AtomMap.empty sids1
      val tymap  = List.foldl (addType false) AtomMap.empty sids2

      fun keepSidUnchanged n = case AtomMap.find (tymap, n) of
                             SOME (SOME ty1, SOME ty2) => E.tyequal (ty1, ty2)
                           | _                         => true
        (* only change a sid name when it is used in both transitions but with
         * different types. Note that sids used in select subs should have the
         * same name and type (see first two lines of ensureConsistency). *)

      fun filterOutDups dn = List.filter (fn (n, _)=> not (n =:= dn))

      fun mergesids ([], newsids, g, used) = (newsids, g, used)
        | mergesids ((s as (n, ty))::ss, newsids, g, used) =
            if keepSidUnchanged n
            then mergesids (filterOutDups n ss,
                            s::newsids, g, used)
            else let val n' = getNewName (n, used)
                 in mergesids (ss, (n', ty)::s::(filterOutDups n newsids),
                               E.renameVar ({old=n, new=n'}, g),
                               used <+ n')
                 end

      val (sids', g2', names) = mergesids (List.revAppend (sids1, rev sids2),
                                           [], g2, n1++n2)
        (* Reverse sids1 and sids2 because we filter out duplicates in
           mergesids, and the rightmost bound variable masks to the left *)
    in
      ActTrans {selectids=sids',
                actionsubs=act1,
                guard=E.BinBoolExpr {left=g1,bop=E.OrOp,right=g2',pos=E.nopos},
                names=names}
    end

  fun groupLikeActions selids
        (tr1 as ActTrans {actionsubs=act1, names=names1, ...},
         tr2 as ActTrans {actionsubs=act2, names=names2, ...}) =
    let
      fun compare (tr1, tr2, [], []) = SOME (joinTrans (tr1, tr2))

        | compare (tr1, tr2, SelectSub n1::acts1, SelectSub n2::acts2) =
            if n1 =:= n2
            then compare (tr1, tr2, acts1, acts2)
            else raise Fail "renameSelectIds must be called first!"

        | compare (tr1, tr2, FreeExprSub e1::acts1, FreeExprSub e2::acts2) =
            if E.equal (e1, e2) then compare (tr1, tr2, acts1, acts2)
                                else NONE

        | compare (_, _, a1::_, a2::_) =
            raise MixedSubscriptTypes (actionsubToExpr a1, actionsubToExpr a2)

    in compare (tr1, tr2, act1, act2) end

  fun newTrans (subtypes, conflictset) = let
      val basename = Atom.atom "s"

      fun addRsubs ([], _, ActTrans {selectids, actionsubs, guard, names}) =
                    ActTrans {selectids=rev selectids,
                              actionsubs=rev actionsubs,
                              guard=guard, names=names}
                    (* reverse here, rather than: addRsubs (rev subtypes, ...
                       below, so that selectid names increase to the right. *)

        | addRsubs (t::ts, confl,
                    ActTrans {selectids, actionsubs, guard, names}) =
            let
              val sid = getNewName (basename, confl)
              val trans = ActTrans {selectids=(sid, t)::selectids,
                                    actionsubs=SelectSub sid::actionsubs,
                                    guard=guard,
                                    names=names <+ sid}
            in addRsubs(ts, confl <+ sid, trans) end
    in
      addRsubs (subtypes, conflictset,
                ActTrans {selectids=[], actionsubs=[],
                          guard=E.trueExpr, names=emptyset})
    end

  fun coverMissingChannels (subtypes, atrans : t list, invariant) = let
      fun isSelectSub (SelectSub _) = true
        | isSelectSub _             = false

      fun toVar s = E.VarExpr (E.SimpleVar (s, E.nopos))

      fun getActionSubs (ActTrans {actionsubs, ...}) = actionsubs

      fun distinguish (_,    SelectSub _,    e) = e
        | distinguish (svar, FreeExprSub fv, e) = let
              val ne = E.RelExpr {left=svar, rel=E.NeOp, right=fv, pos=E.nopos}
            in E.orexpr (e, ne) end

      fun distinguishTrans (selectids : (symbol * E.ty) list, ats) = let
          val selectvars = map (toVar o #1) selectids

          fun checkall (ActTrans {actionsubs, ...}, e) = let
              val ne = ListPair.foldl distinguish
                                      E.falseExpr (selectvars, actionsubs)
            in E.andexpr (e, ne) end

        in foldl checkall E.trueExpr ats end

      val conflicts = foldl (fn (ActTrans a, s)=> s++(#names a))
                                                  emptyset atrans
      val ActTrans {selectids, actionsubs, names, ...} = newTrans (subtypes,
                                                                   conflicts)
      val guard=distinguishTrans (selectids, atrans)
    in
      if E.equal (guard, E.falseExpr)
      then []
      else [ActTrans {selectids=selectids,
                      actionsubs=actionsubs,
                      guard=E.andexpr (guard, invariant), names=names}]
    end
  
  fun chooseSelectIds [] = []
    | chooseSelectIds (atrans as ActTrans {actionsubs, ...}::_) = let
      fun getNames (ActTrans {names, ...}, s) = s ++ names
      val usednames = foldl getNames emptyset atrans

      fun selectNames ([], _) = []
        | selectNames (SelectSub s::xs, used) = let
                                          val n = getNewName (s, used)
                                        in n::selectNames (xs, used <+ n) end
        | selectNames (_::xs, used) = selectNames (xs, used)

    in selectNames (actionsubs, usednames) end

  (* assumes that identifiers in selids and those in names are disjoint *)
  fun renameSelectIds selids (ActTrans {selectids, actionsubs,
                                        guard, names}) =
    let
      val newsids = foldl (fn (i, s)=>AtomSet.add (s, i)) emptyset selids

      fun renExpr (old, new, e) = E.renameVar ({old=old, new=new}, e)
      fun renId m (sid as (s, ty)) = (case AtomMap.find (m, s) of
                                        NONE    => sid
                                      | SOME s' => (s', ty))

      fun newIds ([], _, (rs, m))              = (rev rs, m)
        | newIds (SelectSub s::xs, n::ns, (rs, m)) =
            newIds (xs, ns, (SelectSub n::rs, AtomMap.insert (m, s, n)))
        | newIds (x::xs, ns, (rs, m))          = newIds (xs, ns, (x::rs, m))

      val (actsubs, selMap) = newIds (actionsubs, selids, ([], AtomMap.empty))

    in
      ActTrans {selectids=map (renId selMap) selectids,
                actionsubs=actsubs,
                guard=AtomMap.foldli renExpr guard selMap,
                names=names ++ newsids}
    end

  fun foldThrough f xs = let
      fun fthrough ([], result)    = rev result
        | fthrough (x::xs, result) = let
              fun foldInto (y, [], result)       = (y, result)
                | foldInto (y, z::zs, result) = let in
                      case f (y, z)
                      of NONE    => foldInto (y, zs, z::result)
                       | SOME y' => foldInto (y', zs, result)
                    end

              val (x', xs') = foldInto (x, xs, [])
            in
              fthrough (xs', x'::result)
            end
    in fthrough (xs, []) end

  fun ensureConsistency trans = let
      val selids = chooseSelectIds trans
      val trans = map (renameSelectIds selids) trans
    in foldThrough (groupLikeActions selids) trans end

end
end

