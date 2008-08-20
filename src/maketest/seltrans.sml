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

structure SelTrans :> SEL_TRANS = let
  structure E    = Expression
        and ECVT = ExpressionCvt
        and Env  = Environment
in struct
  type expr      = Expression.expr
   and ty        = Expression.ty
   and symbol    = Atom.atom
   and symbolset = AtomSet.set

  exception NonSimpleSelectIndex of expr
  exception BadChannelIndex of {id: symbol, badty: ty, goodty: ty}
  exception BadSubscriptCount

  infix <+ <- ++ <\ \ =:= ; open Symbol
  
  datatype t = SelTrans of {selectids:  (symbol * ty) list,
                            guard:      expr,
                            names:      symbolset}

  (* utility functions for processing names *)
  fun toBoundId (s, ty) = E.BoundId (s, ty)
  fun fromBoundId (E.BoundId (n, ty)) = (n, ty)

  fun addNameTypes xs = let
      fun add ((n, _), s) = s <+ n
    in foldl add emptyset xs end

  fun addBoundId (E.BoundId (n, _), s) = s <+ n
  fun addExprIds (e, s) = s ++ (E.getFreeNames e)

  fun tnames ({selectids, actionsubs, guard}, (sels, gs)) =
    (foldl addBoundId sels selectids,
     foldl addExprIds (addExprIds (guard, gs)) actionsubs)

  local
  (*{{{1*)
    fun selActionSubs {selectids, actionsubs, guard} = actionsubs

    fun transpose isSelBinding = let
        fun cons (e, gs) = case isSelBinding e of
                             NONE   => gs
                           | SOME s => s::gs
        fun f ([], gss)        = gss
          | f (e::es, [])      = cons (e, [])::f (es, [])
          | f (e::es, gs::gss) = cons (e, gs)::f (es, gss)
      in f end

    fun selNames ([n], (xs, used))   = if n <- used
                                       then let val n' = getNewName (n, used)
                                            in (n'::xs, used <+ n') end
                                       else (n::xs, used <+ n)
      | selNames (n::ns, (xs, used)) = if n <- used
                                       then selNames (ns, (xs, used))
                                       else (n::xs, used <+ n)
      | selNames ([], _) = raise Fail "Fault in SelTrans.makeIndexNames"
  (*}}}1*)
  in
  fun makeIndexNames n ts = let
      val (selNms, gs) = foldl tnames (emptyset, emptyset) ts
      val gNms = gs \ selNms

      fun isSel (E.VarExpr (E.SimpleVar s)) = if s <- selNms
                                              then SOME s else NONE
        | isSel _                           = NONE

      val defaults = List.tabulate (n, fn i => [`("s" ^ Int.toString i)])
      val potNames = List.take (foldl (transpose isSel)
                                      defaults
                                      (map selActionSubs ts), n)
    in List.rev (#1 (foldl selNames ([], gNms) potNames)) end
  end (* local *)

  local (*{{{1*)
    fun symtyToStr (s, ty) = Atom.toString s ^ " : " ^ ECVT.Ty.toString ty
    val symtyList = (ListFormat.fmt {init="(", sep=", ",
                                     final=")", fmt=symtyToStr})
  in (*}}}1*)
  fun toString actidx (SelTrans {selectids, guard, names}) = let in
      "{selectids: " ^ symtyList selectids           ^ "\n" ^
      " guard: "     ^ ECVT.Expr.toString guard      ^ "\n" ^
      " actidx: "    ^ symtyList actidx              ^ "\n" ^
      " names: "     ^ AtomSet.foldl
                       (fn (s, str)=> str ^ " " ^ Atom.toString s) "" names ^
      " }\n"
    end
  end (* local *)

  local
    (*{{{1*)
    fun addBoundTyMap (E.BoundId (n, ty), m) = AtomMap.insert (m, n, ty)

    fun names ({selectids, actionsubs, guard}, (sels, gs)) =
      (foldl addBoundId sels selectids,
       foldl addExprIds (addExprIds (guard, gs)) actionsubs)

    fun equate (l, r) = E.RelExpr {left=l, rel=E.EqOp, right=r}

    fun limit (nm, rel, limit) = E.RelExpr {left=E.VarExpr (E.SimpleVar nm),
                                            rel=rel, right=limit}

    (* matchSelRange (selid, newRange, originalRange) *)
    fun matchSelRange (_, E.INT(NONE, E.NoQual),
                          E.INT(NONE, E.NoQual)) = SOME NONE
      | matchSelRange (nm, E.INT (SOME (lRan, uRan), E.NoQual),
                           E.INT (SOME (lSel, uSel), E.NoQual)) =
            SOME (case (E.equal (lSel, lRan), E.equal (uSel, uRan)) of
                (true, true)   => NONE
              | (true, false)  => SOME (limit (nm, E.LeOp, uSel))
              | (false, true)  => SOME (limit (nm, E.GeOp, lSel))
              | (false, false) => SOME (E.BinBoolExpr{bop=E.AndOp,
                   left=limit (nm,E.GeOp,lSel),right=limit (nm,E.LeOp,uSel)}))

      | matchSelRange (nm, E.NAME (_, E.NoQual, SOME tyex1),
                           E.NAME (_, E.NoQual, SOME tyex2))
                        = matchSelRange (nm, tyex1, tyex2)
      | matchSelRange (nm, ty1, E.NAME (_, E.NoQual, SOME tyex2))
                        = matchSelRange (nm, ty1, tyex2)
      | matchSelRange (nm, E.NAME (_, E.NoQual, SOME tyex1), ty2)
                        = matchSelRange (nm, tyex1, ty2)

      | matchSelRange (_, E.NAME (s1, E.NoQual, _), E.NAME (s2, E.NoQual, _)) =
                      if s1 =:= s2 then SOME NONE else NONE

      | matchSelRange (_, ty1, ty2) = if E.tyequal (ty1, ty2)
                                      then SOME NONE else NONE

    (* addLimitClause (selid, newRange, originalRange, guard) *)
    fun addLimitClause (n, ty, ty', g) =
        case matchSelRange (n, ty, ty') of
          SOME NONE     => g
        | SOME (SOME c) => E.andexpr (g, c)
        | NONE => raise BadChannelIndex {id=n, badty=ty', goodty=ty}

    (* Assumption:
     * Each of the newidx values will have a different prefix (e.g. s0, s1,...)
     * and the getNewName function returns a new name based on the given prefix
     * so there will never be a clash even if we don't update the set of used
     * names. *)
    fun makeMap isSelBinding used = let
        fun f (((newidx, _), e), map) = let
            val map' = AtomMap.insert (map, newidx, getNewName (newidx, used))
          in
            case isSelBinding e of
              NONE   => map'
            | SOME s => if s =:= newidx then map'
                        else AtomMap.insert (map, s, newidx)
          end
      in f end
    
    fun zip (x::xs, y::ys) = (x, y):: zip (xs, ys)
      | zip ([], [])       = []
      | zip _              = raise BadSubscriptCount

    fun toVar s = E.VarExpr (E.SimpleVar s)

    (*}}}1*)
  in
  fun fromTrans actids (ts as {selectids, actionsubs, guard}) = let
      val idxSeqs = zip (actids, actionsubs)
      val (selNms, gs) = tnames (ts, (addNameTypes actids, emptyset))

      val sidTypes = foldl addBoundTyMap AtomMap.empty selectids

      fun isSel (E.VarExpr (E.SimpleVar s)) = if s <- selNms
                                              then SOME s else NONE
        | isSel _                           = NONE

      val renMap = foldl (makeMap isSel (selNms ++ gs)) AtomMap.empty idxSeqs

      fun changeIdxs (((newidx, newty), e), (selids, g, prevUse)) =
        let
          fun varIsSel (env, E.VarExpr (E.SimpleVar s)) =
              (case Env.findValScope env s of
                 NONE            => s <- selNms
               | SOME BoundScope => false)
            | varIsSel _ = false

          (*fun containsSel e = null (Env.filter varIsSel Env.base_env e)*)
          fun containsSel e = not (null (Env.filter varIsSel Env.base_env e))
        in
          case isSel e of
          (* for a selectid i
           *    -if the type doesn't cover the whole dimension then && in a
           *     restriction clause (l <= s_i <= u) where i has the type int[l,u]
           *    -if used in an earlier index, s_j, then && the clause (s_i == s_j)
           *    -if doesn't match the current name then rename (s_i/i)
           *    -remove i from the list of selectids *)
            SOME s => let
                val g'  = addLimitClause (newidx, newty,
                             Option.valOf (AtomMap.find (sidTypes, s)), g)
                val g'' = case AtomMap.find (prevUse, s) of
                            NONE    => g'
                          | SOME s' => let
                                         val c = equate (toVar newidx, toVar s')
                                       in E.andexpr (g', c) end

                fun skip_s (E.BoundId (nm, ty)) = if s =:= nm then NONE
                                                  else SOME (nm, ty)
              in
                (List.filter (fn (nm, _)=> not (s =:= nm)) selids, 
                 E.renameVar ({old=s, new=newidx}, g''),
                 AtomMap.insert (prevUse, s, newidx))
              end

          (*  for a state expression e:
           *    -throw an exception if it contains selectids
           *    -&& the clause (si == R'(e)) to the guard *)
          | NONE   => if containsSel e then raise NonSimpleSelectIndex e
                      else let
                             val c= equate (toVar newidx, E.renameVars renMap e)
                           in (selids, E.andexpr (g, c), prevUse) end
        end (* changeIdxs *)

      val (selids, g, _) = foldl changeIdxs
                             (map fromBoundId selectids, guard, AtomMap.empty)
                             idxSeqs
    in
      SelTrans
        {selectids=selids,
         guard=g,
         names=(foldl (fn ((n, _), s) => s <+ n) (E.getFreeNames g) selids)}
    end
  end (* local *)


  fun toTrans actids (SelTrans {selectids, guard, ...}) =
      {selectids=map toBoundId selectids @ map toBoundId actids,
       actionsubs=map (fn (nm,_)=> E.VarExpr (E.SimpleVar nm)) actids,
       guard=guard}

  local
    (*{{{1*)
    fun merge (SelTrans {selectids=sids1, guard=g1, names=names1},
               SelTrans {selectids=sids2, guard=g2, names=names2}) =
      let
        fun f ((s as (nm, ty)), (selects, rename, used)) =
            if nm <- names2
            then let val nm' = getNewName (nm, used)
                 in
                   ((nm', ty)::selects,
                    AtomMap.insert (rename, nm, nm'),
                    used <+ nm')
                 end
            else (s::selects, rename, used)

        val (selects, rename, used) =
              foldl f ([], AtomMap.empty, names1 ++ names2) sids1
      in
        SelTrans {selectids=List.revAppend (selects, sids2),
                  guard=E.orexpr (E.renameVars rename g1, g2),
                  names=used}
      end
    (*}}}1*)
  in
  fun mergeTrans []           = SelTrans {selectids=[],
                                          guard=E.falseExpr,
                                          names=emptyset}
    | mergeTrans [x]          = x
    | mergeTrans (x1::x2::xs) = mergeTrans (merge (x1, x2)::xs)
  end (* local *)

  fun reduceSelectIds env (at as SelTrans {selectids,guard,names}) =
    let
      val _ = Util.debugVeryDetailed (fn()=> ["* reduceSelectIds:before=\n",
                                              toString [] at])

      val senv = List.foldl (Env.addId Env.SelectScope) env selectids
      fun clocksInExpr expr = not (Env.containsClocks senv expr)
 
      fun f (s as (id, ty), (sids, expr, names)) =
          case E.shrinkScope ((id, ty, false), clocksInExpr) expr of
            NONE             => (s::sids, expr, names)
          | SOME reboundExpr => (sids, reboundExpr, names <\ id)

      val (selectids', guard', names') = foldl f ([], guard, names)
                                                 (rev selectids)
          (* reverse the selectids to handle masking of identical names,
           * folding f has the effect of reversing them back.           *)

      val at' = SelTrans {selectids=selectids', guard=guard', names=names'}
      val _ = Util.debugVeryDetailed
                (fn()=> ["* reduceSelectIds:after =\n", toString [] at'])
    in at' end

end
end

