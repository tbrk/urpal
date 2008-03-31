(* $Id$ *)

(* TODO:
 *  * Tidy this whole file up. Try to effect more structure and reuse.
 *    Shift generic functions into xml/nta parse/parsed-nta.
 *)

local structure P    = ParsedNta
            and E    = UppaalParse.Expression
            and ECvt = UppaalParse.ExpressionCvt
            and Env  = UppaalParse.Environment
            and TF   = TransitionFlipper
in

structure MakeTest : MAKE_TEST
=
struct
  (* shortcuts over Atom and AtomSet *)
  infix <+ <- ++ <\ \ =:= ; open Symbol

  exception InvalidChannelId of symbol
  exception SilentTransition 
  exception NoChannels
  exception BadSubscriptType of string * string
                        (* channel name, subscript type *)


  fun validChannelId env id = let
      fun isChannel (E.CHANNEL _)     = true
        | isChannel (E.ARRAY (ty, _)) = isChannel ty
        | isChannel _                 = false

    in case Env.findValType env id
         of SOME ty => isChannel ty
          | NONE    => false
    end

  fun makeTransition (src as P.LocId s, dst as P.LocId d, sids, g, synco) = let
      val _ = Util.debugVeryDetailed (fn()=>["new transition: ",Int.toString s,
                                             "->", Int.toString d, " "])
    in
          P.Transition {id=NONE, source=src, target=dst, select=(sids, NONE),
                        guard=(g, NONE), sync=(synco, NONE),
                        update=([], NONE), comments=(NONE, NONE),
                        position=NONE, color=Settings.errorColor(), nails=[]}
    end

  fun locName (P.Location {id=P.LocId l, name=(nameo, _), ...}) =
      case nameo of
         NONE   => ("<id=" ^ Int.toString l ^ ">")
       | SOME n => "'" ^ n ^ "'"

  fun formInvariantTrans env (errloc, locs) = let
      fun fromTrans id {selectids, guard, actionsubs} =
          makeTransition(id, errloc, selectids, guard, NONE)

      fun doLoc (loc as P.Location {id, invariant=(e, _), ...}) =
          if E.equal (e, E.trueExpr)
          then []
          else map (fromTrans id) (TF.negateInvariant env e)
    in List.concat (List.map doLoc locs) end

  fun formInverseTrans (errloc, env, chans) (locs, trans) = let

      fun filterChannels chanId = let
          fun f (P.Transition {sync=(NONE,_), ...},_) = raise SilentTransition
            | f (t as P.Transition {sync=(SOME (c,d,acts),_),
                                    select=(sids, _),
                                    guard=(guard, _), ...}, r as (is, os)) =
              if c =:= chanId
              then let val tr = {selectids=sids, actionsubs=acts, guard=guard}
                   in case d
                      of E.Input  => (tr::is, os)
                       | E.Output => (is, tr::os)
                   end
              else r
        in foldl f ([], []) end

      fun flipFailed (loc, chanId, errMsg) = (
          Util.warn  [errMsg];
          Util.abort ["while processing transitions on channel '",
                      Atom.toString chanId, "' from location ", locName loc]
        )

      fun chanIsUrgent chan = let
          fun f (E.CHANNEL {urgent=true, ...}) = true
            | f (E.ARRAY (inner, E.Type s))    = f inner
            | f (E.NAME (_, _, SOME ty))       = f ty
            | f _                              = false
        in case (Env.findValType env chan) of
             NONE    => false   (* no type found, assume the best *)
           | SOME ty => f ty
        end

      fun doLoc (location as P.Location {id=loc as P.LocId l,
                                         invariant=(inv, _), ...}) = let

          val _ =Util.debugIndent (Settings.Detailed,
                                fn()=>["processing location:", Int.toString l])
          val invHasClocks = Env.containsClocks env inv

          fun makeTrans (chanId, dir) {selectids, actionsubs, guard} =
              if E.equal (guard, E.falseExpr)
              then NONE
              else let
                     val sync = SOME (chanId,  dir, actionsubs)
                   in
                     SOME (makeTransition (loc,errloc,selectids,guard,sync))
                   end

          fun flipChannel trans chanId = let
              val _ =Util.debugIndent(Settings.Detailed,
                                      fn()=>["chanId:",Atom.toString chanId])
              val acttys = valOf (TF.chanToSubRanges (env, chanId))
              fun negate trs = (TF.negateTransitions env (acttys, trs, inv))

              val (is, os) = filterChannels chanId trans

              val flipped =
                    (List.mapPartial (makeTrans (chanId, E.Output))(negate is))
                  @ (List.mapPartial (makeTrans (chanId, E.Input)) (negate os))

              val _ = if not (null flipped)
                         andalso invHasClocks
                         andalso chanIsUrgent chanId
                      then Util.warn ["urgent channel '", Atom.toString chanId,
                                      "' from location ", locName location,
                                      " with an invariant containing clocks"]
                      else ()

              val _ = Util.debugOutdent (Settings.Detailed, fn()=>[])
            in flipped end
            handle TF.FlipFailed s => flipFailed (location, chanId, s)

          fun startsHere (P.Transition {source=P.LocId s, ...}) = (s = l)
                (* ...it sure does *)
          val ts = List.filter startsHere trans
        in
          List.concat (map (flipChannel ts) chans)
          before (Util.debugOutdent (Settings.Detailed, fn()=>[]))
        end

    in List.concat (map doLoc locs) end
  
  fun channelSet transitions = let
      fun addSync (P.Transition {sync=(NONE, _), ...}          , s) = s
        | addSync (P.Transition {sync=(SOME (c, _, _), _), ...}, s) = s <+ c
    in foldl addSync AtomSet.empty transitions end

  fun channels (P.Template {transitions, ...})
          = AtomSet.listItems (channelSet transitions)

  fun invertActionAndAddInvariant (env, invmap, dontFlipSet)
      (P.Transition {id, source=source as P.LocId src, target,
                     guard=(g, gpos), sync=(sync, syncpos),
                     select=select as (sel, _), update,
                     comments, position, color, nails}) =
    let
      fun selToEnv (E.BoundId (nm,ty,_), env) = Env.addId Env.SelectScope
                                                          ((nm, ty), env)
      val inv = valOf (IntBinaryMap.find (invmap, src))
      val g' = if E.equal (inv, E.trueExpr)
               then g
               else if E.equal (g, E.trueExpr)
                    then inv
                    else let
                           val senv = List.foldl selToEnv env sel
                           val simplified = TF.andexpr senv (g, inv)
                         in case simplified of
                              (E.BinBoolExpr{bop=E.OrOp,...})=>E.andexpr(inv,g)
                            | _ => simplified
                         end
                         (* Not so tidy...
                          * TF.andexpr runs the whole shebang through
                          * fromDNF (toDNF _) which has the advantage of
                          * simplifying some invariant and guard
                          * combinations, but if the result involves
                          * ORs and clock variables, it may have to be split
                          * across multiple transitions, which we would rather
                          * avoid by using E.andexpr. *)

      val sync' = case sync of
                    NONE             => NONE
                  | SOME (s,dir,idx) => if s <- dontFlipSet then sync
                                        else SOME (s,E.otherDirection dir,idx)
    in
      P.Transition {id=id, source=source, target=target,
                    guard=(g', gpos), sync=(sync', syncpos), 
                    select=select, update=update, comments=comments,
                    position=position, color=color, nails=nails}
    end

  fun stripInvariant (l as P.Location {id, position, color, name,
                                       invariant=(inv, invpos), comments,
                                       urgent, committed}) =
    if E.equal(inv, E.trueExpr)
    then l
    else P.Location {id=id, position=position, color=color, name=name,
                     invariant=(E.trueExpr, NONE), comments=comments,
                     urgent=urgent, committed=committed}

  fun maketest (channelIds, t as P.Template {name=(nm, _), declaration,
                                             locations, transitions, ...}) =
    let
      val _ = Util.debugVeryDetailed (fn()=>["maketest: start '", nm, "'"])

      val _ = if null channelIds then raise NoChannels else ()

      val _ = case List.find (fn c=>not (validChannelId declaration c))
                  channelIds of
                 NONE => ()
               | SOME c=> raise InvalidChannelId c

      val cIdGiven = foldl AtomSet.add' AtomSet.empty channelIds
      val cIdUsed  = channelSet transitions
      val cMissing = AtomSet.difference (cIdUsed, cIdGiven)
      val _ = if AtomSet.isEmpty cMissing then ()
              else Util.warn ("channels are missing from the testing list: "::
                     [ListFormat.fmt {init="", sep=", ", final="",
                              fmt=Atom.toString} (AtomSet.listItems cMissing)])
      val (tplate',err as P.Location {id=errLocId,...})= P.Location.new t "Err"
      val err = P.Location.updColor err (Settings.errorColor ())

      fun keyInv (P.Location {id=P.LocId i,invariant=(inv, _), ...}) = (i,inv)
      fun insertInv (l, m) = IntBinaryMap.insert' (keyInv l, m)
      val invMap = foldl insertInv IntBinaryMap.empty locations

      val _ = Util.debugVeryDetailed (fn()=>["maketest: formInvariantTrans..."])
      val invarianttrans = formInvariantTrans declaration (errLocId, locations)

      val _ = Util.debugVeryDetailed (fn()=>["maketest: formInverseTrans..."])
      val fliptrans = formInverseTrans (errLocId, declaration, channelIds)
                                       (locations, transitions)

      val _ = Util.debugVeryDetailed
                (fn()=>["maketest: invertActionAndAddInvariant..."])
      val normtrans = map (invertActionAndAddInvariant
                           (declaration, invMap, cMissing)) transitions
      
      (* Not quite per the Stoelinga definition. *)
      val errorloop = [P.Transition {id=NONE, source=errLocId, target=errLocId,
                                     select=([],NONE), guard=(E.trueExpr,NONE),
                                     sync=(NONE,NONE), update=([],NONE),
                                     comments=(NONE,NONE),   position=NONE,
                                     color=Settings.errorColor(), nails=[]}]

      val _ = Util.debugVeryDetailed (fn()=>["maketest: finished '", nm, "'"])
    in
      (errLocId, P.Location.map stripInvariant
                   (P.Template.updTransitions tplate'
                      (List.concat [invarianttrans, fliptrans,
                                    normtrans, errorloop])))
    end

  (********** Jensen, Larsen, Skou extensions: ******************************)
  (* This section was written very quickly. It would probably benefit from
   * restructuring, refactoring, and more thought... *)

  fun makeComparison map = let 
    (* makeComparison env map expr
     *
     * Return an expression, for all v_i in domain(map):
     *    /\  (v_i == map(v_i))
     *      i
     *
     * This is the negation of the expression used in JLS's expansion because
     * we add it as an invariant before `flipping'.
     *)
      fun doPair (v, v', e) = E.orexpr (e,
          E.RelExpr {left= E.VarExpr (E.SimpleVar (v, E.nopos)),
                     rel=E.NeOp,
                     right=E.VarExpr (E.SimpleVar (v', E.nopos)),
                     pos=E.nopos})

    in AtomMap.foldli doPair E.falseExpr map end

  fun updateEffects subst
        (P.Transition {id, source, target, select=(sels, selP),
                       guard=(g, gP), sync=(syn, synP), update=(upd, updP),
                       comments, position, color, nails}) =
    let
      fun drop (E.BoundId (nm, _, _), m) = #1 (AtomMap.remove (m, nm))
                                           handle LibBase.NotFound => m
      val doSubst = E.renameVars (foldl drop subst sels)
      fun doSync (nm, dir, subs) = (nm, dir, map doSubst subs)
    in
      P.Transition {id=id,
                    source=source,
                    target=target,
                    select=(sels, selP),
                    guard=(doSubst g, gP),
                    sync=(Option.map doSync syn, synP),
                    update=(map doSubst upd, updP),
                    comments=comments,
                    position=position,
                    color=color,
                    nails=nails}
    end

  fun substituteInvariant subst
     (P.Location {id, position, color, name, invariant=(inv, invP),
                  comments, urgent, committed}) =
      P.Location {id=id, position=position, color=color, name=name,
                  invariant=(E.renameVars subst inv, invP),
                  comments=comments, urgent=urgent, committed=committed}

  fun makeCheckTrans (guard, locs) dstLoc = let
      fun f (P.Location {id, ...}) = P.Transition {id=NONE,
                                       source=id, target=dstLoc,
                                       select=([], NONE), guard=(guard, NONE),
                                       sync=(NONE, NONE), update=([], NONE),
                                       comments=(NONE,NONE), position=NONE,
                                       color=Settings.errorColor(), nails=[]}
    in map f locs end

  fun makeVarCheckTrans vars (t as P.Template {name, parameter, initial,
                        declaration=decl, locations=locs, transitions=trans}) =
    let
      val (dupmap, decl') = Env.dupVariables vars (decl,SOME Env.TemplateScope)
    in
      if AtomMap.isEmpty dupmap then (t, fn _ =>[])
      else (P.Template {name=name, parameter=parameter, declaration=decl',
                        initial=initial,
                        locations=map (substituteInvariant dupmap) locs,
                        transitions=map (updateEffects dupmap) trans},
            makeCheckTrans (makeComparison dupmap, locs))
    end

  local
    exception BadSub of string
    
    val maxIntRange = (~32768, 32767)
    datatype index_var = ScalarIdx of symbol * E.unique
                       | IntIdx    of int * int
                       | IntExp    of E.expr * E.expr

    fun usable (ScalarIdx (_, q1), ScalarIdx (_, q2)) = (q1=q2)
      | usable (IntIdx (l1,u1), IntIdx (l2,u2))       = u1 < u2 andalso l1 > l2
      | usable (IntExp (l1,u1), IntExp (l2,u2))       = E.equal (l1,l2) andalso
                                                        E.equal (u1,u2)
      | usable _                                      = false

    fun typeToIndex (E.INT (NONE, _))        = IntIdx maxIntRange
      | typeToIndex (E.INT (SOME (E.IntCExpr l,E.IntCExpr u),_)) = IntIdx (l,u)
      | typeToIndex (E.INT (SOME (l, u), _)) = IntExp (l, u)
      | typeToIndex (E.NAME (nm, _, SOME (E.SCALAR (_, _, uniq))))
                                             = ScalarIdx (nm, uniq)
      | typeToIndex ty = raise BadSub (ECvt.Ty.toString ty)
      (* All scalars must be given as typedef-ed names (otherwise they
       * can't be redeclared). *)

    (* Could these be meta variables? *)
    fun indexToType (ScalarIdx (nm, uniq))    = E.NAME (nm, E.NoQual, NONE)
      | indexToType (IntIdx (l, u)) = E.INT (if (l,u)=maxIntRange then NONE
                                             else SOME (E.IntCExpr l,
                                                        E.IntCExpr u),E.NoQual)
      | indexToType (IntExp bounds) = E.INT (SOME bounds, E.NoQual)

    (* try to find variable in pool capable of covering reqty,
     * otherwise add a new one to vars/usednames. *)
    fun findVar (vars, pool, usednames, reqty) = let
        fun f (unused, []) = let val n = getNewName (Atom.atom "i",usednames)
                             in (n, (n,reqty)::vars, unused, usednames<+ n) end
          | f (unused, (n, ty)::vs) = if usable (ty, reqty)
                                      then (n, vars, unused @ vs, usednames)
                                      else f ((n,ty)::unused, vs)
          (* Ideally the narrowest such match would be sought... *)
      in f ([], pool) end

    (* try to find variables in pool capabale of covering all of
     * the given subscript types, adding them if necessary *)
    fun allocVars (vars,pool,usednames,subs,[]) = (vars, usednames, rev subs)
      | allocVars (vars,pool,usednames,subs,s::ss) = let
            val (nm,vars',pool',usednames') = findVar (vars,pool,
                                                       usednames,typeToIndex s)
          in allocVars (vars',pool',usednames',nm::subs,ss) end

    (* map a list of name/subscript-type pairs to a list of
     * name/index-variable pairs, updating the environment as appropriate *)
    fun allocateIndexVariables (env, chansAndTypes, selectids) = let
        fun f (vars, [], usednames, done) = (vars, done)
          | f (vars, (nm, subtys)::chans, usednames, done) = let
                val (vars', usednames', idxvars) = allocVars (vars,vars,
                                                              usednames,[],
                                                              subtys)
              in f (vars', chans, usednames', (nm, idxvars)::done) end
              handle BadSub ty => raise BadSubscriptType (Atom.toString nm, ty)

        fun addId ((nm, ity), env) = Env.addId Env.TemplateScope
                                               ((nm, indexToType ity), env)
        val (idxvars, chanList) = f ([], chansAndTypes,
                                     selectids ++ Env.usedIds env, [])

      in (foldl addId env idxvars, chanList) end

    fun newLoc (id, name, pos) = P.Location {id       =id,
                                             position =pos,
                                             color  =Settings.urgChanLocColor(),
                                             name     =(SOME name, NONE),
                                             invariant=(E.trueExpr, NONE),
                                             comments =(NONE, NONE),
                                             urgent   =false,
                                             committed=false}

    (* For each urgent channel:
     *  -create states l_c_in, l_c_out
     *  -connect them to l_u   (deadend state)
     *  -connect them to error (if a delay occurs)
     *
     * Produce a map from channel set names to:
     *   (input l_u_i, output l_u_i, index variables + subs)
     *)
    fun addUrgentLocs (errId, luId, clkNotZero, nextid, locnames, urgchans) =
      let
        fun f (locs, trans, cmap, nid, locNms, []) = (locs, trans, cmap,
                                                         nid, locNms)
          | f (locs, trans, cmap, nid, locNms, ((nm, subs), dir)::cs) = let

                val suffix = case dir of E.Input  => "_in" | E.Output => "_out"
                                
                val locId = P.LocId nid
                val locNm = getNewName (Atom.atom (String.concat
                                ["l_", Atom.toString nm, suffix]), locNms)
                val loc = newLoc (locId, Atom.toString locNm, NONE)
                
                val sync=(nm, dir,
                          map (fn v=>E.VarExpr(E.SimpleVar (v,E.nopos))) subs)
                val to_lu = P.Transition {id=NONE,
                                          source=locId,
                                          target=luId,
                                          select=([], NONE),
                                          guard=(E.trueExpr, NONE),
                                          sync=(SOME sync, NONE),
                                          update=([], NONE),
                                          comments=(NONE,NONE),
                                          position=NONE,
                                          color=Settings.urgChanLocColor(),
                                          nails=[]}

                val to_err = P.Transition {id=NONE,
                                           source=locId,
                                           target=errId,
                                           select=([], NONE),
                                           guard=(clkNotZero, NONE),
                                           sync=(NONE, NONE),
                                           update=([], NONE),
                                           comments=(NONE, NONE),
                                           position=NONE,
                                           color=Settings.urgChanLocColor(),
                                           nails=[]}

                val {input,output,...} = Option.getOpt (AtomMap.find (cmap,nm),
                                        {input=errId, output=errId, subs=subs})
                val cmap = case dir of
                      E.Input  => AtomMap.insert (cmap, nm,
                                      {input=locId, output=output, subs=subs})
                    | E.Output => AtomMap.insert (cmap, nm,
                                      {input=input, output=locId, subs=subs})
              in
                f (loc::locs,
                   to_lu::to_err::trans,
                   cmap,
                   nid + 1,
                   locNms <+ locNm,
                   cs)
              end
      in f ([], [], AtomMap.empty, nextid, locnames, urgchans) end

    fun makeAssign (var, expr) = E.AssignExpr {
                                    var=E.VarExpr (E.SimpleVar (var, E.nopos)),
                                    aop=E.AssignOp, expr=expr, pos=E.nopos}

    fun remapDestination locMap (t as P.Transition {target=P.LocId dst, id,
                                          source, select, guard, sync, update,
                                          comments, position, color, nails}) =
        case IntBinaryMap.find (locMap, dst) of
          NONE => t
        | SOME dst' => P.Transition {target=P.LocId dst', id=id,
                                     source=source, select=select, guard=guard,
                                     sync=sync, update=update,
                                     comments=comments, position=position,
                                     color=color, nails=nails}
          
    (* Split any location that has outgoing transitions on urgent channels
     * into two parts: l, l_tau. Adding transitions:
     *    l -->  l_tau
     *    l -->* l_u_i
     *)
        (*val (slocs, strans, nextid, locNms) *)
    fun splitLocs (urgmap, nextid, clkReset, locNms, locs, trans) = let

        fun justUrgTransFromLoc id (t as P.Transition{source,sync=(syn,_),...})=
            source=id
            andalso case syn of
                      NONE => false
                    | SOME (nm,_,_) => Option.isSome (AtomMap.find (urgmap,nm))

        fun toUrgent src (P.Transition {sync=(NONE,_),...}) =
                                              raise Fail "toUrgent: bad call"
          | toUrgent src (P.Transition {select=(sel, _), guard=(g, _),
                                        sync=(SOME (c, dir, subs), _), ...}) =
          let
            val {input=inDst, output=outDst, subs=idxVars}
                                            = valOf (AtomMap.find (urgmap, c))
            val cacheIndexes = ListPair.map makeAssign (idxVars, subs)
          in
            UppaalParse.removeUnusedSelectIds
              (P.Transition {id=NONE,
                             source=src,
                             target=case dir of E.Input=>inDst | E.Output=>outDst,
                             select=(sel, NONE),
                             guard=(g, NONE),
                             sync=(NONE, NONE),
                             update=(clkReset::cacheIndexes, NONE),
                             comments=(NONE, NONE),
                             position=NONE,
                             color=Settings.urgChanLocColor(),
                             nails=[]})
          end
        
        fun splitLoc (nextid, locNms, locMap,
                      l as P.Location {id=lId as P.LocId lIdInt,
                                       name=(nm, _), position=lpos, ...}) =
            case List.filter (justUrgTransFromLoc lId) trans of
              [] => (nextid, locNms, locMap, ([l], []))
            | urgtrans => let
                  val prelId = P.LocId nextid
                  val prelNm = getNewName (Atom.atom
                                  (Option.getOpt (nm, "l") ^ "_u"), locNms)

                  val (shift_o, shift_u) = (Settings.splitShiftOld(),
                                            Settings.splitShiftNew())
                  val prel_pos = if isSome shift_o orelse isSome shift_u
                                 then lpos else NONE

                  val prel = newLoc (prelId, Atom.toString prelNm, prel_pos)

                  val tauT = P.Transition {id=NONE,
                                           source=prelId,
                                           target=lId,
                                           select=([], NONE),
                                           guard=(E.trueExpr, NONE),
                                           sync=(NONE, NONE),
                                           update=([], NONE),
                                           comments=(NONE, NONE),
                                           position=NONE,
                                           color=Settings.urgChanLocColor(),
                                           nails=[]}

                  val locMap' = IntBinaryMap.insert (locMap, lIdInt, nextid)
                  val utrans = map (toUrgent prelId) urgtrans

                  val l_sh=P.Location.shift (Settings.splitShiftOld()) l
                  val prel_sh=P.Location.shift(Settings.splitShiftNew()) prel
                in
                  (nextid+1, locNms <+ prelNm,
                   locMap', ([prel_sh,l_sh], tauT::utrans))
                end

        fun f (nextid, locNms, locMap, done, []) = (locMap, done)
          | f (nextid, locNms, locMap, done, l::ls) = let
            val (nextid', locNms', locMap', new) = splitLoc (nextid, locNms,
                                                             locMap, l)
          in f (nextid', locNms', locMap', new::done, ls) end

        fun concatPair (f, s, (rf, rs)) = (f @ rf, s @ rs)

        val (locMap, locs') = f (nextid, locNms, IntBinaryMap.empty, [], locs)
      in
        (locMap,
         ListPair.foldl concatPair ([], map (remapDestination locMap) trans)
                                   (ListPair.unzip locs'))
      end
  in

  fun addReadyChecks channels (errId, t as P.Template{name,parameter, initial,
                                                      declaration=decls,
                                                      locations=locs,
                                                      transitions=trans}) =
    let
      fun checkType (subs, E.CHANNEL{urgent=true, ...}) = SOME (rev subs)
        | checkType (subs, E.ARRAY(inner,E.Type s)) = checkType (s::subs,inner)
        | checkType (subs, E.NAME (_, _, SOME ty))  = checkType (subs, ty)
        | checkType _                               = NONE
      
      fun chanTypeToSubs c = Option.map (fn subs=>(c,subs))
                               (Option.mapPartial (fn t=> checkType ([], t))
                                                  (Env.findValType decls c))

      val (errTrans, nonErrTrans) = List.partition
                        (fn (P.Transition {target,...})=>target=errId) trans

      fun addUsedDirections (nm, subs) = let
          fun justDir (P.Transition {sync=(SOME (nm', dir, _), _),...}) =
                         if nm =:= nm' then SOME dir else NONE
            | justDir _ = NONE
          val dirs = List.mapPartial justDir nonErrTrans
          fun hasDir dir = if List.exists (fn d => d=dir) dirs
                           then [((nm, subs), dir)] else []
        in (hasDir E.Input) @ (hasDir E.Output) end

      fun addSelIds (tr, s) = let
          fun add (E.BoundId (nm, _, _), u) = u <+ nm
        in foldl add s (P.Transition.selSelect tr) end

      val urgchans = List.mapPartial chanTypeToSubs channels
      val sids = foldl addSelIds emptyset trans
      val (decls, urgchanIdxs) = allocateIndexVariables (decls, urgchans, sids)
      val urgchans = List.concat (map addUsedDirections urgchanIdxs)
    in
      if null urgchans then t
      else let
        val locNames = foldl (fn (P.Location{name=(SOME nm,_),...},s)
                                          => s <+ Atom.atom nm
                               | (_, s) => s) emptyset locs
        val (P.LocId nextid) = P.Location.newId t

        val (l_u_id, nextid) = (P.LocId nextid, nextid + 1)
        val l_u_nm = getNewName (Atom.atom "l_u", locNames)
        val l_u = newLoc (l_u_id, Atom.toString l_u_nm, NONE)

        (* Use any other clock, or add a new one *)
        fun findClock (nm, Env.VarEntry {ty=E.CLOCK, ref=false, ...}) = SOME nm
          | findClock _ = NONE
        val (urgclock, decls) = case Env.mapValues findClock decls of
              [] => let val nclk = Env.newId (Atom.atom "x_u", decls)
                    in
                      (nclk, Env.addId Env.TemplateScope((nclk,E.CLOCK),decls))
                    end
            | c::_ => (c, decls)

        val clkNotZero = E.RelExpr {
                            left=E.VarExpr (E.SimpleVar (urgclock, E.nopos)),
                            rel=E.GtOp,
                            right=E.IntCExpr 0, pos=E.nopos}
        val clkReset = E.AssignExpr {
                          var=E.VarExpr (E.SimpleVar (urgclock, E.nopos)),
                          aop=E.AssignOp, expr=E.IntCExpr 0, pos=E.nopos}

        val (ulocs, utrans, urgmap, nextid, locNames) = addUrgentLocs (errId,
                      l_u_id, clkNotZero, nextid, locNames <+ l_u_nm, urgchans)
        
        val (errLoc, nonErrLocs) = List.partition
                                     (fn (P.Location{id,...})=>id=errId) locs
        val (locMap, (slocs, strans)) = splitLocs (urgmap,nextid, clkReset,
                                             locNames, nonErrLocs, nonErrTrans)

        fun shiftInitial (P.LocId i) = P.LocId
                             (Option.getOpt (IntBinaryMap.find (locMap, i), i))
      in
        P.Template {name=name,
                    parameter=parameter,
                    initial=Option.map shiftInitial initial,
                    declaration=decls,
                    locations=l_u::List.concat [ulocs, slocs, errLoc],
                    transitions=List.concat [utrans, strans, errTrans]}
      end
    end
  end (* local *)

end
end (* local *)

