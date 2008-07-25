(* $Id$ *)

(* TODO:
    * Make error handling more descriptive:
        -catch the exception
        -name the template and state
        -show the bad transitions.
    * Tidy this whole file up. Try to effect more structure and reuse.
      Shift generic functions into xml/nta parse/parsed-nta.

 *)

local structure P    = ParsedNta
            and E    = UppaalParse.Expression
            and ECvt = UppaalParse.ExpressionCvt
            and Env  = UppaalParse.Environment
in

structure NtaUtil : NTA_UTIL
=
struct
  (* shortcuts over Atom and AtomSet *)
  infix <+ <- ++ <\ \ =:= ; open Symbol

  exception InvalidId of int
  exception CannotSplitChannelArrays

  fun actions (P.Template {transitions, ...}) = let
      fun insert ((nm, dir), []) = [(nm, dir)]
        | insert ((nm, dir),  (x as (nm', dir'))::xs) =
            if nm=:=nm' andalso dir=dir' then x::xs else x::insert ((nm, dir), xs)

      fun addAct (P.Transition {sync=(NONE, _), ...}, l) = l
        | addAct (P.Transition {sync=(SOME (nm, dir, _), _), ...}, l)
              = insert ((nm, dir), l)
    in foldl addAct [] transitions end

  fun channelSet (P.Template {transitions, ...}) = let
      fun addSync (P.Transition {sync=(NONE, _), ...}          , s) = s
        | addSync (P.Transition {sync=(SOME (c, _, _), _), ...}, s) = s <+ c
    in foldl addSync AtomSet.empty transitions end

  fun channels t = AtomSet.listItems (channelSet t)

  fun warnIfCommitted t =
     if List.exists P.Location.isCommitted (P.Template.selLocations t)
     then Util.warn [P.Template.selName t,
                     " contains a committed location (not supported)."] else ()

  fun warnOnChannels (t as P.Template {declaration=env,...}) = let
      fun checkChannel nm = let
          val pre = Atom.toString nm
        in case Env.findVal env nm of
             NONE => SOME [pre, " not in scope."]
           | SOME (Env.VarEntry{ty,ref=r,...}) => (case #1 (E.stripArray ty) of
                E.CHANNEL {broadcast,...} =>
                  if broadcast
                  then SOME ["broadcast outputs on ", pre,
                             " to Err will be omitted; candidate models must",
                             " not input on ", pre, "."]
                  else if r
                       then SOME [pre,"is passed by reference (not supported)"]
                       else NONE
              | _ => NONE)

           | _   => SOME [pre, " not a channel."]
        end
    in List.app Util.warn (List.mapPartial checkChannel (channels t)) end

  local
    structure PLoc = P.Location
          and PTra = P.Transition
          and PTem = P.Template
  in
  fun split (tplate as P.Template {transitions, locations, ...}, actmap) = let
      fun addT (tp, ts) = PTem.addTransitions tp ts

      fun findLoc l = List.find (fn (P.Location {id,...})=>id = l) locations

      fun midPoint ((sx, sy), (tx, ty)) = ((sx + tx) div 2, (sy + ty) div 2)

      fun findPos ((src, dst), []) = let
              val sposo = Option.mapPartial PLoc.selPos (findLoc src)
              val dposo = Option.mapPartial PLoc.selPos (findLoc dst)
            in case (sposo, dposo) of
                 (SOME spos, SOME dpos) => (SOME (midPoint (spos,dpos)),[],[])
               | _ => (NONE,[],[])
            end

        | findPos ((src, dst), nails) = let
          val l = length nails
          val m = (l + 1) div 2
        in
          if l mod 2 = 1
          then (SOME (List.nth (nails, m-1)), List.take (nails, m-1),
                                              List.drop (nails, m))
          else (SOME (midPoint (List.nth (nails, m-1), List.nth (nails, m))),
                List.take (nails, m), List.drop (nails, m))
        end

      fun spl (tr as P.Transition {sync=(NONE, _), ...}, tp) = addT (tp, [tr])
        | spl (tr as P.Transition {sync=(SOME (nm,dir,[]), _),
                                  source, target, nails, ...}, tp) = let in
            case ActionMap.find (actmap, (nm, dir)) of
              NONE             => addT (tp, [tr])
            | SOME (nm', dir') => let
                  val (mpos, preNs, postNs) = findPos ((source, target), nails)

                  val (tp', sloc) = PLoc.new tp ""
                  val slocId = PLoc.selId sloc
                  val tp' = PTem.updLocation tp'
                              (PLoc.updPos (PLoc.mkCommitted sloc) mpos)

                  val fromSloc = PTra.updSync
                                   (PTra.new (slocId, target))
                                   (SOME (nm', dir', []))
                  val fromSloc = PTra.updNails fromSloc postNs
                in
                  addT (tp', [PTra.updNails
                                (PTra.updEndPoints tr (source,slocId)) preNs,
                              fromSloc])
                end
            end
        | spl _ = raise CannotSplitChannelArrays
      
    in foldl spl (PTem.updTransitions tplate []) transitions end
  end (* local *)

  local
    structure IMap = IntBinaryMap

    fun remap m i = case IMap.find (m, i) of
                       NONE    => raise InvalidId i
                     | SOME ni => P.LocId ni
    fun stripLocId (P.LocId i) = i
  in
  fun shiftTemplateIds (P.Template {name as (n,_), parameter, declaration,
                                    initial, locations, transitions}, start) =
    let
      fun uLoc (P.Location {id=P.LocId i, position, color, name, invariant,
                            comments, urgent, committed}, (done, m, ni)) =
        let
          val nloc = P.Location {id=P.LocId ni, position=position,
                                 color=color, name=name, invariant=invariant,
                                 comments=comments, urgent=urgent,
                                 committed=committed}
        in (nloc::done, IMap.insert (m, i, ni), ni + 1) end

      fun uTrans m (P.Transition {id, source=P.LocId si, target=P.LocId ti,
                                  select, guard, sync, update, comments,
                                  position, color, nails}, (done, ni)) =
        let
          val (nid, ni) = case id
                of NONE               => (NONE, ni)
                 | SOME (P.TransId i) => (SOME (P.TransId ni), ni + 1)
          val ntr = P.Transition {id=nid,
                                  source=remap m si,
                                  target=remap m ti,
                                  select=select, guard=guard,
                                  sync=sync, update=update,
                                  comments=comments, position=position,
                                  color=color, nails=nails}
        in  (ntr::done, ni) end
      
      val (nlocs, map, start') = foldl uLoc ([], IMap.empty, start) locations

      val _ = Util.debugVeryDetailed (fn()=>let
                                     fun s (f, t) = Int.toString f^"->"^
                                                    Int.toString t^" "
                                   in "shiftTemplateIds (":: n:: ") "::
                                      (List.map s (IMap.listItemsi map))
                                   end)

      val (ntrs, final) = foldl (uTrans map) ([], start') transitions
    in
      (P.Template {name=name, parameter=parameter,declaration=declaration,
                   initial=Option.map (remap map o stripLocId) initial,
                   locations=rev nlocs, transitions=rev ntrs}, final)
    end
  end

  fun setTransId nid (tr as P.Transition {id, source, target, select, guard,
                               sync, update, comments, position, color, nails})
      = let
          val nochange = case (nid, id) of
                           (NONE, NONE)      => true
                         | (SOME ni, SOME i) => (ni = i)
                         | _                 => false
        in
          if nochange then tr
          else (P.Transition {id=nid, source=source,
                              target=target, select=select,
                              guard=guard, sync=sync, update=update,
                              color=color, comments=comments,
                              position=position, nails=nails})
        end

  fun locsWithoutPositions (P.Template {locations,...}) = let
      val add = IntBinarySet.add
      fun f (P.Location {id=P.LocId i, position=NONE, ...}, s) = add (s, i)
        | f (_, s) = s
      val locs = foldl f IntBinarySet.empty locations
    in fn (P.LocId i) => IntBinarySet.member (locs, i) end

  fun stripTransitionIds (P.Template  {name, parameter, declaration, initial,
                                       locations, transitions}) =
      P.Template {name=name, parameter=parameter, declaration=declaration,
                  initial=initial, locations=locations,
                  transitions=map (setTransId NONE) transitions}

  fun addTransitionIds p (P.Template {name, parameter, declaration, initial,
                                      locations, transitions}) =
    let
      fun biggest (P.Transition {id=NONE, ...}, i) = i
        | biggest (P.Transition {id=SOME (P.TransId j), ...}, i) = Int.max (i, j)
      val i = ref (foldl biggest 1 transitions)

      fun f trans = if p trans then (setTransId (SOME (P.TransId (!i))) trans)
                                    before i := (!i) + 1
                    else trans
    in
      P.Template {name=name, parameter=parameter, declaration=declaration,
                  initial=initial, locations=locations,
                  transitions=map f transitions}
    end

  fun addTransitions (t, newtrans) = P.Template.updTransitions t
                                      (newtrans @ P.Template.selTransitions t)

  fun expandUrgentLocs (t as P.Template {name, parameter, initial,
                                         declaration=decls,
                                         locations, transitions}) =
    let
      fun addUrgentIds (P.Location {id=P.LocId i, urgent, ...}, s) =
                      if urgent then IntBinarySet.add (s, i) else s
      val urgentLocs = foldl addUrgentIds IntBinarySet.empty locations
    in
      if IntBinarySet.isEmpty urgentLocs then t
      else let
        val uclk     = Env.newId (Atom.atom "c_u", decls)
        val uclkvar  = E.VarExpr (E.SimpleVar uclk)
        val uclkcons = E.RelExpr {left=uclkvar, rel=E.LeOp, right=E.IntCExpr 0}
        val uclkrst  = E.AssignExpr {var=uclkvar, aop=E.AssignOp,
                                     expr=E.IntCExpr 0}

        val decls'   = Env.addId Env.TemplateScope ((uclk, E.CLOCK), decls)

        fun fromUrgent (P.Location {id, position, color, name,
                                    invariant=(inv, invP), comments,
                                    urgent=true, committed}) =
            P.Location {id=id, position=position, color=color, name=name,
                        invariant=(E.andexpr (uclkcons, inv), invP),
                        comments=comments, urgent=false, committed=committed}
          | fromUrgent l = l

        fun addReset (t as P.Transition {target=target as P.LocId dst, id,
                                         source, select, guard, sync,
                                         update=(upd, updP),
                                         comments, position, color, nails}) =
            if IntBinarySet.member (urgentLocs, dst)
            then P.Transition {id=id, source=source, target=target,
                               select=select, guard=guard, sync=sync,
                               update=(uclkrst::upd, updP),
                               comments=comments, position=position,
                               color=color, nails=nails}
            else t

      in
        P.Template {name=name, parameter=parameter,
                    declaration=decls', initial=initial,
                    locations=map fromUrgent locations,
                    transitions=map addReset transitions}
      end
    end

  fun makeInputEnabler (actionlist, env) = let
      val l = P.LocId 1

      fun newTrans (c, idtys,  ids, tys, dir) = P.Transition {
                id=NONE, source=l, target=l, select=(idtys, NONE),
                guard=(E.trueExpr, NONE), sync=(SOME (c, dir, ids), NONE),
                update=([], NONE), comments=(NONE, NONE),
                position=NONE, color=NONE, nails=[]}

      fun selfLoop (nm, dir) = let in
          case Option.map E.stripArray (Env.findValType env nm) of
            SOME (E.CHANNEL _, tys) => let
                val ids = List.tabulate (length tys,
                                         fn i=>Atom.atom ("s"^Int.toString i))
                val sels = ListPair.map (fn (nm, t)=>E.BoundId (nm, t))
                                        (ids, tys)
                val eids = map (fn i=>E.VarExpr (E.SimpleVar i)) ids
              in
                SOME (newTrans (nm, sels, eids, tys, dir))
              end

          | _ => (Util.warn ["channel ",Atom.toString nm,
                             " is invalid (skipped)"]; NONE)
        end

    in
      P.Template {name=("AcceptAll", NONE),
                  parameter=([], NONE),
                  initial=SOME l,
                  declaration=env,
                  locations=[P.Location {
                               id=l,
                               position=SOME (0,0),
                               color=NONE,
                               name=(NONE, NONE),
                               invariant=(E.trueExpr, NONE),
                               comments=(NONE, NONE),
                               urgent=false,
                               committed=false}],
                  transitions=List.mapPartial selfLoop actionlist}
    end

  fun namesetToLocset (names, P.Template {locations, ...}) = let
      fun f (P.Location {name=(NONE, _), ...}, s) = s
        | f (P.Location {id=P.LocId i, name=(SOME name, _), ...}, s) =
        if (Atom.atom name) <- names then IntBinarySet.add (s, i) else s
    in
      foldl f IntBinarySet.empty locations
    end

  fun scaleClocks (tp as P.Template {locations, transitions,
                                     declaration, ...}, sc) = let
      fun tyIsClk E.CLOCK           = true
        | tyIsClk (E.ARRAY (ty, _)) = tyIsClk ty
        | tyIsClk _                 = false

      fun isClock s = Option.getOpt (Option.map tyIsClk
                                       (Env.findValType declaration s), false)

      val mul = E.mulClocks (sc, isClock)

      fun fTr (P.Transition {id, source, target, select=select as (sel, _),
                             guard=(g, gP), sync, update=(upd, updP),
                             comments, position, color, nails}) =
        let
          val bound = foldl (fn (E.BoundId (nm,_), s)=> s <+ nm) emptyset sel
          val mul = E.mulClocks (sc, fn s=> not (s <- bound) andalso isClock s)
        in
          P.Transition {id=id, source=source, target=target, select=select,
                        guard=(mul g, gP), sync=sync,
                        update=(map mul upd, updP), comments=comments,
                        position=position, color=color, nails=nails}
        end

      fun fLoc loc = (P.Location.updInvariant loc
                        (mul (P.Location.selInvariant loc)))

    in P.Location.map fLoc (P.Transition.map fTr tp) end

end
end (* local *)

