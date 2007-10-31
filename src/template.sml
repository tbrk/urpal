(* $Id$ *)

(* TODO:
    * bundle up AtomSet operators (+<, =:=, ++, etc.) into a struct we
      can just open.
    * Make error handling more descriptive:
        -catch the exception
        -name the template and state
        -show the bad transitions.
    * Tidy this whole file up. Try to effect more structure and reuse.
 *)

local structure P    = ParsedNta
            and E    = UppaalParse.Expression
            and ECvt = UppaalParse.ExpressionCvt
            and Env  = UppaalParse.Environment
            and TF   = TransitionFlipper
            and UXML = UppaalXML

            and Dot      = TypedDot
            and Plain    = TextPlain
            and EdgeAtt  = TypedAttributes.Edge
            and NodeAtt  = TypedAttributes.Node
            and GraphAtt = TypedAttributes.Graph
in

structure Template : TEMPLATE
=
struct
  (* shortcuts over Atom and AtomSet *)
  infix <+ <- ++ </ =:= ; open Symbol

  exception InvalidChannelId of symbol
  exception MissingChannelIds of symbol list
  exception SilentTransition 
  exception NoChannels
  exception InvalidId of int
  exception BadSubscriptType of string * string
                        (* channel name, subscript type *)

  val dotScale = 100.0

  val trueExpr = E.BoolCExpr true
  val falseExpr = E.BoolCExpr false

  fun maxTemplateId (P.Template {locations, transitions, ...}) = let
      fun maxLoc (P.Location {id=P.LocId i, ...}, m)           = Int.max (m, i)

      fun maxTrans (P.Transition {id=NONE, ...}, m)            = m
        | maxTrans (P.Transition {id=SOME(P.TransId i),...},m) = Int.max (m, i)

    in foldl maxTrans (foldl maxLoc 0 locations) transitions end

  fun usedChannels (P.Template {transitions, ...}) = let
      fun insert ((nm, dir), []) = [(nm, dir)]
        | insert ((nm, dir), (nm', dir')::xs) =
            if nm=:=nm' andalso dir=dir' then xs else insert ((nm, dir), xs)

      fun addChan (P.Transition {sync=(NONE, _), ...}, l) = l
        | addChan (P.Transition {sync=(SOME (nm, dir, _), _), ...}, l)
              = insert ((nm, dir), l)
    in foldl addChan [] transitions end

  fun newLocationName (basename, locations) = let
      fun addName (P.Location {name, ...}, s)   = case name
              of (NONE, _)   => s
               | (SOME n, _) => AtomSet.add (s, Atom.atom n)
      val usednames = foldl addName AtomSet.empty locations
    in Atom.toString (getNewName (Atom.atom basename, usednames)) end

  fun validChannelId env id = let
      fun isChannel (E.CHANNEL _)     = true
        | isChannel (E.ARRAY (ty, _)) = isChannel ty
        | isChannel _                 = false

    in case Env.findValType env id
         of SOME ty => isChannel ty
          | NONE    => false
    end

  fun warnIfCommitted t =
     if P.locationExists P.isCommittedLocation t
     then Util.warn [P.Template.selName t,
                     " contains a committed location (not supported)."] else ()

  fun warnOnChannels (t as P.Template {declaration=env,...}) = let
      fun checkChannel (nm, _) = let
          val pre = Atom.toString nm
        in case Env.findVal env nm of
             NONE => SOME [pre, " not in scope."]
           | SOME (Env.VarEntry{ty,ref=r,...}) => (case #1 (E.stripArray ty) of
                E.CHANNEL {broadcast,...} =>
                  if broadcast
                  then SOME [pre,"is broadcast (not supported)"]
                  else if r
                       then SOME [pre,"is passed by reference (not supported)"]
                       else NONE
              | _ => NONE)

           | _   => SOME [pre, " not a channel."]
        end
    in List.app Util.warn (List.mapPartial checkChannel (usedChannels t)) end

  fun makeTransition (src as P.LocId s, dst as P.LocId d, sids, g, synco) = let
      val _ = Util.debugVeryDetailed (fn()=>["new transition: ",Int.toString s,
                                             "->", Int.toString d, " "])
    in
          P.Transition {id=NONE, source=src, target=dst, select=(sids, NONE),
                        guard=(g, NONE), sync=(synco, NONE),
                        update=([], NONE), comments=(NONE, NONE),
                        position=NONE, color=Settings.errorColor(), nails=[]}
    end

  fun formInvariantTrans (errloc, locs) = let
      fun doLoc (P.Location {id, invariant=(e, _), ...}) =
          if E.equal (e, trueExpr)
          then NONE
          else SOME (makeTransition (id, errloc, [], E.negate e, NONE))
    in List.mapPartial doLoc locs end

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

      fun flipFailed (P.Location {id=P.LocId l, name=(nameo, _), ...},
                      chanId, errMsg) =
        let
          val name = case nameo of
                       NONE   => ("unnamed state (id" ^ Int.toString l ^ ")")
                     | SOME n => n
        in
          Util.warn  [errMsg];
          Util.abort ["while processing transitions on channel ",
                      Atom.toString chanId, " from ", name]
        end

      fun doLoc (location as P.Location {id=loc as P.LocId l,
                                         invariant=(inv, _), ...}) = let

          val _ =Util.debugIndent (Settings.Detailed,
                                fn()=>["processing location:", Int.toString l])

          fun makeTrans (chanId, dir) {selectids, actionsubs, guard} =
              if E.equal (guard, falseExpr)
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
            in (List.mapPartial (makeTrans (chanId, E.Output))   (negate is))
                @ (List.mapPartial (makeTrans (chanId, E.Input)) (negate os))
               before (Util.debugOutdent (Settings.Detailed, fn()=>[]))
            end
            handle TF.FlipFailed s => flipFailed (location, chanId, s)

          fun startsHere (P.Transition {source=P.LocId s, ...}) = (s = l)
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

  fun invertActionAndAddInvariant (env, invmap)
      (P.Transition {id, source=source as P.LocId src, target,
                     guard=(g, gpos), sync=(sync, syncpos),
                     select, update, comments, position, color, nails}) = let
      val inv = valOf (IntBinaryMap.find (invmap, src))
      val g' = if E.equal (inv, E.trueExpr)
               then g
               else if E.equal (g, E.trueExpr)
                    then inv
                    else TF.andexpr env (g, inv)
      val sync' = case sync of
                    NONE                    => NONE
                  | SOME (s, E.Input, idx)  => SOME (s, E.Output, idx)
                  | SOME (s, E.Output, idx) => SOME (s, E.Input,  idx)
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

  fun inverse (channelIds, t as P.Template {name, parameter, declaration,
                                            initial, locations, transitions}) =
    let
      val _ = if null channelIds then raise NoChannels
                                 else ()
      val _ = case List.find (fn c=>not (validChannelId declaration c))
                  channelIds of
                 NONE => ()
               | SOME c=> raise InvalidChannelId c

      val cIdGiven = foldl AtomSet.add' AtomSet.empty channelIds
      val cIdUsed  = channelSet transitions
      val cMissing = AtomSet.difference (cIdUsed, cIdGiven)
      val _ = if AtomSet.isEmpty cMissing
              then () else raise MissingChannelIds (AtomSet.listItems cMissing)
      
      val errName = newLocationName ("Err", locations)
      val errLocId = P.LocId (maxTemplateId t + 1)

      val err = P.Location {id        = errLocId,
                            position  = NONE,
                            color     = Settings.errorColor(),
                            name      = (SOME errName, NONE),
                            invariant = (trueExpr, NONE),
                            comments  = (NONE, NONE),
                            urgent    = false,
                            committed = false}

      fun keyInv (P.Location {id=P.LocId i,invariant=(inv, _), ...}) = (i,inv)
      fun insertInv (l, m) = IntBinaryMap.insert' (keyInv l, m)
      val invMap = foldl insertInv IntBinaryMap.empty locations

      val invarianttrans = formInvariantTrans (errLocId, locations)
      val fliptrans = formInverseTrans (errLocId, declaration, channelIds)
                                       (locations, transitions)
      val normtrans = map (invertActionAndAddInvariant (declaration, invMap))
                          transitions
      
      val errorloop = [P.Transition {id=NONE, source=errLocId, target=errLocId,
                                   select=([], NONE), guard=(E.trueExpr, NONE),
                                   sync=(NONE, NONE), update=([], NONE),
                                   comments=(NONE, NONE),   position=NONE,
                                   color=Settings.errorColor(), nails=[]}]
    in
      (errLocId, P.Template {name=name,
                             parameter=parameter,
                             declaration=declaration,
                             initial=initial,
                             locations=err::map stripInvariant locations,
                             transitions=List.concat [invarianttrans,fliptrans,
                                                      normtrans,errorloop]})
    end

  fun getTemplate (P.Nta {templates, ...}) n = let
      fun p (P.Template {name=(m, _), ...}) = (String.compare (n, m) = EQUAL)
    in List.find p templates end

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

  fun addTemplates (ts, P.Nta {imports, declaration, templates,
                               instantiation, system}) =
    let
      val start = foldl (fn (t, m) => Int.max (m, maxTemplateId t)) 0 templates

      fun renumber ([], _) = []
        | renumber (t::ts, s) = let val (t', s') = shiftTemplateIds (t, s)
                                in t'::renumber (ts, s') end
    in
      P.Nta {imports=imports, declaration=declaration,
             templates=renumber (ts, start) @ templates,
             instantiation=instantiation, system=system}
    end

  fun stripTemplates (P.Nta {imports, declaration, templates,
                             instantiation, system})
      = P.Nta {imports=imports, declaration=declaration, templates=[],
               instantiation=instantiation, system=system}

  fun toDot forLayoutOnly (P.Template {name=(name, _), parameter, initial,
                                       locations, transitions, ...}) =
  let
    fun optInclude (id, v) = case initial of
                               NONE                => NONE
                             | SOME (P.LocId init) => if init = id then SOME v
                                                      else NONE

    infixr //=; fun v //= xs = case v of NONE   => xs
                                      |  SOME x => x::xs
    infix 2 <?; fun f <? v = Option.map f v


    fun makeId i = "id"^Int.toString i
    fun toPointf (x, y) = (Real./ (Real.fromInt x, dotScale),
                           Real./ (Real.fromInt (~y), dotScale))
    fun toList x = [x]

    fun toLabel v = NodeAtt.PlainLabel (if forLayoutOnly then  ""
                                        else case v of NONE     => ""
                                                     | (SOME l) => l)

    fun makeNode (P.Location {id=P.LocId i, name=(nameo, _), position,
                              color, invariant, urgent, committed, ...}) =
        Dot.Node
          {id=makeId i,
           atts=optInclude (i, NodeAtt.DoubleCircleShape)               //=
                (NodeAtt.Color o toList o NodeAtt.RGB) <? Option.mapPartial
                               NodeAtt.X11Color.rgbFromString color     //=
                SOME (toLabel nameo)                                    //=
                (NodeAtt.Pos o toPointf)                   <? position  //= []}

    fun addEdge (P.Transition {id, source=P.LocId s, target=P.LocId t,
                               select=(select, _), guard=(guard, _),
                               sync=(synco, _), update=(update, _),
                               position, color, nails, ...}, graph) =
      let
        val idstr = case id of
                      NONE => ""
                    | SOME (P.TransId i) => Int.toString i ^":"

        val select  = if null select then ""
                      else ECvt.selectToString select ^"\n"
        val guard   = case ECvt.Expr.toString guard of
                        "true" => ""
                      | s      => s^"\n"
        val syncstr = case synco of
                        NONE   => ""
                      | SOME s => ECvt.syncToString s ^"\n"
        val update  = ListFormat.fmt {init="", final="", sep=", ",
                                      fmt=ECvt.Expr.toString} update

        val label = String.concat [idstr, select, guard, syncstr, update]
      in
        TypedDotUtil.nailPath (EdgeAtt.Single (EdgeAtt.Vee {side=NONE}))
          {graph=graph, src=Dot.NodeId (makeId s), dst=Dot.NodeId (makeId t),
           nails=map toPointf nails,
           atts=(EdgeAtt.Color o toList o EdgeAtt.RGB) <? Option.mapPartial
                               EdgeAtt.X11Color.rgbFromString color //= [],
           labelatts=[EdgeAtt.PlainLabel label]}
      end
    
  in
    (* TODO: Try to preserve label positions *)
    Dot.Graph {strict=false,
               directed=true,
               graph=foldl addEdge
                  (Dot.Subgraph {
                     name=SOME name,     (* TODO: add parameter? *)
                     attributes=[GraphAtt.Splines_line(*,
                                 GraphAtt.NodeSep 1.0*)],
                     nodeAtts=  [NodeAtt.CircleShape,
                                 NodeAtt.FontName "Helvetica",
                                 NodeAtt.FontSize 14.0,
                                 NodeAtt.FixedSize,
                                 NodeAtt.Height 0.3,
                                 NodeAtt.Width  0.3,
                                 NodeAtt.Pin],
                     edgeAtts=  [EdgeAtt.FontName "Helvetica",
                                 EdgeAtt.FontSize 14.0],
                     nodes=map makeNode locations,
                     edges=[],
                     subgraphs=[]
                  }) transitions
              }
  end


  (* (pos, (dx, dy)) = positionLabel ((lx, ly), coords) *)
  (* pre: length(coords) >= 2 (it includes source and target end-points) *)
  fun positionLabel (_, []) = raise Fail "positionLabel: not called correctly"
    | positionLabel (_, coords as (fx, fy)::_) = let
      val (maxd, dythresh) = (45.0, 10)
      (*
       * maxd     - the maximum distance between two labels, comes into effect
       *            for line segments longer than 5 * maxd. The intended effect
       *            is to cluster labels toward the source node on longer
       *            transitions.
       * dythresh - when the vertical component of the slope is less than this
       *            value (i.e. the line is mostly horizontal), 2 * maxd is
       *            used.
       *)
      fun limit (dx, dy) = let
          val theta = Math.atan2 (Real.fromInt dy, Real.fromInt dx)

          fun findSide trigf = let
                val max = if abs dy < dythresh
                          then Real.* (2.0, maxd) else maxd
            in Real.toInt IEEEReal.TO_NEAREST (Real.* (max, trigf theta)) end

          val (ix, iy) = (findSide Math.cos, findSide Math.sin)

        in if abs ix < abs dx andalso abs iy < abs dy then (theta, ix, iy)
                                                      else (theta, dx, dy)
        end

      fun f ((x0, y0)::(x1, y1)::_) = let
          val (angle, dx, dy) = limit ((x1 - x0) div 5, (y1 - y0) div 5)

          val slopemin = Real.* (5.0, Real./ (Math.pi, 8.0)) (* \| *)
        in
           if dx < 0 andalso Real.> (Real.abs angle, slopemin)
           then ((x1, y1), (~dx, ~dy)) (* labels always left-to-right *)
           else ((x0, y0), (dx, dy))
        end
        | f _ = raise Fail "positionLabel: internal error"

    in f (case length coords of
            2 => coords
          | 3 => tl (coords)
          | 4 => tl (coords)
          | _ => tl (tl coords))
    end

  local
    structure IMap = IntBinaryMap
  in

  (* Note: Only transitions with a transId are considered. The function
   *       translates the first line of the label into an integer for
   *       matching with transition ids.                             *)
  fun addCoordsFromPlain (P.Template  {name, parameter, declaration, initial,
                                       locations, transitions},
                          {nodes, edges, ...} : TextPlain.graph) =
    let
      (* assume ids have the form "id<num>" *)
      fun fromReal r = Real.round (Real.* (r, dotScale))
                        handle Overflow => 0    (* TODO: find out why fdp
                                                         sometimes gives huge
                                                         label positions *)
      fun fromPoint (x, y) = (fromReal x, ~(fromReal y))

      fun fromId id = Int.fromString (String.extract (id, 2, NONE))
      fun fromNode ({name, x, y, ...} : TextPlain.node) =
                    Option.map (fn i=>(i, fromPoint (x, y))) (fromId name)
                                                        
      fun insertNode (n, m) = case fromNode n of
                                NONE   => m
                              | SOME d => IMap.insert' (d, m)

      fun fromLabelPos (_, x, y) = fromPoint (x, y)
      fun idFromLabel c = Option.map #1
                    (Int.scan StringCvt.DEC Substring.getc (Substring.full c))

      fun fromEdge ({label=NONE, points, ...} : TextPlain.edge) = NONE
        | fromEdge {label=SOME (l,lx,ly), points, ...} =
            Option.map (fn id=>(id, {nails=map fromPoint points,
                                     labelpos=fromPoint (lx, ly)}))
                       (idFromLabel l)
      fun insertEdge (d, m) = case fromEdge d of
                                NONE => m
                              | SOME e => IMap.insert' (e, m)

      val nodemap = foldl insertNode IMap.empty nodes
      val edgemap = foldl insertEdge IMap.empty edges

      fun getNodePos (P.LocId i) = Option.getOpt (IMap.find (nodemap, i),(0,0))

      fun insertDelta (P.Location {id=P.LocId i, position=SOME (px,py),...},m)=
            let val (dx, dy) = case IMap.find (nodemap, i) of
                                 NONE          => (0, 0)
                               | SOME (nx, ny) => (nx - px, ny - py)
            in IMap.insert' ((i, (dx, dy)), m) end
        | insertDelta (_, m) = m
      (* offsets between locations positions and those in plain. *)
      val nodedeltas = foldl insertDelta IMap.empty locations

      fun updateLoc (loc as P.Location {id=id as P.LocId i,
                                 position=SOME pos, color,
                                 name, invariant, comments,
                                 urgent, committed}) =
            (case IMap.find (nodedeltas, i) of
               NONE          => loc
             | SOME (dx, dy) => let (* Graphviz sometimes translates the
                                     * pinned nodes, we shift them back... *)
                                   fun sh (x, y) = (x + dx, y + dy)
                                   fun shlabel (l, NONE)   = (l, NONE)
                                     | shlabel (l, SOME p) = (l, SOME (sh p))
                                in
                                  P.Location {id=id, position=SOME (sh pos),
                                              color=color, name=shlabel name,
                                              invariant=shlabel invariant,
                                              comments=shlabel comments,
                                              urgent=urgent,
                                              committed=committed}
                                end)

        | updateLoc (P.Location {id=id as P.LocId i, color, name, invariant,
                                 comments, urgent, committed, ...}) =
            P.Location {position=IMap.find (nodemap, i),
                        id=id, color=color, name=name, invariant=invariant,
                        comments=comments, urgent=urgent, committed=committed}

      fun updateTrans (trans as P.Transition
              {id=NONE, source=src as P.LocId si, target, select, guard, sync,
               update, comments, position, color, nails}) =
          (* Shift labels by the stored deltas if appropriate. *)
          (case IMap.find (nodedeltas, si) of
             NONE          => trans
           | SOME (dx, dy) => let
                                 fun sh (x, y) = (x + dx, y + dy)
                                 fun shlabel (l, NONE)   = (l, NONE)
                                   | shlabel (l, SOME p) = (l, SOME (sh p))
                               in
                                 P.Transition {id=NONE, source=src,
                                               target=target,
                                               select=shlabel select,
                                               guard=shlabel guard,
                                               sync=shlabel sync,
                                               update=shlabel update,
                                               comments=comments,
                                               position=Option.map sh position,
                                               color=color,
                                               nails=map sh nails}
                               end)

        | updateTrans (trans as P.Transition
              {id=id as SOME (P.TransId i), source, target,
               select=(sel, NONE), guard=(g, NONE), sync=(sy, NONE),
               update=(upd, NONE), comments, position, color, nails=[]}) =
          (* Position labels *)
          (case IMap.find (edgemap, i) of
             NONE => trans
           | SOME {nails, labelpos=(lx, ly)} => let
                 val coords = getNodePos source:: (case nails of
                                                     [] => [getNodePos target]
                                                   | ns => ns)

                 val (pos1, (dx, dy)) = positionLabel ((lx, ly), coords)
                 fun inc (x, y) = (x + dx, y + dy)
                 val pos2 = inc pos1
                 val pos3 = inc pos2
                 val pos4 = inc pos3
                 fun listPos ([], _) = ([], NONE)
                   | listPos (xs, p) = (xs, SOME p)

                 val select = listPos (sel, pos1)
                 val guard  = case g of
                                E.BoolCExpr true => (g, NONE)
                              | _    => (g, SOME pos2)
                 val sync   = (sy, Option.map (fn _=>pos3) sy)
                 val update = listPos (upd, pos4)
               in
                 P.Transition {id=id, source=source, target=target,
                               select=select, guard=guard, sync=sync,
                               update=update, comments=comments,
                               position=position, color=color, nails=nails}
               end)
        | updateTrans trans = trans
    in
      P.Template  {name=name, parameter=parameter, declaration=declaration,
                   initial=initial,
                   locations=map updateLoc locations,
                   transitions=map updateTrans transitions}
    end

  end (* local *)

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

  local
    val (lineHeight, charWidth, sectionGap) = (18, 7, 8)
    fun width str = charWidth * size str + sectionGap
                    (* estimate only (cross-platform accuracy would
                     * introduce library dependencies/complications) *)

    fun estimateWidths (P.Transition {select=(sel,_), guard=(g, _),
                                      sync=(syn, _),...}, (selW,gW,synW))
        = (Int.max (width (ECvt.selectToString sel),   selW),
           Int.max (width (ECvt.Expr.toString g),      gW),
           Int.max (width (Option.getOpt (Option.map ECvt.syncToString syn,
                                          "")), synW))

    fun positionLabels ((bx, by), (selW, gW, synW)) trans = let
        fun f  ([], _) = []
          | f (P.Transition {id, source, target, select=(sel, _),
                             guard=(g, _), sync=(syn, _),
                             update=(upd, _), comments,
                             position, color, nails}::trans, lineY) =
          let
            val gx = bx + selW + sectionGap
            val synx = gx + gW + sectionGap
            val updx = synx + synW + sectionGap
          in
            P.Transition {id=id, source=source, target=target,
                          select=(sel, SOME (bx,   lineY)),
                          guard =(g,   SOME (gx,   lineY)),
                          sync  =(syn, SOME (synx, lineY)),
                          update=(upd, SOME (updx, lineY)),
                          comments=comments, position=position, color=color,
                          nails=nails} :: f (trans, lineY + lineHeight)
          end
      in f (trans, by) end
    
    fun doTrans (pos, transP, trans) = let
      val (toTabulate, dontTouch) = List.partition transP trans
      val widths = foldl estimateWidths (0,0,0) toTabulate
    in List.revAppend (positionLabels (pos, widths) toTabulate, dontTouch) end

  in
  fun tabulateTransLabels (pos, transP,
                            P.Template {name, parameter, declaration,
                                        initial, locations, transitions}) =
      P.Template {name=name, parameter=parameter, declaration=declaration,
                  initial=initial, locations=locations,
                  transitions=doTrans (pos, transP, transitions)}

  fun tabulateAll (dstP, P.Template {name, parameter, declaration,
                                     initial,locations,transitions}) =
    let
      fun doLoc (P.Location {id, position, color, name, invariant,
                             comments, urgent, committed}, trans) =
        let
          fun transP (P.Transition{source=s,target=d,...})= s=id andalso dstP d
          val {xoff, yoff} = Settings.tabulateShift ()
        in case position of
             NONE       => trans
           | SOME (x,y) => doTrans ((x+xoff,y+yoff), transP, trans)
        end
    in
      P.Template {name=name, parameter=parameter, declaration=declaration,
                  initial=initial, locations=locations,
                  transitions=foldl doLoc transitions locations}
    end

  end (* local *)

  fun addTransitions (P.Template {name, parameter, declaration, initial,
                                  locations, transitions=extrans}, newtrans) =
      P.Template {name=name, parameter=parameter, declaration=declaration,
                  initial=initial, locations=locations,
                  transitions=newtrans @ extrans}

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
        val uclkvar  = E.VarExpr (E.SimpleVar (uclk, E.nopos))
        val uclkcons = E.RelExpr {left=uclkvar, rel=E.LeOp,
                                  right=E.IntCExpr 0, pos=E.nopos}
        val uclkrst  = E.AssignExpr {var=uclkvar, aop=E.AssignOp,
                                     expr=E.IntCExpr 0, pos=E.nopos}

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

  (********** Jensen extensions: ****************************************)
  (* This section was written very quickly. It would probably benefit from
   * restructuring and refactoring *)

  fun makeComparison map = let 
    (* makeComparison env map expr
     *
     * Return an expression, for all v_i in domain(map):
     *    /\  (v_i == map(v_i))
     *      i
     *
     * This is the negation of the expression used in Jensen's expansion because
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

  datatype shared_variables = CheckGlobals
                            | CheckParameters
                            | CheckList of symbol list

  fun makeVarCheckTrans whichVars (t as P.Template {name, parameter, initial,
                        declaration=decl, locations=locs, transitions=trans}) =
    let
      fun goodTy (E.INT (_, E.NoQual))       = true
        | goodTy (E.BOOL E.NoQual)           = true
        | goodTy (E.SCALAR (_, E.NoQual, _)) = true
        | goodTy (E.RECORD (_, E.NoQual, _)) = true
        | goodTy (E.ARRAY (ty, _))           = goodTy ty
        | goodTy (E.NAME (_, E.NoQual, _))   = true
        | goodTy _                           = false

      val varP = case whichVars of
           CheckGlobals   =>(fn (_,t,Env.GlobalScope)   =>goodTy t | _ =>false)
         | CheckParameters=>(fn (_,t,Env.ParameterScope)=>goodTy t | _ =>false)
         | CheckList syms =>let val names = foldl AtomSet.add' emptyset syms
                            in fn (nm,t,_) => goodTy t andalso nm <- names end

      val (dupmap, decl') = Env.dupVariables varP (decl,SOME Env.TemplateScope)

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
    fun allocateIndexVariables (env, chansAndTypes) = let
        fun f (vars, [], usednames, done) = (vars, done)
          | f (vars, (nm, subtys)::chans, usednames, done) = let
                val (vars', usednames', idxvars) = allocVars (vars,vars,
                                                              usednames,[],
                                                              subtys)
              in f (vars', chans, usednames', (nm, idxvars)::done) end
              handle BadSub ty => raise BadSubscriptType (Atom.toString nm, ty)

        fun addId ((nm, ity), env) = Env.addId Env.TemplateScope
                                               ((nm, indexToType ity), env)
        val (idxvars, chanList) = f ([], chansAndTypes, Env.usedIds env, [])

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

                  val (shift_o, shift_u) = (Settings.splitShiftOriginal(),
                                            Settings.splitShiftUrgent())
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

                  val l_sh=P.shiftLocation (Settings.splitShiftOriginal()) l
                  val prel_sh=P.shiftLocation(Settings.splitShiftUrgent()) prel
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

      val urgchans = List.mapPartial chanTypeToSubs channels
      val (decls, urgchanIdxs) = allocateIndexVariables (decls, urgchans)
      val urgchans = List.concat (map addUsedDirections urgchanIdxs)
    in
      if null urgchans then t
      else let
        val locNames = foldl (fn (P.Location{name=(SOME nm,_),...},s)
                                          => s <+ Atom.atom nm
                               | (_, s) => s) emptyset locs
        val nextid = maxTemplateId t + 1

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

  (********** Extras: ****************************************)
  fun makeInputEnabler (chanlist, env) = let
      val l = P.LocId 1

      fun newTrans (c, idtys,  ids, tys, dir) = P.Transition {
                id=NONE, source=l, target=l, select=(idtys, NONE),
                guard=(E.trueExpr, NONE), sync=(SOME (c, dir, ids), NONE),
                update=([], NONE), comments=(NONE, NONE),
                position=NONE, color=NONE, nails=[]}

      fun selfLoop c = let in
          case Option.map E.stripArray (Env.findValType env c) of
            SOME (E.CHANNEL _, tys) => let
                val ids = List.tabulate (length tys,
                                         fn i=>Atom.atom ("s"^Int.toString i))
                val sels = ListPair.map (fn (nm,t)=>E.BoundId (nm,t,E.nopos))
                                        (ids, tys)
                val eids = map (fn i=>E.VarExpr (E.SimpleVar (i,E.nopos))) ids
              in
                [newTrans (c, sels, eids, tys, E.Input),
                 newTrans (c, sels, eids, tys, E.Output)]
              end

          | _ =>(Util.warn ["channel ",Atom.toString c," is invalid (skipped)"];
                 [])
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
                  transitions=List.concat (map selfLoop chanlist)}
    end

end
end (* local *)

