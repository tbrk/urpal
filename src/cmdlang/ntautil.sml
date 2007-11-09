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

            and Dot      = TypedDot
            and Plain    = TextPlain
            and EdgeAtt  = TypedAttributes.Edge
            and NodeAtt  = TypedAttributes.Node
            and GraphAtt = TypedAttributes.Graph
in

structure NtaUtil : NTA_UTIL
=
struct
  (* shortcuts over Atom and AtomSet *)
  infix <+ <- ++ </ =:= ; open Symbol

  exception InvalidId of int
  exception CannotSplitChannelArrays

  val dotScale = 100.0

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
                  then SOME [pre,"is broadcast (not supported)"]
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
                val sels = ListPair.map (fn (nm,t)=>E.BoundId (nm,t,E.nopos))
                                        (ids, tys)
                val eids = map (fn i=>E.VarExpr (E.SimpleVar (i,E.nopos))) ids
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
end
end (* local *)

