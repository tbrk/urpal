(* $Id$ *)

(* TODO:
    * Tidy up this API and implementation. Integrate and generalise the various
      functions that were thrown together from different parts of the system.

    * Maybe create a functor that can work over any NTA (text or parsed)?
 *)

local structure P    = ParsedNta
            and E    = UppaalParse.Expression
            and ECvt = UppaalParse.ExpressionCvt

            and Dot      = TypedDot
            and Plain    = TextPlain
            and EdgeAtt  = TypedAttributes.Edge
            and NodeAtt  = TypedAttributes.Node
            and GraphAtt = TypedAttributes.Graph
in

structure Layout : LAYOUT
=
struct
  (* shortcuts over Atom and AtomSet *)
  infix <+ <- ++ <\ \ =:= ; open Symbol

  type pos = ParsedNta.pos

  (* Constants for GraphViz interface *)
  val dotScale = 100.0

  (* Constants for tabulate *)
  val (lineHeight, charWidth, sectionGap) = (18, 7, 8)

  (* Constants for matrixTrans *)
  val loopHorOffset   = ~40   (* self-loop horizontal offset            *)
  val loopVerOffset   = 30    (* self-loop vertical offset              *)
  val jumpHorOffset   = ~80   (* same-column jump transition separation *)
  val crossColEdge    = 60    (* cross-column horizontal shift          *)


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


  (* (pos, (dx, dy)) = positionLabel coords *)
  (* pre: length(coords) >= 2 (it includes source and target end-points) *)
  fun positionLabel [] = raise Fail "positionLabel: not called correctly"
    | positionLabel (coords as (fx, fy)::_) = let
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
           (* labels always left-to-right *)
           then ((x1 - (dx + dx div 3), y1 - (dy + dy div 3)), (~dx, ~dy))
           else ((x0 + (dx + dx div 3), y0 + (dy + dy div 3)), (dx, dy))
        end
        | f _ = raise Fail "positionLabel: internal error"

    in f (case length coords of
            2 => coords
          | 3 => tl (coords)
          | 4 => tl (coords)
          | _ => tl (tl coords))
    end

    fun positionLabels locMap (tr as P.Transition {id, source, target,
                                            select=(sel, _), guard=(g, _),
                                            sync=(sync, _), update=(upd, _),
                                            comments, position, color, nails}) =
      case (locMap source, locMap target) of
        (SOME srcPos, SOME tgtPos) => let
              val coords = srcPos:: (case nails of
                                       [] => [tgtPos]
                                     | ns => ns)

              val (pos1 as (px, py), (dx, dy)) = positionLabel coords
              val (pos1, (dx, dy)) = if dx <> 0
                             then (pos1, (dx, dy))
                             else ((px + 5, py), (0, Int.sign dy * lineHeight))
                                  (* treat vertical case specially *)
              fun inc (x, y) = (x + dx, y + dy)

              fun listPos ([], p) = (([], NONE), p)
                | listPos (xs, p) = ((xs, SOME p), inc p)

              val (select, pos2) = listPos (sel, pos1)
              val (guard,  pos3) = case g of
                                     E.BoolCExpr true => ((g, NONE), pos2)
                                   | _                => ((g, SOME pos2), inc pos2)
              val (sync,   pos4) = case sync of
                                     NONE   => ((NONE, NONE), pos3)
                                   | SOME _ => ((sync, SOME pos3), inc pos3)
              val (update, _) = listPos (upd, pos4)
            in
              P.Transition {id=id, source=source, target=target,
                            select=select, guard=guard, sync=sync,
                            update=update, comments=comments,
                            position=position, color=color, nails=nails}
            end

      | _ => tr

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

        | updateTrans (trans as P.Transition {id=id as SOME (P.TransId i),
                                              nails=[], ...})=
          (case IMap.find (edgemap, i) of
             NONE => trans
           | SOME {nails, ...} => positionLabels (fn id=>SOME (getNodePos id))
                                            (P.Transition.updNails trans nails))
        | updateTrans trans = trans
    in
      P.Template  {name=name, parameter=parameter, declaration=declaration,
                   initial=initial,
                   locations=map updateLoc locations,
                   transitions=map updateTrans transitions}
    end

  end (* local *)

  local
    fun width str = charWidth * size str + sectionGap
                    (* estimate only (cross-platform accuracy would
                     * introduce library dependencies/complications) *)

    fun estimateWidths (P.Transition {select=(sel,_), guard=(g, _),
                                      sync=(syn, _),...}, (selW,gW,synW))
        = (Int.max (width (ECvt.selectToString sel),   selW),
           Int.max (width (ECvt.Expr.toString g),      gW),
           Int.max (width (Option.getOpt (Option.map ECvt.syncToString syn,
                                          "")), synW))

    fun tabulateLabels ((bx, by), (selW, gW, synW)) trans = let
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
    in List.revAppend (tabulateLabels (pos, widths) toTabulate, dontTouch) end

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

  local
    infixr :::
    fun NONE ::: xs = xs | (SOME x) ::: xs = x::xs

    fun getHorizOffset (x, (ly, uy), others) = let
        fun overlapping (x', (ly', uy'), h) =
          if (x' = x andalso uy >= ly' andalso ly <= uy')
             (*  overlap of two jump links in the same column. *)
          then SOME h else NONE

        val offsets = List.mapPartial overlapping others
        fun getNext c = if List.exists (fn h=>h=c) offsets
                        then getNext (c + jumpHorOffset) else c

      in getNext (x + loopHorOffset + jumpHorOffset) end

    fun joinLoop (x, y) = [(x + loopHorOffset, y + loopVerOffset),
                           (x + loopHorOffset, y - loopVerOffset)]

    fun joinRows ((sx, sy), (dx, dy), h) = [(sx + h, sy), (dx + h, dy)]

  in
  fun joinColumns ((sx, sy), (dx, dy)) = if sx < dx
         then [(sx + crossColEdge, sy), (dx - crossColEdge, dy)]
         else [(sx - crossColEdge, sy), (dx + crossColEdge, dy)]

  fun matrixTrans (locIdToPos, jmpTrans) = let
      fun sortPair (y1, y2) = if y1 < y2 then (y1, y2) else (y2, y1)
      val posLabels = positionLabels locIdToPos

      fun doTrans (tr, (others, done)) = let
          val (sLoc, dLoc) = P.Transition.selEndPoints tr
          val (sp as (sx,sy), dp as (dx,dy)) = (valOf (locIdToPos sLoc),
                                                valOf (locIdToPos dLoc))

          val (ns, v) = if sx <> dx then (joinColumns (sp, dp), NONE)
            else        if sy = dy  then (joinLoop sp, NONE)
            else let val h = getHorizOffset (sx, sortPair (sy, dy), others)
                 in (joinRows (sp, dp, h), SOME (sx, sortPair (sy, dy), h)) end

        in (v:::others, posLabels (P.Transition.updNails tr ns)::done) end

    in #2 (foldl doTrans ([], []) jmpTrans) end
  end (* local *)

end
end (* local *)

