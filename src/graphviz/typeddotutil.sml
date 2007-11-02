(* $Id$ *)

structure TypedDotUtil :
sig
  structure Dot      : DOT
        and GraphAtt : GRAPH_ATTRIBUTE
        and NodeAtt  : NODE_ATTRIBUTE
        and EdgeAtt  : EDGE_ATTRIBUTE

  val augmentGraph : Dot.subgraph * Dot.node list * Dot.edge list
                     -> Dot.subgraph

  (* Create a path from pinned, invisible, intermediate nodes at the given
   * coordinates. The intermediate node names are prefixed: '__nailed:'.
   * The first path segment has additional attributes given by labelatts. *)
  val nailPath     : EdgeAtt.arrowType ->
                     {graph: Dot.subgraph, src: Dot.anchor, dst: Dot.anchor,
                      nails: NodeAtt.pointf list, atts: EdgeAtt.t list,
                      labelatts: EdgeAtt.t list} -> Dot.subgraph
end
=
struct
  structure Dot = TypedDot
        and GraphAtt = TypedAttributes.Graph
        and NodeAtt  = TypedAttributes.Node
        and EdgeAtt  = TypedAttributes.Edge

  fun augmentGraph (Dot.Subgraph {name, attributes, nodeAtts, edgeAtts, nodes,
                                  subgraphs, edges}, newnodes, newedges)
    = Dot.Subgraph {name=name, attributes=attributes,
                    nodeAtts=nodeAtts, edgeAtts=edgeAtts, subgraphs=subgraphs,
                    nodes=newnodes @ nodes, edges=newedges @ edges}

  local
    val pathId = ref 0
    fun freshPathPre () = (String.concat ["__nailed:",
                                          Int.toString (!pathId), ":"])
                          before (pathId := (!pathId) + 1)
  in
  fun nailPath ah {graph, src, dst, nails=[], atts, labelatts} = augmentGraph
      (graph,[],[Dot.Edge {src=src, dst=dst,
                           atts=labelatts @ EdgeAtt.ArrowHead ah::atts}])

    | nailPath ah {graph, src, dst, nails, atts, labelatts} = let
        val pre = freshPathPre ()
        fun anchor i = Dot.NodeId
                         (valOf (Dot.Id.fromString (pre^Int.toString i)))

        val nailatts =[NodeAtt.NoShape, NodeAtt.PlainLabel "",
                       NodeAtt.FixedSize, NodeAtt.Pin]
        fun makeNail (Dot.NodeId id, p) = Dot.Node
                                      {id=id, atts=NodeAtt.Pos p::nailatts}
          | makeNail _ = raise Fail "bad makeNail call."

        val noArrowAtts = EdgeAtt.ArrowHead EdgeAtt.None::atts
        val (clTail, clHead) = (EdgeAtt.NoTailClip, EdgeAtt.NoHeadClip)

        val nailanchors = List.tabulate (length nails, anchor)
        val newnodes = ListPair.map makeNail (nailanchors, nails)

        val newedges = [Dot.Edge {src=src, dst=hd nailanchors,
                                  atts=(labelatts @ clHead::noArrowAtts)},
                        Dot.Path {stops=nailanchors,
                                  atts=clTail::clHead::noArrowAtts},
                        Dot.Edge {src=List.last nailanchors, dst=dst,
                                  atts=EdgeAtt.ArrowHead ah::clTail::atts}]

      in augmentGraph (graph, newnodes, newedges) end
  end

end

