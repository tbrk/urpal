(* $Id$

   20070822 T. Bourke
     Basic implementation of DOT files.
 *)

functor DotFn (
  structure Id       : ID
  structure EdgeAtt  : ATTRIBUTE
  structure NodeAtt  : ATTRIBUTE
  structure GraphAtt : ATTRIBUTE
  ) : DOT
=
struct
  structure EdgeAtt  = EdgeAtt
        and NodeAtt  = NodeAtt
        and GraphAtt = GraphAtt
        and Id       = Id

  datatype compassPt = N | NE | E | SE | S | SW | W | NW

  datatype anchor = NodeId of Id.t
                  | SubgraphId of Id.t
                  | PortId of Id.t * Id.t * compassPt option
  
  datatype node = Node of {id:   Id.t,
                           atts: NodeAtt.t list}

  datatype edge = Edge of {src:  anchor,
                           dst:  anchor,
                           atts: EdgeAtt.t list}
                | Path of {stops: anchor list,
                           atts: EdgeAtt.t list}

  datatype subgraph = Subgraph of {name:       Id.t option,

                                   attributes: GraphAtt.t list,
                                   nodeAtts:   NodeAtt.t list,
                                   edgeAtts:   EdgeAtt.t list,

                                   nodes:      node list,
                                   subgraphs:  subgraph list,
                                   edges:      edge list}

  datatype graph = Graph of {strict:     bool,
                             directed:   bool,
                             graph:      subgraph}

end

