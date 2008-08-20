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
 *
 *   Basic datatypes for DOT files.
 *
 *   Simplifications:
 *      * no anonymous subgraphs in edges.
 *        (not possible: n0 -> subgraph {...})
 *      * no explicit types for cluster or subgraph attributes.
 *)

signature DOT =
sig
  structure EdgeAtt  : ATTRIBUTE
        and NodeAtt  : ATTRIBUTE
        and GraphAtt : ATTRIBUTE (* includes subgraphs and clusters *)
        and Id       : ID

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

