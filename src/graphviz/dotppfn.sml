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
 * Pretty printer for DOT files.
 *)

functor DotPPFn (
          structure PPStream : PP_STREAM
          structure Dot      : DOT
        ) :
sig
  val output : PPStream.stream * Dot.graph -> unit
end
=
let structure S = PPStream
in
struct
  structure PPD = PPDescFn (S)

  structure EdgeAttPP  = ShowAttFn (val name = "edge";
                                    structure Att = Dot.EdgeAtt  and PPD = PPD)
  structure NodeAttPP  = ShowAttFn (val name = "node";
                                    structure Att = Dot.NodeAtt  and PPD = PPD)
  structure GraphAttPP = ShowAttFn (val name = "graph";
                                    structure Att = Dot.GraphAtt and PPD = PPD)

  val noIndent = S.Abs 0
  val relIndent = S.Rel 0
  val medIndent = S.Abs 4

  fun nbSpace strm = S.nbSpace strm 1
  fun space strm = S.space strm 1

  fun quote str = let
      fun firstNotDigit s = (size s = 0 orelse
                             not (Char.isDigit (String.sub (s, 0))))
      fun isIdChar c = Char.isAlphaNum c orelse c = #"_"
      fun f c = if c = #"\"" then "\\\"" else Char.toCString c
    in
      if CharVector.all isIdChar str andalso firstNotDigit str then str
      else"\"" ^ String.translate f str ^ "\""
    end

  fun showId strm id = S.string strm (quote (Dot.Id.toString id))

  fun showCompass strm c = S.string strm (case c
      of Dot.N  => "N" | Dot.NE => "NE" | Dot.E => "E" | Dot.SE => "SE"
       | Dot.S  => "S" | Dot.SW => "SW" | Dot.W => "W" | Dot.NW => "NW")

  fun showAnchor strm a = case a
      of Dot.NodeId id         => showId strm id
       | Dot.SubgraphId id     => (S.string strm "subgraph";
                                   space strm; showId strm id)
       | Dot.PortId (id, port, NONE)    => (showId strm id; S.string strm ":";
                                            showId strm port)
       | Dot.PortId (id, port, SOME cp) => (showId strm id; S.string strm ":";
                                            showId strm id; S.string strm ":";
                                            showCompass strm cp)

  fun showNode strm (Dot.Node {id, atts}) = let
    in  (*{{{1*)
      space strm;
      showId strm id;
      NodeAttPP.showBoxedList strm atts
    end (*}}}1*)


  fun showEdge directed strm e = let
    (*{{{1*)
      fun addSource src = (showAnchor strm src)

      fun addAnchor anc = (S.nbSpace strm 1;
                           S.string strm (if directed then "->" else "--");
                           space strm;
                           S.openHBox strm; showAnchor strm anc; S.closeBox strm)
      
      fun addRest (Dot.Edge {src, dst, atts}) =
              (addSource src; addAnchor dst; EdgeAttPP.showBoxedList strm atts)

        | addRest (Dot.Path {stops=s1::s2::ss, atts}) = (addSource s1;
                     app addAnchor (s2::ss); EdgeAttPP.showBoxedList strm atts)

        | addRest (Dot.Path _) = ()
    in
      S.cut strm; S.openHOVBox strm (S.Rel 4); addRest e; S.closeBox strm
    end (*}}}1*)

  fun showSubgraph directed strm (Dot.Subgraph {name, attributes, nodeAtts,
                                                edgeAtts, nodes, subgraphs,
                                                edges}) =
    let  (*{{{1*)
      fun showSub sg = (space strm;
                        S.openHBox strm;
                          S.string strm "subgraph";
                          showSubgraph directed strm sg;
                        S.closeBox strm)
    in
      space strm;
      if isSome name then (showId strm (valOf name); nbSpace strm) else ();
      S.string strm "{";

      S.openVBox strm (S.Abs 4);
        S.cut strm;
        GraphAttPP.showList strm attributes;
        NodeAttPP.showNamedList strm nodeAtts;
        EdgeAttPP.showNamedList strm edgeAtts;

        app (showNode strm) nodes;
        app showSub subgraphs;
        app (showEdge directed strm) edges;
      S.closeBox strm; S.newline strm;

      S.string strm "}"
    end (*}}}1*)

  fun output (strm, Dot.Graph {strict, directed, graph}) = let
    in  (*{{{1*)
      S.openHBox strm;
        if strict then (S.string strm "strict"; nbSpace strm) else ();
        if directed then S.string strm "digraph" else S.string strm "graph";
        showSubgraph directed strm graph;
      S.closeBox strm;
      S.newline strm
    end (*}}}1*)

end (* struct *)
end (* let *)

