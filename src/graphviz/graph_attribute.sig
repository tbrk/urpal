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

signature GRAPH_ATTRIBUTE =
sig
  include ATTRIBUTE
  structure X11Color : X11_COLOR

  type pointf = real * real

  type escString = string   (* but printing is special *)
  type rect = {llx: int, lly: int, urx: int, ury: int}

  type lblString
  val PlainString        : escString -> lblString (*enclosing dblquotes added*)
  val HTMLString         : string -> lblString    (*no dblquotes added*)
    
  type spacing
  val SpacingEqual       : real -> spacing
  val SpacingDifferent   : real * real -> spacing

  type color
  val RGB : {red: int, green: int, blue: int} -> color
  val X11 : X11Color.t -> color

  type viewport = {width: real, height: real, zoom: real option,
                   center: pointf option}
  
  type outputMode
  val BreadthFirst            : outputMode
  val NodesFirst              : outputMode
  val EdgesFirst              : outputMode

  type ratio
  val RatioNumeric            : real -> ratio
  val RatioFill               : ratio
  val RatioCompress           : ratio
  val RatioExpand             : ratio
  val RatioAuto               : ratio

  type pageDir
  val Page_BL                 : pageDir
  val Page_BR                 : pageDir
  val Page_TL                 : pageDir
  val Page_TR                 : pageDir
  val Page_RB                 : pageDir
  val Page_RT                 : pageDir
  val Page_LB                 : pageDir
  val Page_LT                 : pageDir

  type rankDir
  val Rank_TB                 : rankDir
  val Rank_LR                 : rankDir
  val Rank_BT                 : rankDir
  val Rank_RL                 : rankDir

  type startStyle
  val Regular                 : startStyle
  val Self                    : startStyle
  val Random                  : startStyle

  type clusterStyle
  val Filled                  : clusterStyle
  val Rounded                 : clusterStyle
  val Custom                  : string -> clusterStyle

  type overlap
  val OL_True                 : overlap
  val OL_False                : overlap
  val OL_Scale                : overlap
  val OL_ScaleXY              : overlap
  val OL_Ortho                : overlap
  val OL_OrthoXY              : overlap
  val OL_OrthYX               : overlap
  val OL_POrtho               : overlap
  val OL_POrthoXY             : overlap
  val OL_POrthoYX             : overlap
  val OL_Compress             : overlap
  val OL_IPSep                : overlap
  val OL_VPSC                 : overlap
  val OL_FDP                  : int * overlap -> overlap

  val Damping                 : real -> t       (*neato only*)
  val K                       : real -> t       (*fdp only*)
  val URL                     : lblString -> t  (*svg, postscript, map only*)
  val Bb                      : rect -> t
  val BgColor                 : color -> t
  val Center                  : t
  val NoCenter                : t
  val Charset                 : string -> t
  val ClusterRank_local       : t
  val ClusterRank_global      : t
  val ClusterRank_none        : t
  val Color                   : color list -> t
  val ColorScheme             : string -> t
  val Comment                 : string -> t
  val Compound                : t               (*dot only*)
  val NoCompound              : t               (*dot only*)
  val Concentrate             : t               (*dot only*)
  val NoConcentrate           : t               (*dot only*)
  val DefaultDist             : real -> t       (*neato only*)
  val Dim                     : int -> t        (*fdp, neato only*)
  val DirEdgeConstraints_true : t               (*neato only*)
  val DirEdgeConstraints_none : t               (*neato only*)
  val DirEdgeConstraints_hier : t               (*neato only*)
  val Dpi                     : real -> t       (*svg, bitmap output only*)
  val Epsilon                 : real -> t       (*neato only*)
  val Esep                    : real -> t       (*not dot*)
  val FillColor               : color -> t
  val FontColor               : color -> t
  val FontName                : string -> t
  val FontNames               : string -> t     (*svg only*)
  val FontPath                : string -> t
  val FontSize                : real -> t
  val Label                   : lblString -> t
  val LabelJust               : string -> t
  val LabelLoc_top            : t
  val LabelLoc_bottom         : t
  val Landscape               : t
  val NoLandscape             : t
  val Layers                  : string list -> t
  val LayerSep                : string -> t
  val LevelsGap               : real -> t       (*neato only*)
  val LabelPos                : pointf -> t
  val Margin                  : spacing -> t
  val MaxIter                 : int -> t        (*fdp, neato only*)
  val McLimit                 : real -> t       (*dot only*)
  val MinDist                 : real -> t       (*circo only*)
  val Mode                    : string -> t     (*neato only*)
  val Model                   : string -> t     (*neato only*)
  val Mosek                   : t               (*neato only*)
  val NodeSep                 : real -> t       (*dot only*)
  val Justify                 : t
  val NoJustify               : t
  val Normalize               : t               (*not dot*)
  val NoNormalize             : t               (*not dot*)
  val NsLimit                 : real -> t       (*dot only*)
  val NsLimit1                : real -> t       (*dot only*)
  val Ordering                : string -> t     (*dot only*)
  val Orientation             : string -> t
  val OutputOrder             : outputMode -> t
  val Overlap                 : overlap -> t    (*not dot*)
  val PackTrue                : t               (*not dot*)
  val PackFalse               : t               (*not dot*)
  val PackValue               : int -> t        (*not dot*)
  val PackMode_node           : t               (*not dot*)
  val PackMode_cluster        : t               (*not dot*)
  val PackMode_graph          : t               (*not dot*)
  val Pad                     : spacing -> t
  val Page                    : pointf -> t
  val PageDir                 : pageDir -> t
  val PenColor                : color -> t
  val Peripheries             : int -> t
  val Quantum                 : real -> t
  val Rank_same               : t               (*dot only*)
  val Rank_min                : t               (*dot only*)
  val Rank_source             : t               (*dot only*)
  val Rank_max                : t               (*dot only*)
  val Rank_sink               : t               (*dot only*)
  val RankDir                 : rankDir -> t    (*dot only*)
  val RankSep                 : real -> t       (*twopi, dot only*)
  val Ratio                   : ratio -> t
  val ReMinCross              : t               (*dot only*)
  val NoReMinCross            : t               (*dot only*)
  val Resolution              : real -> t       (*svg, bitmap output only*)
  val Root                    : string -> t     (*circo, twopi only*)
  val Rotate                  : int -> t
  val SearchSize              : int -> t        (*dot only*)
  val Sep                     : spacing -> t
  val ShowBoxes               : int -> t        (*dot only*)
  val Size                    : pointf -> t
  val Splines_true            : t
  val Splines_line            : t
  val Splines_none            : t
  val Splines_compound        : t               (*fdp only*)
  val Start                   : startStyle option * int option -> t
                                                (*fdp, neato only*)
  val Style                   : clusterStyle -> t
  val StyleSheet              : string -> t     (*svg only*)
  val Target                  : escString -> t  (*svg, map only*)
  val Tooltip                 : escString -> t  (*svg, cmap only*)
  val TrueColor               : bool -> t       (*bitmap output only*)
  val Viewport                : viewport -> t   (*bitmap output,cmap,map only*)
  val Voro_Margin             : real -> t

end

