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

signature NODE_ATTRIBUTE =
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

  type layer
  val LayerName          : string -> layer
  val LayerNumber        : int -> layer

  type layerRange
  val AllLayers          : layerRange
  val SomeLayers         : layer list -> layerRange

  type color
  val RGB : {red: int, green: int, blue: int} -> color
  val X11 : X11Color.t -> color

  type nodeStyle
  val Filled             : nodeStyle
  val Invisible          : nodeStyle
  val Diagonals          : nodeStyle
  val Rounded            : nodeStyle
  val Dashed             : nodeStyle
  val Dotted             : nodeStyle
  val Solid              : nodeStyle
  val Bold               : nodeStyle
  val CustomStyle        : string -> nodeStyle

  type recordField
  val Record             : recordField list -> recordField
  val Field              : string option * string option -> recordField

  val URL                : lblString -> t     (*svg, postscript, map only*)
  val Color              : color list -> t
  val ColorScheme        : string -> t
  val Comment            : string -> t
  val Distortion         : real -> t
  val FillColor          : color -> t
  val FixedSize          : t
  val NoFixedSize        : t
  val FontColor          : color -> t
  val FontName           : string -> t
  val FontSize           : real -> t
  val Group              : string -> t        (*dot only*)
  val Height             : real -> t
  val PlainLabel         : string -> t
  val RecordLabel        : recordField list -> t
  val Layer              : layerRange -> t
  val Margin             : spacing -> t
  val Justify            : t
  val NoJustify          : t
  val Orientation        : real -> t
  val Peripheries        : int -> t
  val Pin                : t                  (*fdp, neato only*)
  val NoPin              : t                  (*fdp, neato only*)
  val Pos                : pointf -> t
  val Rects              : rect -> t
  val Regular            : t
  val NotRegular         : t
  val Root               : t                  (*circo, twopi only*)
  val SamplePoints       : int -> t
  val BoxShape           : t
  val PolygonShape       : t
  val EllipseShape       : t
  val CircleShape        : t
  val PointShape         : t
  val EggShape           : t
  val TriangleShape      : t
  val PlainTextShape     : t
  val DiamondShape       : t
  val TrapeziumShape     : t
  val ParallelogramShape : t
  val HouseShape         : t
  val PentagonShape      : t
  val HexagonShape       : t
  val OctagonShape       : t
  val DoubleCircleShape  : t
  val SeptagonShape      : t
  val DoubleOctagonShape : t
  val TripleOctagonShape : t
  val InvTriangleShape   : t
  val InvTrapeziumShape  : t
  val InvHouseShape      : t
  val MDiamondShape      : t
  val MSquareShape       : t
  val MCircleShape       : t
  val NoShape            : t
  val RecordShape        : t
  val MRecordShape       : t
  val ShapeFile          : string -> t
  val ShowBoxes          : int -> t           (*dot only*)
  val Sides              : int -> t
  val Skew               : real -> t
  val Style              : nodeStyle list -> t
  val Target             : escString -> t     (*svg, map only*)
  val Tooltip            : escString -> t     (*svg, cmap only*)
  val Vertices           : pointf list -> t
  val Width              : real -> t
  val Z                  : real -> t

end

