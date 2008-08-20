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

signature EDGE_ATTRIBUTE =
sig
  include ATTRIBUTE
  structure X11Color : X11_COLOR

  type pointf = real * real

  type escString = string   (* but printing is special *)
  type splineType = {endp:    pointf option,
                     startp:  pointf option,
                     point:   pointf,
                     triples: (pointf * pointf * pointf) list} list

  type lblString
  val PlainString    : escString -> lblString (*enclosing dblquotes added*)
  val HTMLString     : string -> lblString    (*no dblquotes added*)
    
  type layer
  val LayerName      : string -> layer
  val LayerNumber    : int -> layer

  type layerRange
  val AllLayers      : layerRange
  val SomeLayers     : layer list -> layerRange

  type color
  val RGB : {red: int, green: int, blue: int} -> color
  val X11 : X11Color.t -> color

  type compass
  val N              : compass
  val NE             : compass
  val E              : compass
  val SE             : compass
  val S              : compass
  val SW             : compass
  val W              : compass
  val NW             : compass
  
  type portPos
  val Port           : string * compass option -> portPos
  val Compass        : compass -> portPos
  
  type edgeStyle
  val Dashed         : edgeStyle
  val Dotted         : edgeStyle
  val Solid          : edgeStyle
  val Invisible      : edgeStyle
  val Bold           : edgeStyle
  val CustomStyle    : string -> edgeStyle
  
  type arrowmod
  val LeftOnly       : arrowmod
  val RightOnly      : arrowmod

  type arrowShape
  val Box            : {side: arrowmod option, filled: bool} -> arrowShape
  val Crow           : {side: arrowmod option} -> arrowShape
  val Diamond        : {side: arrowmod option, filled: bool} -> arrowShape
  val Dot            : {filled: bool} -> arrowShape
  val Inv            : {side: arrowmod option, filled: bool} -> arrowShape
  val Normal         : {side: arrowmod option, filled: bool} -> arrowShape
  val Tee            : {side: arrowmod option} -> arrowShape
  val Vee            : {side: arrowmod option} -> arrowShape

  type arrowType
  val None           : arrowType
  val Single         : arrowShape -> arrowType
  val Double         : arrowShape * arrowShape -> arrowType

  val URL            : lblString -> t   (*svg, postscript, map only*) 
  val ArrowHead      : arrowType -> t
  val ArrowSize      : real -> t
  val ArrowTail      : arrowType -> t
  val Color          : color list -> t
  val ColorScheme    : string -> t
  val Comment        : string -> t
  val Constraint     : t                (*dot only*)
  val NoConstraint   : t                (*dot only*)
  val Decorate       : t
  val NoDecorate     : t
  val Dir_forward    : t
  val Dir_back       : t
  val Dir_both       : t
  val Dir_none       : t
  val EdgeURL        : lblString -> t   (*svg, map only*)
  val EdgeHref       : lblString -> t   (*svg, map only*)
  val EdgeTarget     : escString -> t   (*svg, map only*)
  val EdgeTooltip    : escString -> t   (*svg, cmap only*)
  val FontColor      : color -> t
  val FontName       : string -> t
  val FontSize       : real -> t
  val HeadURL        : lblString -> t   (*svg, map only*)
  val HeadClip       : t
  val NoHeadClip     : t
  val HeadHref       : lblString -> t   (*svg, map only*)
  val HeadLabel      : lblString -> t
  val HeadPort       : portPos -> t
  val HeadTarget     : escString -> t   (*svg, map only*)
  val HeadTooltip    : escString -> t   (*svg, cmap only*)
  val Href           : lblString -> t   (*svg,postscript,map only*)
  val PlainLabel     : escString -> t
  val HTMLLabel      : string -> t
  val LabelURL       : lblString -> t   (*svg, map only*)
  val LabelAngle     : real -> t
  val LabelDistance  : real -> t
  val LabelFloat     : t
  val NoLabelFloat   : t
  val LabelFontColor : color -> t
  val LabelFontName  : string -> t
  val LabelFontSize  : real -> t
  val LabelHref      : lblString -> t   (*svg, map only*)
  val LabelTarget    : escString -> t   (*svg, map only*)
  val LabelTooltip   : escString -> t   (*svg, cmap only*)
  val Layer          : layerRange -> t
  val Len            : real -> t        (*fdp, neato only*)
  val LHead          : string -> t      (*dot only*)
  val LabelPos       : pointf -> t
  val LTail          : string -> t      (*dot only*)
  val MinLen         : int -> t         (*dot only*)
  val Justify        : t
  val NoJustify      : t
  val Pos            : splineType -> t
  val SameHead       : string -> t      (*dot only*)
  val SameTail       : string -> t      (*dot only*)
  val ShowBoxes      : int -> t         (*dot only*)
  val Style          : edgeStyle -> t
  val TailURL        : lblString -> t   (*svg, map only*)
  val TailClip       : t
  val NoTailClip     : t
  val TailHref       : lblString -> t   (*svg, map only*)
  val TailLabel      : lblString -> t
  val TailPort       : portPos -> t
  val TailTarget     : escString -> t   (*svg, map only*)
  val TailTooltip    : escString -> t   (*svg, cmap only*)
  val Target         : escString -> t   (*svg, map only*)
  val Tooltip        : escString -> t   (*svg, cmap only*)
  val Weight         : real -> t

end

