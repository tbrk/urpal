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
 * Quick and dirty attempt at a typed attribute scheme for GraphViz.
 *
 * Based on the documentation for GraphViz v2.14:
 *   http://www.graphviz.org/doc/info/attrs.html
 *
 * Caveat programmator. Known limitations:
 *  - not extensively tested, used or debugged.
 *  - no '!' modifiers for points
 *)

structure TypedAttributes =
struct
  structure Common =
  struct (*{{{1*)
    type pointf = real * real

    type escString = string   (* but printing is special *)

    datatype lblString = PlainString of escString (*enclosing dblquotes added*)
                       | HTMLString of string      (*no dblquotes added*)
      
    type rect = {llx: int, lly: int, urx: int, ury: int}

    structure X11Color = X11Color
    datatype color = RGB of {red: int, green: int, blue: int}
                   | X11 of X11Color.t

    type splineType = {endp:    pointf option,
                       startp:  pointf option,
                       point:   pointf,
                       triples: (pointf * pointf * pointf) list} list

    datatype spacing = SpacingEqual of real | SpacingDifferent of real * real

    datatype layer = LayerName of string | LayerNumber of int
    datatype layerRange = AllLayers | SomeLayers of layer list

    fun dbl s = "\""^s^"\""
    fun makeList fmt = ListFormat.fmt {init="\"", final="\"", sep=":", fmt=fmt}
    fun showInt i = if i < 0
                    then "-" ^ Int.toString (Int.abs i)
                    else Int.toString i
    fun showReal r = if Real.sign r = ~1
                     then "-" ^ Real.toString (Real.abs r)
                     else Real.toString r

    fun showString s = "\""^String.toCString s^"\""      (* expand \n to \\n *)
    fun showPointf (x, y) = "\""^ showReal x ^","^ showReal y ^"\""

    fun lblStringToString (PlainString s) = showString s
      | lblStringToString (HTMLString s)  = s

    fun rectToString {llx, lly, urx, ury} = String.concat
                    ["\"", showInt llx, ",", showInt lly, ",",
                           showInt urx, ",", showInt ury, "\""]

    fun colorToString (RGB {red, green, blue}) = let
          fun dd i = (if i < 16 then "0" else "")^Int.fmt StringCvt.HEX i
        in String.concat ["#", dd red, dd green, dd blue] end
      | colorToString (X11 xc) = X11Color.toString xc

    fun splineTypeToString splines = let
      fun f {endp, startp, point, triples} =
        let
          fun seStr (c, NONE) = ""
            | seStr (c, SOME (x, y)) = c ^","^ showReal x ^","^showReal y

          fun ptStr (x, y) = showReal x ^","^ showReal y^" "
          fun tripleStr (p1, p2, p3) = ptStr p1 ^ ptStr p2 ^ ptStr p3

          val (endPStr, startPStr) = (seStr ("e", endp), seStr ("s", startp))
          val tripleStr = ListFormat.fmt {init="", final="",
                                          sep=" ", fmt=tripleStr} triples
        in String.concat [endPStr,startPStr,ptStr point,tripleStr] end

      in ListFormat.fmt {init="\"", final="\"", sep=";", fmt=f} splines end
      
    fun layerToString (LayerName s)   = "\""^s^"\""
      | layerToString (LayerNumber i) = showInt i

    fun layerRangeToString AllLayers           = "\"all\""
      | layerRangeToString (SomeLayers layers) = makeList layerToString layers

    fun spacingToString (SpacingEqual r)          = showReal r
      | spacingToString (SpacingDifferent (x, y)) = "\""^showReal x
                                                  ^ ","^ showReal y^"\""
  end (*}}}1*)

  structure Graph :> GRAPH_ATTRIBUTE =
  struct
    open Common

    type viewport = {width: real, height: real, zoom: real option,
                     center: pointf option}
    
    datatype outputMode   = BreadthFirst | NodesFirst | EdgesFirst

    datatype ratio        = RatioNumeric of real
                          | RatioFill | RatioCompress | RatioExpand | RatioAuto

    datatype pageDir      = Page_BL | Page_BR | Page_TL | Page_TR
                          | Page_RB | Page_RT | Page_LB | Page_LT

    datatype rankDir      = Rank_TB | Rank_LR | Rank_BT | Rank_RL

    datatype startStyle   = Regular | Self | Random

    datatype clusterStyle = Filled | Rounded | Custom of string

    datatype overlap      = OL_True     | OL_False 
                          | OL_Scale    | OL_ScaleXY
                          | OL_Ortho    | OL_OrthoXY  | OL_OrthYX
                          | OL_POrtho   | OL_POrthoXY | OL_POrthoYX
                          | OL_Compress | OL_IPSep    | OL_VPSC
                          | OL_FDP of int * overlap

    datatype t = Damping of real         (*neato only*)
    (*{{{1*)   | K of real               (*fdp only*)
               | URL of lblString        (*svg, postscript, map only*)
               | Bb of rect
               | BgColor of color
               | Center
               | NoCenter
               | Charset of string
               | ClusterRank_local
               | ClusterRank_global
               | ClusterRank_none
               | Color of color list
               | ColorScheme of string
               | Comment of string
               | Compound                (*dot only*)
               | NoCompound              (*dot only*)
               | Concentrate             (*dot only*)
               | NoConcentrate           (*dot only*)
               | DefaultDist of real     (*neato only*)
               | Dim of int              (*fdp, neato only*)
               | DirEdgeConstraints_true (*neato only*)
               | DirEdgeConstraints_none (*neato only*)
               | DirEdgeConstraints_hier (*neato only*)
               | Dpi of real             (*svg, bitmap output only*)
               | Epsilon of real         (*neato only*)
               | Esep of real            (*not dot*)
               | FillColor of color
               | FontColor of color
               | FontName of string
               | FontNames of string     (*svg only*)
               | FontPath of string
               | FontSize of real
               | Label of lblString
               | LabelJust of string
               | LabelLoc_top
               | LabelLoc_bottom
               | Landscape
               | NoLandscape
               | Layers of string list
               | LayerSep of string
               | LevelsGap of real       (*neato only*)
               | LabelPos of pointf
               | Margin of spacing
               | MaxIter of int          (*fdp, neato only*)
               | McLimit of real         (*dot only*)
               | MinDist of real         (*circo only*)
               | Mode of string          (*neato only*)
               | Model of string         (*neato only*)
               | Mosek                   (*neato only*)
               | NodeSep of real         (*dot only*)
               | Justify
               | NoJustify
               | Normalize               (*not dot*)
               | NoNormalize             (*not dot*)
               | NsLimit of real         (*dot only*)
               | NsLimit1 of real        (*dot only*)
               | Ordering of string      (*dot only*)
               | Orientation of string
               | OutputOrder of outputMode
               | Overlap of overlap      (*not dot*)
               | PackFalse               (*not dot*)
               | PackTrue                (*not dot*)
               | PackValue of int        (*not dot*)
               | PackMode_node           (*not dot*)
               | PackMode_cluster        (*not dot*)
               | PackMode_graph          (*not dot*)
               | Pad of spacing
               | Page of pointf
               | PageDir of pageDir
               | PenColor of color
               | Peripheries of int
               | Quantum of real
               | Rank_same               (*dot only*)
               | Rank_min                (*dot only*)
               | Rank_source             (*dot only*)
               | Rank_max                (*dot only*)
               | Rank_sink               (*dot only*)
               | RankDir of rankDir      (*dot only*)
               | RankSep of real         (*twopi, dot only*)
               | Ratio of ratio
               | ReMinCross              (*dot only*)
               | NoReMinCross            (*dot only*)
               | Resolution of real      (*svg, bitmap output only*)
               | Root of string          (*circo, twopi only*)
               | Rotate of int
               | SearchSize of int       (*dot only*)
               | Sep of spacing
               | ShowBoxes of int        (*dot only*)
               | Size of pointf
               | Splines_true
               | Splines_line
               | Splines_none
               | Splines_compound
               | Start of startStyle option * int option (*fdp, neato only*)
               | Style of clusterStyle
               | StyleSheet of string    (*svg only*)
               | Target of escString     (*svg, map only*)
               | Tooltip of escString    (*svg, cmap only*)
               | TrueColor of bool       (*bitmap output only*)
               | Viewport of viewport    (*bitmap output, cmap, map only*)
       (*}}}1*)| Voro_Margin of real

    fun name (Damping _)        = "Damping"
      (*{{{1*)
      | name (K _)              = "K"
      | name (URL _)            = "URL"
      | name (Bb _)             = "bb"
      | name (BgColor _)        = "bgcolor"
      | name (Center)           = "center"
      | name (NoCenter)         = "center"
      | name (Charset _)        = "charset"
      | name (ClusterRank_local)= "clusterrank"
      | name (ClusterRank_global)= "clusterrank"
      | name (ClusterRank_none) = "clusterrank"
      | name (Color _)          = "color"
      | name (ColorScheme _)    = "colorscheme"
      | name (Comment _)        = "comment"
      | name (Compound)         = "compound"
      | name (NoCompound)       = "compound"
      | name (Concentrate)      = "concentrate"
      | name (NoConcentrate)    = "concentrate"
      | name (DefaultDist _)    = "defaultdist"
      | name (Dim _)            = "dim"
      | name (DirEdgeConstraints_true) = "diredgeconstraints"
      | name (DirEdgeConstraints_none) = "diredgeconstraints"
      | name (DirEdgeConstraints_hier) = "diredgeconstraints"
      | name (Dpi _)            = "dpi"
      | name (Epsilon _)        = "epsilon"
      | name (Esep _)           = "esep"
      | name (FillColor _)      = "fillcolor"
      | name (FontColor _)      = "fontcolor"
      | name (FontName _)       = "fontname"
      | name (FontNames _)      = "fontnames"
      | name (FontPath _)       = "fontpath"
      | name (FontSize _)       = "fontsize"
      | name (Label _)          = "label"
      | name (LabelJust _)      = "labeljust"
      | name (LabelLoc_top)     = "labelloc"
      | name (LabelLoc_bottom)  = "labelloc"
      | name (Landscape)        = "landscape"
      | name (NoLandscape)      = "landscape"
      | name (Layers _)         = "layers"
      | name (LayerSep _)       = "layersep"
      | name (LevelsGap _)      = "levelsgap"
      | name (LabelPos _)       = "lp"
      | name (Margin _)         = "margin"
      | name (MaxIter _)        = "maxiter"
      | name (McLimit _)        = "mclimit"
      | name (MinDist _)        = "mindist"
      | name (Mode _)           = "mode"
      | name (Model _)          = "model"
      | name (Mosek)            = "mosek"
      | name (NodeSep _)        = "nodesep"
      | name (Justify)          = "nojustify"
      | name (NoJustify)        = "nojustify"
      | name (Normalize)        = "normalize"
      | name (NoNormalize)      = "normalize"
      | name (NsLimit _)        = "nslimit"
      | name (NsLimit1 _)       = "nslimit1"
      | name (Ordering _)       = "ordering"
      | name (Orientation _)    = "orientation"
      | name (OutputOrder _)    = "outputorder"
      | name (Overlap _)        = "overlap"
      | name (PackFalse)        = "pack"
      | name (PackTrue)         = "pack"
      | name (PackValue _)      = "pack"
      | name (PackMode_node)    = "packmode"
      | name (PackMode_cluster) = "packmode"
      | name (PackMode_graph)   = "packmode"
      | name (Pad _)            = "pad"
      | name (Page _)           = "page"
      | name (PageDir _)        = "pagedir"
      | name (PenColor _)       = "pencolor"
      | name (Peripheries _)    = "peripheries"
      | name (Quantum _)        = "quantum"
      | name (Rank_same)        = "rank"
      | name (Rank_min)         = "rank"
      | name (Rank_source)      = "rank"
      | name (Rank_max)         = "rank"
      | name (Rank_sink)        = "rank"
      | name (RankDir _)        = "rankdir"
      | name (RankSep _)        = "ranksep"
      | name (Ratio _)          = "ratio"
      | name (ReMinCross)       = "remincross"
      | name (NoReMinCross)     = "remincross"
      | name (Resolution _)     = "resolution"
      | name (Root _)           = "root"
      | name (Rotate _)         = "rotate"
      | name (SearchSize _)     = "searchsize"
      | name (Sep _)            = "sep"
      | name (ShowBoxes _)      = "showboxes"
      | name (Size _)           = "size"
      | name (Splines_true)     = "splines"
      | name (Splines_line)     = "splines"
      | name (Splines_none)     = "splines"
      | name (Splines_compound) = "splines"
      | name (Start _)          = "start"
      | name (Style _)          = "style"
      | name (StyleSheet _)     = "stylesheet"
      | name (Target _)         = "target"
      | name (Tooltip _)        = "tooltip"
      | name (TrueColor _)      = "truecolor"
      | name (Viewport _)       = "viewport"
      | name (Voro_Margin _)    = "voro_margin"
      (*}}}1*)

    (* Auxilliary display functions *)
    (* {{{1*)
    fun outputModeToString BreadthFirst = "breadthfirst"
      | outputModeToString NodesFirst   = "nodesfirst"
      | outputModeToString EdgesFirst   = "edgesfirst"

    fun ratioToString (RatioNumeric r) = showReal r
      | ratioToString RatioFill        = "\"fill\""
      | ratioToString RatioCompress    = "\"compress\""
      | ratioToString RatioExpand      = "\"expand\""
      | ratioToString RatioAuto        = "\"auto\""

    fun pageDirToString Page_BL = "BL" | pageDirToString Page_BR = "BR"
      | pageDirToString Page_TL = "TL" | pageDirToString Page_TR = "TR"
      | pageDirToString Page_RB = "RB" | pageDirToString Page_RT = "RT"
      | pageDirToString Page_LB = "LB" | pageDirToString Page_LT = "LT"

    fun rankDirToString Rank_TB = "TB" | rankDirToString Rank_LR = "LR"
      | rankDirToString Rank_BT = "BT" | rankDirToString Rank_RL = "RL"

    fun overlapToString OL_True     = "true"
      | overlapToString OL_False    = "false"             (*{{{2*)
      | overlapToString OL_Scale    = "scale"
      | overlapToString OL_ScaleXY  = "scalexy"
      | overlapToString OL_Ortho    = "ortho"
      | overlapToString OL_OrthoXY  = "orthoxy"
      | overlapToString OL_OrthYX   = "orthoyx"
      | overlapToString OL_POrtho   = "portho"
      | overlapToString OL_POrthoXY = "porthoxy"
      | overlapToString OL_POrthoYX = "porthyyx"
      | overlapToString OL_Compress = "compress"
      | overlapToString OL_IPSep    = "ipsep"
      | overlapToString OL_VPSC     = "vpsc"
      | overlapToString (OL_FDP (i, ol)) = showInt i^":"^overlapToString ol
      (*}}}2*)

    fun startStyleToString Regular = "regular"
      | startStyleToString Self    = "self"
      | startStyleToString Random  = "random"

    fun startToString (NONE, NONE)     = "\"regular\""
      | startToString (SOME s, NONE)   = (dbl o startStyleToString) s
      | startToString (NONE, SOME i)   = showInt i
      | startToString (SOME s, SOME i) = "\""^startStyleToString s
                                         ^" "^showInt i^"\""

    fun clusterStyleToString Filled     = "filled"
      | clusterStyleToString Rounded    = "rounded"
      | clusterStyleToString (Custom s) = s

    fun viewportToString {width, height, zoom, center} =
        let
          fun showCenter NONE          = ""
            | showCenter (SOME (x, y)) = ","^showReal x^","^showReal y
        in
          String.concat ["\"", showReal width, ",", showReal height, ",",
                         showReal (Option.getOpt (zoom, 1.0)),
                         showCenter center, "\""]
        end
    (*}}}1*)

    fun value (Damping r)       = showReal r            (*G*)
      (*{{{1*)
      | value (K r)             = showReal r
      | value (URL ls)          = lblStringToString ls
      | value (Bb r)            = rectToString r
      | value (BgColor c)       = (dbl o colorToString) c
      | value (Center)          = "true"
      | value (NoCenter)        = "false"
      | value (Charset s)       = showString s
      | value ClusterRank_local = "\"local\""
      | value ClusterRank_global= "\"global\""
      | value ClusterRank_none  = "\"none\""
      | value (Color cs)        = makeList colorToString cs
      | value (ColorScheme s)   = showString s
      | value (Comment s)       = showString s
      | value (Compound)        = "true"
      | value (NoCompound)      = "false"
      | value (Concentrate)     = "true"
      | value (NoConcentrate)   = "false"
      | value (DefaultDist r)   = showReal r
      | value (Dim i)           = showInt i
      | value (DirEdgeConstraints_true)= "true"
      | value (DirEdgeConstraints_none)= "\"none\""
      | value (DirEdgeConstraints_hier)= "\"hier\""
      | value (Dpi r)           = showReal r
      | value (Epsilon r)       = showReal r
      | value (Esep r)          = showReal r
      | value (FillColor c)     = (dbl o colorToString) c
      | value (FontColor c)     = (dbl o colorToString) c
      | value (FontName s)      = showString s
      | value (FontNames s)     = showString s
      | value (FontPath s)      = showString s
      | value (FontSize r)      = showReal r
      | value (Label ls)        = lblStringToString ls
      | value (LabelJust s)     = showString s
      | value (LabelLoc_top)    = "t"
      | value (LabelLoc_bottom) = "b"
      | value (Landscape)       = "true"
      | value (NoLandscape)     = "false"
      | value (Layers ls)       = makeList (fn f=>f) ls
      | value (LayerSep s)      = showString s
      | value (LevelsGap r)     = showReal r
      | value (LabelPos p)      = showPointf p
      | value (Margin sp)       = spacingToString sp
      | value (MaxIter i)       = showInt i
      | value (McLimit r)       = showReal r
      | value (MinDist r)       = showReal r
      | value (Mode s)          = showString s
      | value (Model s)         = showString s
      | value (Mosek)           = "true"
      | value (NodeSep r)       = showReal r
      | value (Justify)         = "false"
      | value (NoJustify)       = "true"
      | value (Normalize)       = "true"
      | value (NoNormalize)     = "false"
      | value (NsLimit r)       = showReal r
      | value (NsLimit1 r)      = showReal r
      | value (Ordering s)      = showString s
      | value (Orientation s)   = showString s
      | value (OutputOrder om)  = (dbl o outputModeToString) om
      | value (Overlap ol)      = overlapToString ol
      | value (PackTrue)        = "true"
      | value (PackFalse)       = "false"
      | value (PackValue i)     = showInt i
      | value (PackMode_node)   = "\"node\""
      | value (PackMode_cluster)= "\"cluster\""
      | value (PackMode_graph)  = "\"graph\""
      | value (Pad sp)          = spacingToString sp
      | value (Page p)          = showPointf p
      | value (PageDir pd)      = (dbl o pageDirToString) pd
      | value (PenColor c)      = (dbl o colorToString) c
      | value (Peripheries i)   = showInt i
      | value (Quantum r)       = showReal r
      | value (Rank_same)       = "\"same\""
      | value (Rank_min)        = "\"min\""
      | value (Rank_source)     = "\"source\""
      | value (Rank_max)        = "\"max\""
      | value (Rank_sink)       = "\"sink\""
      | value (RankDir rd)      = (dbl o rankDirToString) rd
      | value (RankSep r)       = showReal r
      | value (Ratio r)         = ratioToString r
      | value (ReMinCross)      = "true"
      | value (NoReMinCross)    = "false"
      | value (Resolution r)    = showReal r
      | value (Root s)          = showString s
      | value (Rotate i)        = showInt i
      | value (SearchSize i)    = showInt i
      | value (Sep sp)          = spacingToString sp
      | value (ShowBoxes i)     = showInt i
      | value (Size p)          = showPointf p
      | value (Splines_true)    = "true"
      | value (Splines_line)    = "\"line\""
      | value (Splines_none)    = "\"none\""
      | value (Splines_compound)= "\"compound\""
      | value (Start args)      = startToString args
      | value (Style s)         = (dbl o clusterStyleToString) s
      | value (StyleSheet s)    = showString s
      | value (Target e)        = showString e
      | value (Tooltip e)       = showString e
      | value (TrueColor b)     = Bool.toString b
      | value (Viewport vp)     = viewportToString vp
      | value (Voro_Margin r)   = showReal r
      (*}}}1*)

    fun hasValue _ = true
  end

  structure Edge :> EDGE_ATTRIBUTE =
  struct
    open Common

    datatype compass    = N | NE | E | SE | S | SW | W | NW
    
    datatype portPos    = Port of string * compass option
                        | Compass of compass
    
    datatype edgeStyle  = Dashed | Dotted | Solid | Invisible
                        | Bold   | CustomStyle of string
    
    datatype arrowmod   = LeftOnly | RightOnly
    datatype arrowShape = Box     of {side: arrowmod option, filled: bool}
                        | Crow    of {side: arrowmod option}
                        | Diamond of {side: arrowmod option, filled: bool}
                        | Dot     of {filled: bool}
                        | Inv     of {side: arrowmod option, filled: bool}
                        | Normal  of {side: arrowmod option, filled: bool}
                        | Tee     of {side: arrowmod option}
                        | Vee     of {side: arrowmod option}
    datatype arrowType  = None
                        | Single of arrowShape
                        | Double of arrowShape * arrowShape

    datatype t = URL of lblString           (*svg, postscript, map only*) 
    (*{{{1*)   | ArrowHead of arrowType
               | ArrowSize of real
               | ArrowTail of arrowType
               | Color of color list
               | ColorScheme of string
               | Comment of string
               | Constraint                 (*dot only*)
               | NoConstraint               (*dot only*)
               | Decorate
               | NoDecorate
               | Dir_forward
               | Dir_back
               | Dir_both
               | Dir_none
               | EdgeURL of lblString       (*svg, map only*)
               | EdgeHref of lblString      (*svg, map only*)
               | EdgeTarget of escString    (*svg, map only*)
               | EdgeTooltip of escString   (*svg, cmap only*)
               | FontColor of color
               | FontName of string
               | FontSize of real
               | HeadURL of lblString       (*svg, map only*)
               | HeadClip
               | NoHeadClip
               | HeadHref of lblString      (*svg, map only*)
               | HeadLabel of lblString
               | HeadPort of portPos
               | HeadTarget of escString    (*svg, map only*)
               | HeadTooltip of escString   (*svg, cmap only*)
               | Href of lblString          (*svg, postscript, map only*)
               | Label of lblString
               | LabelURL of lblString      (*svg, map only*)
               | LabelAngle of real
               | LabelDistance of real
               | LabelFloat
               | NoLabelFloat
               | LabelFontColor of color
               | LabelFontName of string
               | LabelFontSize of real
               | LabelHref of lblString     (*svg, map only*)
               | LabelTarget of escString   (*svg, map only*)
               | LabelTooltip of escString  (*svg, cmap only*)
               | Layer of layerRange
               | Len of real                (*fdp, neato only*)
               | LHead of string            (*dot only*)
               | LabelPos of pointf
               | LTail of string            (*dot only*)
               | MinLen of int              (*dot only*)
               | Justify
               | NoJustify
               | Pos of splineType
               | SameHead of string         (*dot only*)
               | SameTail of string         (*dot only*)
               | ShowBoxes of int           (*dot only*)
               | Style of edgeStyle
               | TailURL of lblString       (*svg, map only*)
               | TailClip
               | NoTailClip
               | TailHref of lblString      (*svg, map only*)
               | TailLabel of lblString
               | TailPort of portPos
               | TailTarget of escString    (*svg, map only*)
               | TailTooltip of escString   (*svg, cmap only*)
               | Target of escString        (*svg, map only*)
               | Tooltip of escString       (*svg, cmap only*)
               | Weight of real
        (*}}}1*)

    fun HTMLLabel s  = Label (HTMLString s)
    fun PlainLabel s = Label (PlainString s)

    fun name (URL _)            = "URL"
      (*{{{1*)
      | name (ArrowHead _)      = "arrowhead"
      | name (ArrowSize _)      = "arrowsize"
      | name (ArrowTail _)      = "arrowtail"
      | name (Color _)          = "color"
      | name (ColorScheme _)    = "colorscheme"
      | name (Comment _)        = "comment"
      | name (Constraint)       = "constraint"
      | name (NoConstraint)     = "constraint"
      | name (Decorate)         = "decorate"
      | name (NoDecorate)       = "decorate"
      | name (Dir_forward)      = "dir"
      | name (Dir_back)         = "dir"
      | name (Dir_both)         = "dir"
      | name (Dir_none)         = "dir"
      | name (EdgeURL _)        = "edgeURL"
      | name (EdgeHref _)       = "edgehref"
      | name (EdgeTarget _)     = "edgetarget"
      | name (EdgeTooltip _)    = "edgetooltip"
      | name (FontColor _)      = "fontcolor"
      | name (FontName _)       = "fontname"
      | name (FontSize _)       = "fontsize"
      | name (HeadURL _)        = "headURL"
      | name (HeadClip)         = "headclip"
      | name (NoHeadClip)       = "headclip"
      | name (HeadHref _)       = "headhref"
      | name (HeadLabel _)      = "headlabel"
      | name (HeadPort _)       = "headport"
      | name (HeadTarget _)     = "headtarget"
      | name (HeadTooltip _)    = "headtooltip"
      | name (Href _)           = "href"
      | name (Label _)          = "label"
      | name (LabelURL _)       = "labelURL"
      | name (LabelAngle _)     = "labelangle"
      | name (LabelDistance _)  = "labeldistance"
      | name (LabelFloat)       = "labelfloat"
      | name (NoLabelFloat)     = "labelfloat"
      | name (LabelFontColor _) = "labelfontcolor"
      | name (LabelFontName _)  = "labelfontname"
      | name (LabelFontSize _)  = "labelfontsize"
      | name (LabelHref _)      = "labelhref"
      | name (LabelTarget _)    = "labeltarget"
      | name (LabelTooltip _)   = "labeltooltip"
      | name (Layer _)          = "layer"
      | name (Len _)            = "len"
      | name (LHead _)          = "lhead"
      | name (LabelPos _)       = "lp"
      | name (LTail _)          = "ltail"
      | name (MinLen _)         = "minlen"
      | name (Justify)          = "nojustify"
      | name (NoJustify)        = "nojustify"
      | name (Pos _)            = "pos"
      | name (SameHead _)       = "samehead"
      | name (SameTail _)       = "sametail"
      | name (ShowBoxes _)      = "showboxes"
      | name (Style _)          = "style"
      | name (TailURL _)        = "tailURL"
      | name (TailClip)         = "tailclip"
      | name (NoTailClip)       = "tailclip"
      | name (TailHref _)       = "tailhref"
      | name (TailLabel _)      = "taillabel"
      | name (TailPort _)       = "tailport"
      | name (TailTarget _)     = "tailtarget"
      | name (TailTooltip _)    = "tailtooltip"
      | name (Target _)         = "target"
      | name (Tooltip _)        = "tooltip"
      | name (Weight _)         = "weight"
      (*}}}1*)

    (* Auxilliary display functions *)
    (* {{{1*)
    fun arrowShapeToString shape = let
        fun addside (NONE)           = ""
          | addside (SOME LeftOnly)  = "l" 
          | addside (SOME RightOnly) = "r" 
        fun addopen false = "o"
          | addopen true = ""
      in String.concat (
            case shape
              of Box {side, filled}   => [addopen filled, addside side, "box"]
               | Crow {side}          => [addside side, "crow"]
               | Diamond {side,filled}=> [addopen filled, addside side, "diamond"]
               | Dot {filled}         => [addopen filled, "dot"]
               | Inv {side, filled}   => [addopen filled, addside side, "inv"]
               | Normal {side, filled}=> [addopen filled, addside side, "normal"]
               | Tee {side}           => [addside side, "tee"]
               | Vee {side}           => [addside side, "vee"])
      end

    fun arrowTypeToString (None)            = "none"
      | arrowTypeToString (Single a)        = arrowShapeToString a
      | arrowTypeToString (Double (a1, a2)) = arrowShapeToString a1
                                            ^ arrowShapeToString a2

    fun compassToString N  = "N" | compassToString NE = "NE"
      | compassToString E  = "E" | compassToString SE = "SE"
      | compassToString S  = "S" | compassToString SW = "SW"
      | compassToString W  = "W" | compassToString NW = "NW"

    fun portPosToString (Port (p, NONE))   = "\""^p^"\""
      | portPosToString (Port (p, SOME c)) = "\""^p^":"^compassToString c^"\""
      | portPosToString (Compass c)        = "\""^compassToString c^"\""

    fun edgeStyleToString Dashed          = "dashed"
      | edgeStyleToString Dotted          = "dotted"
      | edgeStyleToString Solid           = "solid"
      | edgeStyleToString Invisible       = "invis"
      | edgeStyleToString Bold            = "bold"
      | edgeStyleToString (CustomStyle s) = s
    (*}}}1*)

    fun value (URL ls)          = lblStringToString ls
      (*{{{1*)
      | value (ArrowHead a)     = (dbl o arrowTypeToString) a
      | value (ArrowSize r)     = showReal r
      | value (ArrowTail a)     = (dbl o arrowTypeToString) a
      | value (Color cs)        = makeList colorToString cs
      | value (ColorScheme s)   = showString s
      | value (Comment s)       = showString s
      | value (Constraint)      = "true"
      | value (NoConstraint)    = "false"
      | value (Decorate)        = "true"
      | value (NoDecorate)      = "false"
      | value (Dir_forward)     = "\"forward\""
      | value (Dir_back)        = "\"back\""
      | value (Dir_both)        = "\"both\""
      | value (Dir_none)        = "\"none\""
      | value (EdgeURL ls)      = lblStringToString ls
      | value (EdgeHref ls)     = lblStringToString ls
      | value (EdgeTarget e)    = showString e
      | value (EdgeTooltip e)   = showString e
      | value (FontColor c)     = (dbl o colorToString) c
      | value (FontName s)      = showString s
      | value (FontSize r)      = showReal r
      | value (HeadURL ls)      = lblStringToString ls
      | value (HeadClip)        = "true"
      | value (NoHeadClip)      = "false"
      | value (HeadHref ls)     = lblStringToString ls
      | value (HeadLabel ls)    = lblStringToString ls
      | value (HeadPort pp)     = portPosToString pp
      | value (HeadTarget e)    = showString e
      | value (HeadTooltip e)   = showString e
      | value (Href ls)         = lblStringToString ls
      | value (Label ls)        = lblStringToString ls
      | value (LabelURL ls)     = lblStringToString ls
      | value (LabelAngle r)    = showReal r
      | value (LabelDistance r) = showReal r
      | value (LabelFloat)      = "true"
      | value (NoLabelFloat)    = "false"
      | value (LabelFontColor c)= (dbl o colorToString) c
      | value (LabelFontName s) = showString s
      | value (LabelFontSize r) = showReal r
      | value (LabelHref ls)    = lblStringToString ls
      | value (LabelTarget e)   = showString e
      | value (LabelTooltip e)  = showString e
      | value (Layer lr)        = layerRangeToString lr
      | value (Len r)           = showReal r
      | value (LHead s)         = showString s
      | value (LabelPos p)      = showPointf p
      | value (LTail s)         = showString s
      | value (MinLen i)        = showInt i
      | value (Justify)         = "false"
      | value (NoJustify)       = "true"
      | value (Pos s)           = splineTypeToString s
      | value (SameHead s)      = showString s
      | value (SameTail s)      = showString s
      | value (ShowBoxes i)     = showInt i
      | value (Style s)         = (dbl o edgeStyleToString) s
      | value (TailURL ls)      = lblStringToString ls
      | value (TailClip)        = "true"
      | value (NoTailClip)      = "false"
      | value (TailHref ls)     = lblStringToString ls
      | value (TailLabel ls)    = lblStringToString ls
      | value (TailPort pp)     = portPosToString pp
      | value (TailTarget e)    = showString e
      | value (TailTooltip e)   = showString e
      | value (Target e)        = showString e
      | value (Tooltip e)       = showString e
      | value (Weight r)        = showReal r
      (*}}}1*)

    fun hasValue _ = true
  end

  structure Node :> NODE_ATTRIBUTE =
  struct
    open Common

    datatype nodeStyle = Filled  | Invisible | Diagonals
                       | Rounded | Dashed    | Dotted
                       | Solid   | Bold      | CustomStyle of string

    datatype recordField = Record of recordField list
                         | Field of string option * string option
    datatype recordLabel = Plain of string
                         | LabelRecord of recordField list

    datatype t = URL of lblString         (*svg, postscript, map only*)
      (*{{{1*) | Color of color list
               | ColorScheme of string
               | Comment of string
               | Distortion of real
               | FillColor of color
               | FixedSize
               | NoFixedSize
               | FontColor of color
               | FontName of string
               | FontSize of real
               | Group of string          (*dot only*)
               | Height of real
               | Label of recordLabel
               | Layer of layerRange
               | Margin of spacing
               | Justify
               | NoJustify
               | Orientation of real
               | Peripheries of int
               | Pin                      (*fdp, neato only*)
               | NoPin                    (*fdp, neato only*)
               | Pos of pointf
               | Rects of rect
               | Regular
               | NotRegular
               | Root                     (*circo, twopi only*)
               | SamplePoints of int
               | BoxShape
               | PolygonShape
               | EllipseShape
               | CircleShape
               | PointShape
               | EggShape        
               | TriangleShape
               | PlainTextShape
               | DiamondShape
               | TrapeziumShape
               | ParallelogramShape
               | HouseShape
               | PentagonShape
               | HexagonShape
               | OctagonShape
               | DoubleCircleShape
               | SeptagonShape
               | DoubleOctagonShape
               | TripleOctagonShape
               | InvTriangleShape
               | InvTrapeziumShape
               | InvHouseShape
               | MDiamondShape
               | MSquareShape
               | MCircleShape
               | NoShape
               | RecordShape
               | MRecordShape
               | ShapeFile of string
               | ShowBoxes of int         (*dot only*)
               | Sides of int
               | Skew of real
               | Style of nodeStyle list
               | Target of escString      (*svg, map only*)
               | Tooltip of escString     (*svg, cmap only*)
               | Vertices of pointf list
               | Width of real
               | Z of real
        (*}}}1*)

    fun PlainLabel s  = Label (Plain s)
    fun RecordLabel s = Label (LabelRecord s)

    fun name (URL _)            = "URL"                 (*ENGC*)
      (*{{{1*)
      | name (Color _)          = "color"
      | name (ColorScheme _)    = "colorscheme"
      | name (Comment _)        = "comment"
      | name (Distortion _)     = "distortion"
      | name (FillColor _)      = "fillcolor"
      | name (FixedSize)        = "fixedsize"
      | name (NoFixedSize)      = "fixedsize"
      | name (FontColor _)      = "fontcolor"
      | name (FontName _)       = "fontname"
      | name (FontSize _)       = "fontsize"
      | name (Group _)          = "group"
      | name (Height _)         = "height"
      | name (Label _)          = "label"
      | name (Layer _)          = "layer"
      | name (Margin _)         = "margin"
      | name (Justify)          = "nojustify"
      | name (NoJustify)        = "nojustify"
      | name (Orientation _)    = "orientation"
      | name (Peripheries _)    = "peripheries"
      | name (Pin)              = "pin"
      | name (NoPin)            = "pin"
      | name (Pos _)            = "pos"
      | name (Rects _)          = "rects"
      | name (Regular)          = "regular"
      | name (NotRegular)       = "regular"
      | name (Root)             = "root"
      | name (SamplePoints _)   = "samplepoints"
      | name (BoxShape)         = "shape"
      | name (PolygonShape)     = "shape"
      | name (EllipseShape)     = "shape"
      | name (CircleShape)      = "shape"
      | name (PointShape)       = "shape"
      | name (EggShape)         = "shape"
      | name (TriangleShape)    = "shape"
      | name (PlainTextShape)   = "shape"
      | name (DiamondShape)     = "shape"
      | name (TrapeziumShape)   = "shape"
      | name (ParallelogramShape)= "shape"
      | name (HouseShape)       = "shape"
      | name (PentagonShape)    = "shape"
      | name (HexagonShape)     = "shape"
      | name (OctagonShape)     = "shape"
      | name (DoubleCircleShape)= "shape"
      | name (SeptagonShape)    = "shape"
      | name (DoubleOctagonShape)= "shape"
      | name (TripleOctagonShape)= "shape"
      | name (InvTriangleShape) = "shape"
      | name (InvTrapeziumShape)= "shape"
      | name (InvHouseShape)    = "shape"
      | name (MDiamondShape)    = "shape"
      | name (MSquareShape)     = "shape"
      | name (MCircleShape)     = "shape"
      | name (NoShape)          = "shape"
      | name (RecordShape)      = "shape"
      | name (MRecordShape)     = "shape"
      | name (ShapeFile _)      = "shapefile"
      | name (ShowBoxes _)      = "showboxes"
      | name (Sides _)          = "sides"
      | name (Skew _)           = "skew"
      | name (Style _)          = "style"
      | name (Target _)         = "target"
      | name (Tooltip _)        = "tooltip"
      | name (Vertices _)       = "vertices"
      | name (Width _)          = "width"
      | name (Z _)              = "z"
      (*}}}1*)

    (* Auxilliary display functions *)
    (* {{{1*)
    fun nodeStyleToString Filled          = "filled"
      | nodeStyleToString Invisible       = "invisible"
      | nodeStyleToString Diagonals       = "diagonals"
      | nodeStyleToString Rounded         = "rounded"
      | nodeStyleToString Dashed          = "dashed"
      | nodeStyleToString Dotted          = "dotted"
      | nodeStyleToString Solid           = "solid"
      | nodeStyleToString Bold            = "bold"
      | nodeStyleToString (CustomStyle s) = s

    fun fieldIdStr s = let
        fun tr #"{" = "\\{" | tr #"}" = "\\}"
          | tr #"|" = "\\|" | tr #"<" = "\\<"
          | tr #">" = "\\>" | tr #" " = "\\ "
          | tr c    = Char.toCString c
      in String.translate tr s end

    fun recordFieldsToString fs = ListFormat.fmt {init="\"",final="\"",sep="|",
                                                  fmt=recordFieldToString} fs
    and recordFieldToString (Record fs) = "{"^recordFieldsToString fs^"}"
      | recordFieldToString (Field (NONE, NONE))     = ""
      | recordFieldToString (Field (SOME p, NONE))   = "<"^fieldIdStr p^">"
      | recordFieldToString (Field (NONE, SOME l))   = fieldIdStr l
      | recordFieldToString (Field (SOME p, SOME l)) = String.concat
                                      ["<", fieldIdStr p, "> ", fieldIdStr l]

    fun recordLabelToString (Plain s)        = showString s
      | recordLabelToString (LabelRecord fs) = recordFieldsToString fs

    (*}}}1*)

    fun value (URL ls)          = lblStringToString ls
      (*{{{1*)
      | value (Color cs)        = makeList colorToString cs
      | value (ColorScheme s)   = showString s
      | value (Comment s)       = showString s
      | value (Distortion r)    = showReal r
      | value (FillColor c)     = (dbl o colorToString) c
      | value (FixedSize)       = "true"
      | value (NoFixedSize)     = "false"
      | value (FontColor c)     = (dbl o colorToString) c
      | value (FontName s)      = showString s
      | value (FontSize r)      = showReal r
      | value (Group s)         = showString s
      | value (Height r)        = showReal r
      | value (Label ls)        = recordLabelToString ls
      | value (Layer lr)        = layerRangeToString lr
      | value (Margin sp)       = spacingToString sp
      | value (Justify)         = "false"
      | value (NoJustify)       = "true"
      | value (Orientation r)   = showReal r
      | value (Peripheries i)   = showInt i
      | value (Pin)             = "true"
      | value (NoPin)           = "false"
      | value (Pos p)           = showPointf p
      | value (Rects r)         = rectToString r
      | value (Regular)         = "true"
      | value (NotRegular)      = "false"
      | value (Root)            = "true"
      | value (SamplePoints i)  = showInt i
      | value (BoxShape)        = "\"box\""
      | value (PolygonShape)    = "\"polygon\""
      | value (EllipseShape)    = "\"ellipse\""
      | value (CircleShape)     = "\"circle\""
      | value (PointShape)      = "\"point\""
      | value (EggShape)        = "\"egg\""
      | value (TriangleShape)   = "\"triangle\""
      | value (PlainTextShape)  = "\"plaintext\""
      | value (DiamondShape)    = "\"diamond\""
      | value (TrapeziumShape)  = "\"trapezium\""
      | value (ParallelogramShape)= "\"parallelogram\""
      | value (HouseShape)      = "\"house\""
      | value (PentagonShape)   = "\"pentagon\""
      | value (HexagonShape)    = "\"hexagon\""
      | value (SeptagonShape)   = "\"septagon\""
      | value (OctagonShape)    = "\"octagon\""
      | value (DoubleCircleShape)= "\"doublecircle\""
      | value (DoubleOctagonShape)= "\"doubleoctagon\""
      | value (TripleOctagonShape)= "\"tripleoctagon\""
      | value (InvTriangleShape)= "\"invtriangle\""
      | value (InvTrapeziumShape)= "\"invtrapezium\""
      | value (InvHouseShape)   = "\"invhouse\""
      | value (MDiamondShape)   = "\"Mdiamond\""
      | value (MSquareShape)    = "\"Msquare\""
      | value (MCircleShape)    = "\"Mcircle\""
      | value (NoShape)         = "\"none\""
      | value (RecordShape)     = "\"record\""
      | value (MRecordShape)    = "\"Mrecord\""
      | value (ShapeFile s)     = showString s
      | value (ShowBoxes i)     = showInt i
      | value (Sides i)         = showInt i
      | value (Skew r)          = showReal r
      | value (Style ss)        = makeList nodeStyleToString ss
      | value (Target e)        = showString e
      | value (Tooltip e)       = showString e
      | value (Vertices vs)     = makeList (fn (x, y)=>showReal x
                                                       ^","^showReal y) vs
      | value (Width r)         = showReal r
      | value (Z r)             = showReal r
      (*}}}1*)

    fun hasValue _ = true
  end

end

