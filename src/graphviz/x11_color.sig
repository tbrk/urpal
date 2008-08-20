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
 * X11 colour names. Original source: Paul McFedrie's home page (book: `The
 * Complete Idiot's Guide to Creating a Web Page and Blog' 6ed, but any of the very
 * many equivalent pages would have served equally well):
 *   http://www.mcfedries.com/books/cightml/x11color.htm
 *)

signature X11_COLOR =
sig
  datatype t = LightPink
             | Pink
             | Crimson
             | LavenderBlush
             | PaleVioletRed
             | HotPink
             | DeepPink
             | MediumVioletRed
             | Orchid
             | Thistle
             | Plum
             | Violet
             | Magenta
             | Fuchsia
             | DarkMagenta
             | Purple
             | MediumOrchid
             | DarkViolet
             | DarkOrchid
             | Indigo
             | BlueViolet
             | MediumPurple
             | MediumSlateBlue
             | SlateBlue
             | DarkSlateBlue
             | Lavender
             | GhostWhite
             | Blue
             | MediumBlue
             | MidnightBlue
             | DarkBlue
             | Navy
             | RoyalBlue
             | CornflowerBlue
             | LightSteelBlue
             | LightSlateGray
             | SlateGray
             | DodgerBlue
             | AliceBlue
             | SteelBlue
             | LightSkyBlue
             | SkyBlue
             | DeepSkyBlue
             | LightBlue
             | PowderBlue
             | CadetBlue
             | Azure
             | LightCyan
             | PaleTurquoise
             | Cyan
             | Aqua
             | DarkTurquoise
             | DarkSlateGray
             | DarkCyan
             | Teal
             | MediumTurquoise
             | LightSeaGreen
             | Turquoise
             | Aquamarine
             | MediumAquamarine
             | MediumSpringGreen
             | MintCream
             | SpringGreen
             | MediumSeaGreen
             | SeaGreen
             | Honeydew
             | LightGreen
             | PaleGreen
             | DarkSeaGreen
             | LimeGreen
             | Lime
             | ForestGreen
             | Green
             | DarkGreen
             | Chartreuse
             | LawnGreen
             | GreenYellow
             | DarkOliveGreen
             | YellowGreen
             | OliveDrab
             | Beige
             | LightGoldenrodYellow
             | Ivory
             | LightYellow
             | Yellow
             | Olive
             | DarkKhaki
             | LemonChiffon
             | PaleGoldenrod
             | Khaki
             | Gold
             | Cornsilk
             | Goldenrod
             | DarkGoldenrod
             | FloralWhite
             | OldLace
             | Wheat
             | Moccasin
             | Orange
             | PapayaWhip
             | BlanchedAlmond
             | NavajoWhite
             | AntiqueWhite
             | Tan
             | BurlyWood
             | Bisque
             | DarkOrange
             | Linen
             | Peru
             | PeachPuff
             | SandyBrown
             | Chocolate
             | SaddleBrown
             | Seashell
             | Sienna
             | LightSalmon
             | Coral
             | OrangeRed
             | DarkSalmon
             | Tomato
             | MistyRose
             | Salmon
             | Snow
             | LightCoral
             | RosyBrown
             | IndianRed
             | Red
             | Brown
             | FireBrick
             | DarkRed
             | Maroon
             | White
             | WhiteSmoke
             | Gainsboro
             | LightGrey
             | Silver
             | DarkGray
             | Gray
             | DimGray
             | Black

  val toString      : t -> string
  val fromString    : string -> t
  val toRGB         : t -> {red:int, green:int, blue:int}
  val rgbFromString : string -> {red:int, green:int, blue:int} option
end
