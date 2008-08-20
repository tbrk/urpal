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
structure X11Color : X11_COLOR =
struct
  datatype t = LightPink | Pink | Crimson | LavenderBlush | PaleVioletRed
             | HotPink | DeepPink | MediumVioletRed | Orchid
             | Thistle | Plum | Violet | Magenta
             | Fuchsia | DarkMagenta | Purple | MediumOrchid
             | DarkViolet | DarkOrchid | Indigo | BlueViolet
             | MediumPurple | MediumSlateBlue | SlateBlue | DarkSlateBlue
             | Lavender | GhostWhite | Blue | MediumBlue
             | MidnightBlue | DarkBlue | Navy | RoyalBlue
             | CornflowerBlue | LightSteelBlue | LightSlateGray | SlateGray
             | DodgerBlue | AliceBlue | SteelBlue | LightSkyBlue
             | SkyBlue | DeepSkyBlue | LightBlue | PowderBlue
             | CadetBlue | Azure | LightCyan | PaleTurquoise
             | Cyan | Aqua | DarkTurquoise | DarkSlateGray
             | DarkCyan | Teal | MediumTurquoise | LightSeaGreen
             | Turquoise | Aquamarine | MediumAquamarine | MediumSpringGreen
             | MintCream | SpringGreen | MediumSeaGreen | SeaGreen
             | Honeydew | LightGreen | PaleGreen | DarkSeaGreen
             | LimeGreen | Lime | ForestGreen | Green
             | DarkGreen | Chartreuse | LawnGreen | GreenYellow
             | DarkOliveGreen | YellowGreen | OliveDrab | Beige
             | LightGoldenrodYellow | Ivory | LightYellow | Yellow
             | Olive | DarkKhaki | LemonChiffon | PaleGoldenrod
             | Khaki | Gold | Cornsilk | Goldenrod
             | DarkGoldenrod | FloralWhite | OldLace | Wheat
             | Moccasin | Orange | PapayaWhip | BlanchedAlmond
             | NavajoWhite | AntiqueWhite | Tan | BurlyWood
             | Bisque | DarkOrange | Linen | Peru
             | PeachPuff | SandyBrown | Chocolate | SaddleBrown
             | Seashell | Sienna | LightSalmon | Coral
             | OrangeRed | DarkSalmon | Tomato | MistyRose
             | Salmon | Snow | LightCoral | RosyBrown
             | IndianRed | Red | Brown | FireBrick
             | DarkRed | Maroon | White | WhiteSmoke
             | Gainsboro | LightGrey | Silver | DarkGray
             | Gray | DimGray | Black

  fun toString LightPink                = "lightpink"
    | toString Pink                     = "pink"
    | toString Crimson                  = "crimson"
    | toString LavenderBlush            = "lavenderblush"
    | toString PaleVioletRed            = "palevioletred"
    | toString HotPink                  = "hotpink"
    | toString DeepPink                 = "deeppink"
    | toString MediumVioletRed          = "mediumvioletred"
    | toString Orchid                   = "orchid"
    | toString Thistle                  = "thistle"
    | toString Plum                     = "plum"
    | toString Violet                   = "violet"
    | toString Magenta                  = "magenta"
    | toString Fuchsia                  = "fuchsia"
    | toString DarkMagenta              = "darkmagenta"
    | toString Purple                   = "purple"
    | toString MediumOrchid             = "mediumorchid"
    | toString DarkViolet               = "darkviolet"
    | toString DarkOrchid               = "darkorchid"
    | toString Indigo                   = "indigo"
    | toString BlueViolet               = "blueviolet"
    | toString MediumPurple             = "mediumpurple"
    | toString MediumSlateBlue          = "mediumslateblue"
    | toString SlateBlue                = "slateblue"
    | toString DarkSlateBlue            = "darkslateblue"
    | toString Lavender                 = "lavender"
    | toString GhostWhite               = "ghostwhite"
    | toString Blue                     = "blue"
    | toString MediumBlue               = "mediumblue"
    | toString MidnightBlue             = "midnightblue"
    | toString DarkBlue                 = "darkblue"
    | toString Navy                     = "navy"
    | toString RoyalBlue                = "royalblue"
    | toString CornflowerBlue           = "cornflowerblue"
    | toString LightSteelBlue           = "lightsteelblue"
    | toString LightSlateGray           = "lightslategray"
    | toString SlateGray                = "slategray"
    | toString DodgerBlue               = "dodgerblue"
    | toString AliceBlue                = "aliceblue"
    | toString SteelBlue                = "steelblue"
    | toString LightSkyBlue             = "lightskyblue"
    | toString SkyBlue                  = "skyblue"
    | toString DeepSkyBlue              = "deepskyblue"
    | toString LightBlue                = "lightblue"
    | toString PowderBlue               = "powderblue"
    | toString CadetBlue                = "cadetblue"
    | toString Azure                    = "azure"
    | toString LightCyan                = "lightcyan"
    | toString PaleTurquoise            = "paleturquoise"
    | toString Cyan                     = "cyan"
    | toString Aqua                     = "aqua"
    | toString DarkTurquoise            = "darkturquoise"
    | toString DarkSlateGray            = "darkslategray"
    | toString DarkCyan                 = "darkcyan"
    | toString Teal                     = "teal"
    | toString MediumTurquoise          = "mediumturquoise"
    | toString LightSeaGreen            = "lightseagreen"
    | toString Turquoise                = "turquoise"
    | toString Aquamarine               = "aquamarine"
    | toString MediumAquamarine         = "mediumaquamarine"
    | toString MediumSpringGreen        = "mediumspringgreen"
    | toString MintCream                = "mintcream"
    | toString SpringGreen              = "springgreen"
    | toString MediumSeaGreen           = "mediumseagreen"
    | toString SeaGreen                 = "seagreen"
    | toString Honeydew                 = "honeydew"
    | toString LightGreen               = "lightgreen"
    | toString PaleGreen                = "palegreen"
    | toString DarkSeaGreen             = "darkseagreen"
    | toString LimeGreen                = "limegreen"
    | toString Lime                     = "lime"
    | toString ForestGreen              = "forestgreen"
    | toString Green                    = "green"
    | toString DarkGreen                = "darkgreen"
    | toString Chartreuse               = "chartreuse"
    | toString LawnGreen                = "lawngreen"
    | toString GreenYellow              = "greenyellow"
    | toString DarkOliveGreen           = "darkolivegreen"
    | toString YellowGreen              = "yellowgreen"
    | toString OliveDrab                = "olivedrab"
    | toString Beige                    = "beige"
    | toString LightGoldenrodYellow     = "lightgoldenrodyellow"
    | toString Ivory                    = "ivory"
    | toString LightYellow              = "lightyellow"
    | toString Yellow                   = "yellow"
    | toString Olive                    = "olive"
    | toString DarkKhaki                = "darkkhaki"
    | toString LemonChiffon             = "lemonchiffon"
    | toString PaleGoldenrod            = "palegoldenrod"
    | toString Khaki                    = "khaki"
    | toString Gold                     = "gold"
    | toString Cornsilk                 = "cornsilk"
    | toString Goldenrod                = "goldenrod"
    | toString DarkGoldenrod            = "darkgoldenrod"
    | toString FloralWhite              = "floralwhite"
    | toString OldLace                  = "oldlace"
    | toString Wheat                    = "wheat"
    | toString Moccasin                 = "moccasin"
    | toString Orange                   = "orange"
    | toString PapayaWhip               = "papayawhip"
    | toString BlanchedAlmond           = "blanchedalmond"
    | toString NavajoWhite              = "navajowhite"
    | toString AntiqueWhite             = "antiquewhite"
    | toString Tan                      = "tan"
    | toString BurlyWood                = "burlywood"
    | toString Bisque                   = "bisque"
    | toString DarkOrange               = "darkorange"
    | toString Linen                    = "linen"
    | toString Peru                     = "peru"
    | toString PeachPuff                = "peachpuff"
    | toString SandyBrown               = "sandybrown"
    | toString Chocolate                = "chocolate"
    | toString SaddleBrown              = "saddlebrown"
    | toString Seashell                 = "seashell"
    | toString Sienna                   = "sienna"
    | toString LightSalmon              = "lightsalmon"
    | toString Coral                    = "coral"
    | toString OrangeRed                = "orangered"
    | toString DarkSalmon               = "darksalmon"
    | toString Tomato                   = "tomato"
    | toString MistyRose                = "mistyrose"
    | toString Salmon                   = "salmon"
    | toString Snow                     = "snow"
    | toString LightCoral               = "lightcoral"
    | toString RosyBrown                = "rosybrown"
    | toString IndianRed                = "indianred"
    | toString Red                      = "red"
    | toString Brown                    = "brown"
    | toString FireBrick                = "firebrick"
    | toString DarkRed                  = "darkred"
    | toString Maroon                   = "maroon"
    | toString White                    = "white"
    | toString WhiteSmoke               = "whitesmoke"
    | toString Gainsboro                = "gainsboro"
    | toString LightGrey                = "lightgrey"
    | toString Silver                   = "silver"
    | toString DarkGray                 = "darkgray"
    | toString Gray                     = "gray"
    | toString DimGray                  = "dimgray"
    | toString Black                    = "black"

  fun fromString "lightpink"            = LightPink
    | fromString "pink"                 = Pink
    | fromString "crimson"              = Crimson
    | fromString "lavenderblush"        = LavenderBlush
    | fromString "palevioletred"        = PaleVioletRed
    | fromString "hotpink"              = HotPink
    | fromString "deeppink"             = DeepPink
    | fromString "mediumvioletred"      = MediumVioletRed
    | fromString "orchid"               = Orchid
    | fromString "thistle"              = Thistle
    | fromString "plum"                 = Plum
    | fromString "violet"               = Violet
    | fromString "magenta"              = Magenta
    | fromString "fuchsia"              = Fuchsia
    | fromString "darkmagenta"          = DarkMagenta
    | fromString "purple"               = Purple
    | fromString "mediumorchid"         = MediumOrchid
    | fromString "darkviolet"           = DarkViolet
    | fromString "darkorchid"           = DarkOrchid
    | fromString "indigo"               = Indigo
    | fromString "blueviolet"           = BlueViolet
    | fromString "mediumpurple"         = MediumPurple
    | fromString "mediumslateblue"      = MediumSlateBlue
    | fromString "slateblue"            = SlateBlue
    | fromString "darkslateblue"        = DarkSlateBlue
    | fromString "lavender"             = Lavender
    | fromString "ghostwhite"           = GhostWhite
    | fromString "blue"                 = Blue
    | fromString "mediumblue"           = MediumBlue
    | fromString "midnightblue"         = MidnightBlue
    | fromString "darkblue"             = DarkBlue
    | fromString "navy"                 = Navy
    | fromString "royalblue"            = RoyalBlue
    | fromString "cornflowerblue"       = CornflowerBlue
    | fromString "lightsteelblue"       = LightSteelBlue
    | fromString "lightslategray"       = LightSlateGray
    | fromString "slategray"            = SlateGray
    | fromString "dodgerblue"           = DodgerBlue
    | fromString "aliceblue"            = AliceBlue
    | fromString "steelblue"            = SteelBlue
    | fromString "lightskyblue"         = LightSkyBlue
    | fromString "skyblue"              = SkyBlue
    | fromString "deepskyblue"          = DeepSkyBlue
    | fromString "lightblue"            = LightBlue
    | fromString "powderblue"           = PowderBlue
    | fromString "cadetblue"            = CadetBlue
    | fromString "azure"                = Azure
    | fromString "lightcyan"            = LightCyan
    | fromString "paleturquoise"        = PaleTurquoise
    | fromString "cyan"                 = Cyan
    | fromString "aqua"                 = Aqua
    | fromString "darkturquoise"        = DarkTurquoise
    | fromString "darkslategray"        = DarkSlateGray
    | fromString "darkcyan"             = DarkCyan
    | fromString "teal"                 = Teal
    | fromString "mediumturquoise"      = MediumTurquoise
    | fromString "lightseagreen"        = LightSeaGreen
    | fromString "turquoise"            = Turquoise
    | fromString "aquamarine"           = Aquamarine
    | fromString "mediumaquamarine"     = MediumAquamarine
    | fromString "mediumspringgreen"    = MediumSpringGreen
    | fromString "mintcream"            = MintCream
    | fromString "springgreen"          = SpringGreen
    | fromString "mediumseagreen"       = MediumSeaGreen
    | fromString "seagreen"             = SeaGreen
    | fromString "honeydew"             = Honeydew
    | fromString "lightgreen"           = LightGreen
    | fromString "palegreen"            = PaleGreen
    | fromString "darkseagreen"         = DarkSeaGreen
    | fromString "limegreen"            = LimeGreen
    | fromString "lime"                 = Lime
    | fromString "forestgreen"          = ForestGreen
    | fromString "green"                = Green
    | fromString "darkgreen"            = DarkGreen
    | fromString "chartreuse"           = Chartreuse
    | fromString "lawngreen"            = LawnGreen
    | fromString "greenyellow"          = GreenYellow
    | fromString "darkolivegreen"       = DarkOliveGreen
    | fromString "yellowgreen"          = YellowGreen
    | fromString "olivedrab"            = OliveDrab
    | fromString "beige"                = Beige
    | fromString "lightgoldenrodyellow" = LightGoldenrodYellow
    | fromString "ivory"                = Ivory
    | fromString "lightyellow"          = LightYellow
    | fromString "yellow"               = Yellow
    | fromString "olive"                = Olive
    | fromString "darkkhaki"            = DarkKhaki
    | fromString "lemonchiffon"         = LemonChiffon
    | fromString "palegoldenrod"        = PaleGoldenrod
    | fromString "khaki"                = Khaki
    | fromString "gold"                 = Gold
    | fromString "cornsilk"             = Cornsilk
    | fromString "goldenrod"            = Goldenrod
    | fromString "darkgoldenrod"        = DarkGoldenrod
    | fromString "floralwhite"          = FloralWhite
    | fromString "oldlace"              = OldLace
    | fromString "wheat"                = Wheat
    | fromString "moccasin"             = Moccasin
    | fromString "orange"               = Orange
    | fromString "papayawhip"           = PapayaWhip
    | fromString "blanchedalmond"       = BlanchedAlmond
    | fromString "navajowhite"          = NavajoWhite
    | fromString "antiquewhite"         = AntiqueWhite
    | fromString "tan"                  = Tan
    | fromString "burlywood"            = BurlyWood
    | fromString "bisque"               = Bisque
    | fromString "darkorange"           = DarkOrange
    | fromString "linen"                = Linen
    | fromString "peru"                 = Peru
    | fromString "peachpuff"            = PeachPuff
    | fromString "sandybrown"           = SandyBrown
    | fromString "chocolate"            = Chocolate
    | fromString "saddlebrown"          = SaddleBrown
    | fromString "seashell"             = Seashell
    | fromString "sienna"               = Sienna
    | fromString "lightsalmon"          = LightSalmon
    | fromString "coral"                = Coral
    | fromString "orangered"            = OrangeRed
    | fromString "darksalmon"           = DarkSalmon
    | fromString "tomato"               = Tomato
    | fromString "mistyrose"            = MistyRose
    | fromString "salmon"               = Salmon
    | fromString "snow"                 = Snow
    | fromString "lightcoral"           = LightCoral
    | fromString "rosybrown"            = RosyBrown
    | fromString "indianred"            = IndianRed
    | fromString "red"                  = Red
    | fromString "brown"                = Brown
    | fromString "firebrick"            = FireBrick
    | fromString "darkred"              = DarkRed
    | fromString "maroon"               = Maroon
    | fromString "white"                = White
    | fromString "whitesmoke"           = WhiteSmoke
    | fromString "gainsboro"            = Gainsboro
    | fromString "lightgrey"            = LightGrey
    | fromString "silver"               = Silver
    | fromString "darkgray"             = DarkGray
    | fromString "gray"                 = Gray
    | fromString "dimgray"              = DimGray
    | fromString "black"                = Black
    | fromString _ = raise Fail "X11Color.fromString: unrecognized color name"

  fun toRGB LightPink                   = {red=0xff, green=0xb6, blue=0xc1}
    | toRGB Pink                        = {red=0xff, green=0xc0, blue=0xcb}
    | toRGB Crimson                     = {red=0xdc, green=0x14, blue=0x3c}
    | toRGB LavenderBlush               = {red=0xff, green=0xf0, blue=0xf5}
    | toRGB PaleVioletRed               = {red=0xdb, green=0x70, blue=0x93}
    | toRGB HotPink                     = {red=0xff, green=0x69, blue=0xb4}
    | toRGB DeepPink                    = {red=0xff, green=0x14, blue=0x93}
    | toRGB MediumVioletRed             = {red=0xc7, green=0x15, blue=0x85}
    | toRGB Orchid                      = {red=0xda, green=0x70, blue=0xd6}
    | toRGB Thistle                     = {red=0xd8, green=0xbf, blue=0xd8}
    | toRGB Plum                        = {red=0xdd, green=0xa0, blue=0xdd}
    | toRGB Violet                      = {red=0xee, green=0x82, blue=0xee}
    | toRGB Magenta                     = {red=0xff, green=0x00, blue=0xff}
    | toRGB Fuchsia                     = {red=0xff, green=0x00, blue=0xff}
    | toRGB DarkMagenta                 = {red=0x8b, green=0x00, blue=0x8b}
    | toRGB Purple                      = {red=0x80, green=0x00, blue=0x80}
    | toRGB MediumOrchid                = {red=0xba, green=0x55, blue=0xd3}
    | toRGB DarkViolet                  = {red=0x94, green=0x00, blue=0xd3}
    | toRGB DarkOrchid                  = {red=0x99, green=0x32, blue=0xcc}
    | toRGB Indigo                      = {red=0x4b, green=0x00, blue=0x82}
    | toRGB BlueViolet                  = {red=0x8a, green=0x2b, blue=0xe2}
    | toRGB MediumPurple                = {red=0x93, green=0x70, blue=0xdb}
    | toRGB MediumSlateBlue             = {red=0x7b, green=0x68, blue=0xee}
    | toRGB SlateBlue                   = {red=0x6a, green=0x5a, blue=0xcd}
    | toRGB DarkSlateBlue               = {red=0x48, green=0x3d, blue=0x8b}
    | toRGB Lavender                    = {red=0xe6, green=0xe6, blue=0xfa}
    | toRGB GhostWhite                  = {red=0xf8, green=0xf8, blue=0xff}
    | toRGB Blue                        = {red=0x00, green=0x00, blue=0xff}
    | toRGB MediumBlue                  = {red=0x00, green=0x00, blue=0xcd}
    | toRGB MidnightBlue                = {red=0x19, green=0x19, blue=0x70}
    | toRGB DarkBlue                    = {red=0x00, green=0x00, blue=0x8b}
    | toRGB Navy                        = {red=0x00, green=0x00, blue=0x80}
    | toRGB RoyalBlue                   = {red=0x41, green=0x69, blue=0xe1}
    | toRGB CornflowerBlue              = {red=0x64, green=0x95, blue=0xed}
    | toRGB LightSteelBlue              = {red=0xb0, green=0xc4, blue=0xde}
    | toRGB LightSlateGray              = {red=0x77, green=0x88, blue=0x99}
    | toRGB SlateGray                   = {red=0x70, green=0x80, blue=0x90}
    | toRGB DodgerBlue                  = {red=0x1e, green=0x90, blue=0xff}
    | toRGB AliceBlue                   = {red=0xf0, green=0xf8, blue=0xff}
    | toRGB SteelBlue                   = {red=0x46, green=0x82, blue=0xb4}
    | toRGB LightSkyBlue                = {red=0x87, green=0xce, blue=0xfa}
    | toRGB SkyBlue                     = {red=0x87, green=0xce, blue=0xeb}
    | toRGB DeepSkyBlue                 = {red=0x00, green=0xbf, blue=0xff}
    | toRGB LightBlue                   = {red=0xad, green=0xd8, blue=0xe6}
    | toRGB PowderBlue                  = {red=0xb0, green=0xe0, blue=0xe6}
    | toRGB CadetBlue                   = {red=0x5f, green=0x9e, blue=0xa0}
    | toRGB Azure                       = {red=0xf0, green=0xff, blue=0xff}
    | toRGB LightCyan                   = {red=0xe0, green=0xff, blue=0xff}
    | toRGB PaleTurquoise               = {red=0xaf, green=0xee, blue=0xee}
    | toRGB Cyan                        = {red=0x00, green=0xff, blue=0xff}
    | toRGB Aqua                        = {red=0x00, green=0xff, blue=0xff}
    | toRGB DarkTurquoise               = {red=0x00, green=0xce, blue=0xd1}
    | toRGB DarkSlateGray               = {red=0x2f, green=0x4f, blue=0x4f}
    | toRGB DarkCyan                    = {red=0x00, green=0x8b, blue=0x8b}
    | toRGB Teal                        = {red=0x00, green=0x80, blue=0x80}
    | toRGB MediumTurquoise             = {red=0x48, green=0xd1, blue=0xcc}
    | toRGB LightSeaGreen               = {red=0x20, green=0xb2, blue=0xaa}
    | toRGB Turquoise                   = {red=0x40, green=0xe0, blue=0xd0}
    | toRGB Aquamarine                  = {red=0x7f, green=0xff, blue=0xd4}
    | toRGB MediumAquamarine            = {red=0x66, green=0xcd, blue=0xaa}
    | toRGB MediumSpringGreen           = {red=0x00, green=0xfa, blue=0x9a}
    | toRGB MintCream                   = {red=0xf5, green=0xff, blue=0xfa}
    | toRGB SpringGreen                 = {red=0x00, green=0xff, blue=0x7f}
    | toRGB MediumSeaGreen              = {red=0x3c, green=0xb3, blue=0x71}
    | toRGB SeaGreen                    = {red=0x2e, green=0x8b, blue=0x57}
    | toRGB Honeydew                    = {red=0xf0, green=0xff, blue=0xf0}
    | toRGB LightGreen                  = {red=0x90, green=0xee, blue=0x90}
    | toRGB PaleGreen                   = {red=0x98, green=0xfb, blue=0x98}
    | toRGB DarkSeaGreen                = {red=0x8f, green=0xbc, blue=0x8f}
    | toRGB LimeGreen                   = {red=0x32, green=0xcd, blue=0x32}
    | toRGB Lime                        = {red=0x00, green=0xff, blue=0x00}
    | toRGB ForestGreen                 = {red=0x22, green=0x8b, blue=0x22}
    | toRGB Green                       = {red=0x00, green=0x80, blue=0x00}
    | toRGB DarkGreen                   = {red=0x00, green=0x64, blue=0x00}
    | toRGB Chartreuse                  = {red=0x7f, green=0xff, blue=0x00}
    | toRGB LawnGreen                   = {red=0x7c, green=0xfc, blue=0x00}
    | toRGB GreenYellow                 = {red=0xad, green=0xff, blue=0x2f}
    | toRGB DarkOliveGreen              = {red=0x55, green=0x6b, blue=0x2f}
    | toRGB YellowGreen                 = {red=0x9a, green=0xcd, blue=0x32}
    | toRGB OliveDrab                   = {red=0x6b, green=0x8e, blue=0x23}
    | toRGB Beige                       = {red=0xf5, green=0xf5, blue=0xdc}
    | toRGB LightGoldenrodYellow        = {red=0xfa, green=0xfa, blue=0xd2}
    | toRGB Ivory                       = {red=0xff, green=0xff, blue=0xf0}
    | toRGB LightYellow                 = {red=0xff, green=0xff, blue=0xe0}
    | toRGB Yellow                      = {red=0xff, green=0xff, blue=0x00}
    | toRGB Olive                       = {red=0x80, green=0x80, blue=0x00}
    | toRGB DarkKhaki                   = {red=0xbd, green=0xb7, blue=0x6b}
    | toRGB LemonChiffon                = {red=0xff, green=0xfa, blue=0xcd}
    | toRGB PaleGoldenrod               = {red=0xee, green=0xe8, blue=0xaa}
    | toRGB Khaki                       = {red=0xf0, green=0xe6, blue=0x8c}
    | toRGB Gold                        = {red=0xff, green=0xd7, blue=0x00}
    | toRGB Cornsilk                    = {red=0xff, green=0xf8, blue=0xdc}
    | toRGB Goldenrod                   = {red=0xda, green=0xa5, blue=0x20}
    | toRGB DarkGoldenrod               = {red=0xb8, green=0x86, blue=0x0b}
    | toRGB FloralWhite                 = {red=0xff, green=0xfa, blue=0xf0}
    | toRGB OldLace                     = {red=0xfd, green=0xf5, blue=0xe6}
    | toRGB Wheat                       = {red=0xf5, green=0xde, blue=0xb3}
    | toRGB Moccasin                    = {red=0xff, green=0xe4, blue=0xb5}
    | toRGB Orange                      = {red=0xff, green=0xa5, blue=0x00}
    | toRGB PapayaWhip                  = {red=0xff, green=0xef, blue=0xd5}
    | toRGB BlanchedAlmond              = {red=0xff, green=0xeb, blue=0xcd}
    | toRGB NavajoWhite                 = {red=0xff, green=0xde, blue=0xad}
    | toRGB AntiqueWhite                = {red=0xfa, green=0xeb, blue=0xd7}
    | toRGB Tan                         = {red=0xd2, green=0xb4, blue=0x8c}
    | toRGB BurlyWood                   = {red=0xde, green=0xb8, blue=0x87}
    | toRGB Bisque                      = {red=0xff, green=0xe4, blue=0xc4}
    | toRGB DarkOrange                  = {red=0xff, green=0x8c, blue=0x00}
    | toRGB Linen                       = {red=0xfa, green=0xf0, blue=0xe6}
    | toRGB Peru                        = {red=0xcd, green=0x85, blue=0x3f}
    | toRGB PeachPuff                   = {red=0xff, green=0xda, blue=0xb9}
    | toRGB SandyBrown                  = {red=0xf4, green=0xa4, blue=0x60}
    | toRGB Chocolate                   = {red=0xd2, green=0x69, blue=0x1e}
    | toRGB SaddleBrown                 = {red=0x8b, green=0x45, blue=0x13}
    | toRGB Seashell                    = {red=0xff, green=0xf5, blue=0xee}
    | toRGB Sienna                      = {red=0xa0, green=0x52, blue=0x2d}
    | toRGB LightSalmon                 = {red=0xff, green=0xa0, blue=0x7a}
    | toRGB Coral                       = {red=0xff, green=0x7f, blue=0x50}
    | toRGB OrangeRed                   = {red=0xff, green=0x45, blue=0x00}
    | toRGB DarkSalmon                  = {red=0xe9, green=0x96, blue=0x7a}
    | toRGB Tomato                      = {red=0xff, green=0x63, blue=0x47}
    | toRGB MistyRose                   = {red=0xff, green=0xe4, blue=0xe1}
    | toRGB Salmon                      = {red=0xfa, green=0x80, blue=0x72}
    | toRGB Snow                        = {red=0xff, green=0xfa, blue=0xfa}
    | toRGB LightCoral                  = {red=0xf0, green=0x80, blue=0x80}
    | toRGB RosyBrown                   = {red=0xbc, green=0x8f, blue=0x8f}
    | toRGB IndianRed                   = {red=0xcd, green=0x5c, blue=0x5c}
    | toRGB Red                         = {red=0xff, green=0x00, blue=0x00}
    | toRGB Brown                       = {red=0xa5, green=0x2a, blue=0x2a}
    | toRGB FireBrick                   = {red=0xb2, green=0x22, blue=0x22}
    | toRGB DarkRed                     = {red=0x8b, green=0x00, blue=0x00}
    | toRGB Maroon                      = {red=0x80, green=0x00, blue=0x00}
    | toRGB White                       = {red=0xff, green=0xff, blue=0xff}
    | toRGB WhiteSmoke                  = {red=0xf5, green=0xf5, blue=0xf5}
    | toRGB Gainsboro                   = {red=0xdc, green=0xdc, blue=0xdc}
    | toRGB LightGrey                   = {red=0xd3, green=0xd3, blue=0xd3}
    | toRGB Silver                      = {red=0xc0, green=0xc0, blue=0xc0}
    | toRGB DarkGray                    = {red=0xa9, green=0xa9, blue=0xa9}
    | toRGB Gray                        = {red=0x80, green=0x80, blue=0x80}
    | toRGB DimGray                     = {red=0x69, green=0x69, blue=0x69}
    | toRGB Black                       = {red=0x00, green=0x00, blue=0x00}

  local
    fun readint ss = valOf (Int.scan StringCvt.HEX Substring.getc ss)
  in
  fun rgbFromString s = if size s <> 7 then NONE
                   else let 
                     val ss = Substring.full s
                     val f = valOf (Substring.first ss)
                     val (r, _) = readint (Substring.slice (ss, 1, SOME 2))
                     val (g, _) = readint (Substring.slice (ss, 3, SOME 2))
                     val (b, _) = readint (Substring.slice (ss, 5, SOME 2))
                   in if f= #"#" then SOME {red=r,green=g,blue=b} else NONE end
                   handle Option => NONE
  end (* local *)

end

