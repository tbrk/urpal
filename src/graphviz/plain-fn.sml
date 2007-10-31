(* $Id$
 *
 * 20070827 T. Bourke
 *   Original code.
 *
 * Version à l'Haskell:
 *
 *   Disadvantages:
 *      - pretty-printed code in SML/NJ (on compile errors, for instance)
 *        is not so pretty (new indent for each line).
 *      - type errors are harder to debug.
 *      - variables given on right-hand-side.
 *      - closing brackets needed.
 *      - not usual SML style.
 *
 *   Advantages:
 *      - Fully Functional.
 *      - Much plumbing is hidden.
 *
 *   Alternatives:
 *      - sequence of statements:
 *          val (height, strm) = valOf (readReal strm)
 *          val (width,  strm) = valOf (readReal strm)
 *          ...
 *          handle Bind => NONE
 *
 *      - Don't use StringCvt.reader, rather read the input line-by-line
 *        and use the Substring functions to break each up into parts.
 *
 *)

functor PlainFn (
  type id and label and style and shape and color
  val scanId : (char, 'a) StringCvt.reader -> (id, 'a) StringCvt.reader
  val scanLabel : (char, 'a) StringCvt.reader -> (label, 'a) StringCvt.reader
  val scanStyle : (char, 'a) StringCvt.reader -> (style, 'a) StringCvt.reader
  val scanShape : (char, 'a) StringCvt.reader -> (shape, 'a) StringCvt.reader
  val scanColor : (char, 'a) StringCvt.reader -> (color, 'a) StringCvt.reader

  val idToString    : id -> string
  val labelToString : label -> string
  val styleToString : style -> string
  val shapeToString : shape -> string
  val colorToString : color -> string
) : PLAIN =
struct
  type id    = id    and label = label and style = style
   and shape = shape and color = color

  type node  = {name : id,     x : real,     y : real,
                width : real,  height: real, label : label,
                style : style, shape : shape,
                color : color, fillcolor : color}

  type edge  = {head : id, tail : id,
                points : (real * real) list,
                label : (label * real * real) option,
                style : style, color : color}

  type graph = {scale : real, width : real, height : real,
                nodes : node list, edges : edge list}
  
  infix ||; fun f || g = fn strm=> case f strm of NONE => g strm | x => x

  fun debug msg strm = (TextIO.print (String.concat msg); SOME ((), strm))

  fun scan rdr = let
      val readReal  = Real.scan rdr
      val readInt   = Int.scan StringCvt.DEC rdr
      val readId    = scanId rdr
      val readLabel = scanLabel rdr
      val readStyle = scanStyle rdr
      val readShape = scanShape rdr
      val readColor = scanColor rdr

      fun skipWS strm = case rdr strm
          of NONE => strm
           | SOME (c, strm') => if c <> #"\n" andalso Char.isSpace c
                                then skipWS strm' else strm

      infix >*>=; fun f >*>= (g, exc) = fn strm=> case f (skipWS strm)
                                                  of NONE           => exc strm
                                                   | SOME (x, strm) => g x strm

      infix >>=;  fun f >>= g = fn strm => case f (skipWS strm)
                                                  of NONE           => NONE
                                                   | SOME (x, strm) => g x strm
      fun fail strm = NONE
      fun return value strm = SOME (value, strm)

      fun readList' readItem = fn strm=>SOME ([], strm)
      fun readList readItem = let
          fun next xs = readItem >*>= (fn x=> next (x::xs), return (rev xs))
        in next [] end

      fun expect str = let
          fun check []      = return ()
            | check (x::xs) = rdr >>= (fn c=> if c=x then return xs else fail)
                                  >>= check
        in check (explode str) end

      val endofline = rdr >>= (fn c=> if c <> #"\n" then fail else return c)

      val readPoint = readReal >>= (fn x=>readReal >>= (fn y=>return (x, y)))
      fun readPoints n = let
          fun read (0, l) = return (rev l)
            | read (n, l) = readPoint >>= (fn p=>read (n-1, p::l))
        in read (n, []) end

      val readNode  = expect "node"         >>= (fn _=>
                      readId                >>= (fn name=>
                      readPoint             >>= (fn (x,y)=>
                      readReal              >>= (fn width=>
                      readReal              >>= (fn height=>
                      readLabel             >>= (fn label=>
                      readStyle             >>= (fn style=>
                      readShape             >>= (fn shape=>
                      readColor             >>= (fn color=>
                      readColor             >>= (fn fillcolor=>
                      endofline             >>= (fn _=>
                      return {name=name, x=x, y=y,
                              width=width, height=height,
                              label=label, style=style,
                              shape=shape, color=color,
                              fillcolor=fillcolor} )))))))))))
      
      fun restOfEdge label =
                      readStyle             >>= (fn style=>
                      readColor             >>= (fn color=>
                      endofline             >>= (fn _=>
                      return (label, style, color) )))

      val tryLabel  = readLabel             >>= (fn ltext=>
                      readPoint             >>= (fn (x,y)=>
                      restOfEdge (SOME (ltext, x, y))  ))

      val noLabel   = restOfEdge NONE

      val readEdge  = expect "edge"         >>= (fn _=>
                      readId                >>= (fn head=>
                      readId                >>= (fn tail=>
                      readInt               >>=
                      readPoints            >>= (fn points=>
                      tryLabel || noLabel   >>= (fn (labelo, style, color)=>
                      return {head=head, tail=tail, points=points,
                              label=labelo, style=style, color=color})))))

      val readGraph = expect "graph"        >>= (fn _=>
                      readReal              >>= (fn scale=>
                      readReal              >>= (fn width=>
                      readReal              >>= (fn height=>
                      endofline             >>= (fn _=>
                      return (scale, width, height) )))))

      in readGraph             >>= (fn (scale, width, height)=>
         readList readNode     >>= (fn nodes=>
         readList readEdge     >>= (fn edges=>
         expect "stop"         >>= (fn _=>
         return {scale=scale, width=width, height=height,
                 nodes=nodes, edges=edges} ))))
      end
      
    fun output (ostrm, {scale, width, height, nodes, edges} : graph) = let
        fun pr s = TextIO.output (ostrm, s)
        fun prs s = TextIO.output (ostrm, " "^s)
        fun prr r = pr (" "^Real.toString r)

        fun showNode ({name, x, y, width, height, label,
                      style, shape, color, fillcolor} : node) =
            (pr "node "; pr (idToString name); prr x; prr y;
             prr width; prr height; prs (labelToString label);
             prs (styleToString style); prs (shapeToString shape);
             prs (colorToString color); prs (colorToString fillcolor);
             pr "\n")

        fun showLabel NONE             = ()
          | showLabel (SOME (l, x,y)) = (prs (labelToString l); prr x; prr y)

        fun showEdge ({head, tail, points, label, style, color} : edge) =
            (pr "edge"; prs (idToString head); prs (idToString tail);
             app (fn (x, y)=> (prr x; prr y)) points;
             showLabel label;
             prs (styleToString style); prs (colorToString color); pr "\n")

      in
        pr "graph"; prr scale; prr width; prr height; pr "\n";
        app showNode nodes; app showEdge edges;
        TextIO.output (ostrm, "stop\n")
      end

end

