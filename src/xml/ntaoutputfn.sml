(* $Id$ *)

functor NtaOutputFn (
  structure T : NTA_TYPES_OUTPUT
                where type outstream = TextIO.StreamIO.outstream
  structure S : TEXT_STREAM_IO
                where type outstream = TextIO.StreamIO.outstream
               (* where type vector = CharVector.vector
                    and type elem = Char.char
                    and type outstream = TextIO.StreamIO.outstream *)
  (*sharing type T.outstream = S.outstream*)
) : NTA_OUTPUT
=
struct
  structure Parent = NtaFn (T)
  open Parent

  val dtdName = "-//Uppaal Team//DTD Flat System 1.1//EN"
  val dtdURL  = "http://www.it.uu.se/research/group/darts/uppaal/flat-1_1.dtd"

  val toData = XMLWriter.data
  val toXML  = XMLWriter.xml

  type outstream = S.outstream
  fun pr strm str = S.output (strm, str)

  fun iToCStr i = if i >= 0
                  then Int.toString i
                  else "-" ^ Int.toString (Int.abs i)

  fun prOpenTag os name  = let val pr = pr os
                           in pr "<"; pr name; pr ">" end
  fun prCloseTag os name = let val pr = pr os
                           in pr "</"; pr name; pr ">" end

  fun outLocId   (os, attname, NONE)               = ()
    | outLocId   (os, attname, SOME (LocId i))     = let val pr = pr os
        in pr " "; pr attname; pr "=\"id"; pr (iToCStr i); pr "\"" end

  fun outLocRef os (label, LocId i) = (pr os "<"; pr os label;
                                       pr os " ref=\"id"; pr os (iToCStr i);
                                       pr os "\"/>")

  fun outTransId   (os, NONE)             = ()
    | outTransId   (os, SOME (TransId i)) = let val pr = pr os
        in pr " id=\"id"; pr (iToCStr i); pr "\"" end

  fun outAttInt (os, n, v) = let val pr = pr os
        in pr " "; pr n; pr "=\""; pr (iToCStr v); pr "\"" end

  fun outPosAtt os NONE          = ()
    | outPosAtt os (SOME (x, y)) = (outAttInt (os, "x", x);
                                    outAttInt (os, "y", y))

  fun outStringAtt os (_, NONE)     = ()
    | outStringAtt os (att, SOME s) = let val pr = pr os
        in pr " "; pr att; pr "=\""; pr s; pr "\"" end

  fun outOptional os (tagstr, present, prcontents, value) =
      if present value
      then (prOpenTag os tagstr;
              pr os toData; prcontents (os, value); pr os toXML;
            prCloseTag os tagstr)
      else ()

  fun outLabel os (kind, pos, prData) = let
      val pr = pr os
    in
      pr "<label kind=\""; pr kind; pr "\""; outPosAtt os pos; pr ">";
      pr toData; prData (); pr toXML; pr "</label>"
    end

  fun outFlag os (_, false)   = ()
    | outFlag os (name, true) = let val pr = pr os
        in pr "<"; pr name; pr "/>" end

  fun outPosLabel os (name, pos, NONE) = ()
    | outPosLabel os (name, pos, SOME data) = let
      val pr = pr os

    in pr "<"; pr name; outPosAtt os pos; pr ">";
         pr toData; pr data; pr toXML;
       pr "</"; pr name; pr ">"
    end
    
  fun outputLocation os (Location {id, position, color,
                                   name=(name, namePos),
                                   invariant=(invariant, invariantPos),
                                   comments=(comments, commentsPos),
                                   urgent, committed}) =
    let
      val pr = pr os
      val outLabel = outLabel os
      val outPosLabel = outPosLabel os
      val outFlag = outFlag os
    in
      pr "<location";
        outLocId (os, "id", SOME id);
        outPosAtt os position;
        outStringAtt os ("color", color);
      pr ">";
      outPosLabel ("name", namePos, name);

      if T.hasInvariant invariant
      then outLabel ("invariant", invariantPos,
                     fn()=> T.outputInvariant (os, invariant)) else ();

      case comments of
        NONE   => ()
      | SOME c => outLabel ("comments", commentsPos, fn()=> pr c);

      outFlag ("urgent", urgent);
      outFlag ("committed", committed);

      pr "</location>"
    end

  fun outputTransition os (Transition {id, source, target,
                                       select = (select, selectPos),
                                       guard  = (guard , guardPos),
                                       sync   = (sync  , syncPos),
                                       update = (update, updatePos),
                                       comments = (comments, commentsPos),
                                       position, color, nails}) =
    let
      val pr = pr os

      fun prNail pos = (pr "<nail"; outPosAtt os (SOME pos); pr "/>")

      val outLabel = outLabel os
      val outPosLabel = outPosLabel os
    in
      pr "<transition";
        outTransId (os, id);
        outPosAtt os position;
        outStringAtt os ("color", color);
      pr ">";

      outLocRef os ("source", source);
      outLocRef os ("target", target);

      if T.hasSelect select
      then outLabel ("select", selectPos,
                     fn()=> T.outputSelect (os, select)) else ();

      if T.hasGuard guard
      then outLabel ("guard", guardPos,
                     fn()=> T.outputGuard (os, guard)) else ();

      if T.hasSync sync
      then outLabel ("synchronisation", syncPos,
                     fn()=> T.outputSync (os, sync)) else();

      if T.hasUpdate update
      then outLabel ("assignment", updatePos,
                     fn()=> T.outputUpdate (os, update)) else ();

      case comments of
        NONE   => ()
      | SOME c => outLabel ("comments", commentsPos, fn()=> pr c);

      app prNail nails;
      pr "</transition>"
    end

  fun outputTemplate os (Template {name=(name, namePos),
                                   parameter=(param, paramPos),
                                   declaration, initial,
                                   locations, transitions, ...}) =
    let
      val pr = pr os
      val optpr = outOptional os
    in
      pr "<template>";
      outPosLabel os ("name", namePos, SOME name);

      if T.hasParameter param
      then (pr "<"; pr "parameter"; outPosAtt os paramPos; pr ">";
              pr toData; T.outputParameter (os, param); pr toXML;
            pr "</parameter>")
      else ();

      optpr ("declaration", T.hasDeclaration,
             T.outputTemplateDeclaration, declaration);
      app (outputLocation os) locations;

      case initial
      of NONE   => ()
       | SOME i => outLocRef os ("init", i);

      app (outputTransition os) transitions;

      pr "</template>"
    end

  fun output os (Nta {imports, declaration, templates,
                      instantiation=inst, system, ... }) =
    let
      val os = XMLWriter.mkOutstream os
      val pr = pr os

      fun optpr arg = outOptional os arg
    in
      pr "<?xml version='1.0' encoding='utf-8'?>";
      pr "<!DOCTYPE nta PUBLIC '"; pr dtdName; pr "' '"; pr dtdURL; pr "'>";
      pr "<nta>";
      optpr ("imports", T.hasImports, T.outputImports, imports);
      optpr ("declaration", T.hasDeclaration, T.outputDeclaration,declaration);
      app (outputTemplate os) templates;
      optpr ("instantiation", T.hasInstantiation, T.outputInstantiation, inst);
      pr "<system>";
        pr toData; T.outputSystem (os, system); pr toXML;
      pr "</system>";
      pr "</nta>";
      S.flushOut os
    end

end

